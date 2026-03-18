library(readxl) # read excel files (UI)
library(tidyverse) # dataframe manipulations
library(purrr) # nested list indexing
require(scales) # graphing colours
require(foreach) # parallel sensitivity analysis
require(doParallel) # parallel sensitivity analysis
require(gridExtra)

devtools::document()

################################################################################
############################# Load input data from UI ##########################
################################################################################

path <- "Templates/PEATREST_input_sensitivity_analysis.xlsx" # select user input spreadsheet

input.dat <- getData(path)
if (0) {
  input.dat <- input.dat[-5] # drop area 2
} else {
  input.dat <- input.dat[-4] # drop area 2
  names(input.dat)[4] <- "Area.1"
}

growthYield <- getGrowthYieldData()

## YC not passed by user, these are estimated from height/age data
if (any(sapply(map(input.dat[grep("Area", names(input.dat))], .f = "YC"), FUN = is.null))) {
  YC <- getYC(input.dat,
              growthYield.dat)
  for (i in 1:length(YC)) {
    input.dat[[names(YC)[i]]]$YC <- YC[[i]]
  }
}

################################################################################
########################## Get list of controlled pars #########################
################################################################################

get_all_names <- function(x) {
  if (!is.list(x)) {
    return(NULL)  # Non-list elements have no names
  }

  # Get current level names (if any)
  current_names <- names(x)

  # Recursively get names from sublists
  nested_names <- unlist(lapply(x, get_all_names), use.names = FALSE)

  # Combine and remove duplicates
  unique(c(current_names, nested_names))
}

find_index_paths <- function(lst, target_name, current_path = integer()) {
  paths <- list()

  # Ensure input is a list
  if (!is.list(lst)) {
    return(paths)
  }

  for (i in seq_along(lst)) {
    name_i <- names(lst)[i]

    # If the current element's name matches the target
    if (!is.null(name_i) && name_i == target_name) {
      paths <- append(paths, list(c(current_path, i)))
    }

    # If the element is itself a list, search recursively
    if (is.list(lst[[i]])) {
      sub_paths <- find_index_paths(lst[[i]], target_name, c(current_path, i))
      if (length(sub_paths) > 0) {
        paths <- append(paths, sub_paths)
      }
    }
  }

  return(paths)
}

input_vars <- get_all_names(input.dat)
input_vars_all <- input_vars
input_vars <- input_vars[-c(1:4)] # remove higher level names (Windfarm, Emissions, Aq.Carbon, Area.1)

if (1) { # remove variables only relevant to windfarm LCA. This will likely be removed early in final version
  input_vars <- input_vars[!input_vars %in% c("YC",
                                              "A_windfarm",
                                              "H_turb",
                                              "Vwind_site",
                                              "t_down",
                                              "n_turb",
                                              "pow_curve",
                                              "slope_pow_curve",
                                              "int_pow_curve",
                                              "soil_type",
                                              "species",
                                              "rotation",
                                              "timber_removed",
                                              "A_harv_turb",
                                              "D_width",
                                              "t_replant",
                                              "t_seedling_replant",
                                              "A_replant_turb",
                                              "p_biofuel",
                                              "dam",
                                              "bund",
                                              "smooth",
                                              "turf_import",
                                              "turf_local",
                                              "fert")]
}

################################################################################
######################### Load decay and 3PG parameters ########################
################################################################################

parms_decay <- read_xlsx(path,
                         sheet = "Decay rate parms",
                         range = "A1:F10",
                         progress = F)

parms_3PG <- read_excel(path,
                        sheet = "3PG parms",
                        range = "A1:E24",
                        progress = F)

# Re-format
parms_3PG <- as.data.frame(parms_3PG %>%
                             filter(complete.cases(.)) %>%
                             select(Var_name, all_of(c("Scots_Pine", "Sitka_Spruce"))))

parms_fE <-  read_excel(path,
                        sheet = "fE YC coefficients",
                        range = "A1:H7",
                        progress = F)

parms_decay$delta <- 0.5
parms_decay <- as.data.frame(parms_decay)

input.dat$parms_decay <- parms_decay
input.dat$parms_3PG <- parms_3PG
input.dat$parms_fE <- parms_fE
input.dat$growthYield <- growthYield

if (1) {

  # Initialise parallel implementation
  numCores <- min(length(unique(input_vars)), detectCores() - 1)
  cl <- makeCluster(numCores)
  registerDoParallel(cl)

  # Path to the folder containing your R scripts
  script_dir <- "R/"

  # Get all R files in the directory
  r_files <- list.files(script_dir, pattern = "\\.R$", full.names = TRUE)

  # Export the list of files to all workers
  clusterExport(cl, varlist = c("r_files", "input.dat"))

  # Source the files on each worker
  clusterEvalQ(cl, {
    for (f in r_files) {
      tryCatch(
        source(f, local = TRUE),
        error = function(e) message("Error sourcing ", f, ": ", e$message)
      )
    }
  })

  # Run model in parallel
  sensitivity_resA <- foreach(i = seq(6, 14, 2),
                              .packages = c("readxl", "tidyverse", "purrr"),
                              .combine=rbind) %dopar% {
    res_j <- c()

    for (j in 1:100) {

      set.seed(j*i)

      input_modifier <- rnorm(length(input_vars), 1, 0.25)

      input.dat_j <- input.dat

      input.dat_j$Area.1$YC[1] <- i

      for (k in 1:length(input_modifier)) {
        var_ind <- find_index_paths(input.dat, input_vars[k])
        input.dat_j[[var_ind[[1]][1]]][[var_ind[[1]][2]]][1] <- input_modifier[k]*input.dat_j[[var_ind[[1]][1]]][[var_ind[[1]][2]]][1]
      }

      ################################################################################
      ##################### CO2 sequestration loss from Forestry #####################
      ################################################################################

      S_forest <- ForestSequestrationMod(input.dat_j)

      ################################################################################
      ###################### CO2 loss from soils under Forestry ######################
      ################################################################################

      skipIteration <- F
      tryCatch(L_forest_soils <- ForestSoilsEmissionsMod(input.dat_j),
               error = function(e) skipIteration <<- T)
      if (skipIteration) next


      ################################################################################
      ################ Aquatic carbon loss from soils under Forestry #################
      ################################################################################

      L_AqC_forest_soils <- AquaticCarbonMod(input.dat_j,
                                             L_forest_soils,
                                             forest_soils = T)

      ################################################################################
      ############ Harvesting/Restoration emissions and wood product decay ###########
      ################################################################################

      L_forest <- HarvestingManagementMod(input.dat_j,
                                          S_forest)

      ################################################################################
      ############################### Loss of Soil CO2 ###############################
      ################################################################################

      L_peatland <- PeatlandSoilsEmissionsMod(input.dat_j)

      ################################################################################
      ###################### Aquatic carbon loss from peatland #######################
      ################################################################################

      L_AqC_peatland <- AquaticCarbonMod(input.dat_j,
                                         L_peatland,
                                         forest_soils = F)

      ################################################################################
      ######################### CO2 payback time estimation ##########################
      ################################################################################

      res <- getCarbonDf(S_forest,
                         L_forest_soils,
                         L_AqC_forest_soils,
                         L_forest,
                         L_peatland,
                         L_AqC_peatland)

      t_payback_res <- CarbonMitigationMod(res, sum_areas = F)

      if (0) {
        # p_L_forest <- plotL_forest(res %>% filter(Est == "Exp"), plabs = paste0("YC", input.dat$Area.1$YC[c(2,1,3)]))
        # p_L_forest[[1]]

        # plotL_AqC_peatland(res %>% filter(Est == "Exp"))
        # grid.arrange(p_L_forest[[2]],
        plotLCA(res %>% filter(Est == "Exp"),
                   t_payback_res %>% filter(Est == "Exp"),
                   sum_areas = F,
                   plabs = paste0("YC", input.dat_j$Area.1$YC[c(2,1,3)]))#)
      }

      res_j <- rbind(res_j,
                     data.frame(YC = i,
                                rep = j,
                                t_flux = (t_payback_res %>%
                                            filter(Est == "Exp", metric == "t_flux"))$t,
                                t_payback = (t_payback_res %>%
                                               filter(Est == "Exp", metric == "t_payback"))$t,
                                t_rest = input.dat_j$Area.1$t_restore_peatland[1],
                                t_fallow = input.dat_j$Area.1$t_fallow[1]))

    }
    rownames(res_j) <- NULL
    res_j
  }

  # Stop the cluster
  stopCluster(cl)

  write.csv(sensitivity_resA, "../Data/sensitivity_res_relative_mitigation_Area2.csv", row.names = F)

} else {

  sensitivity_res1 <- read.csv("../Data/sensitivity_res_relative_mitigation.csv")
  sensitivity_res2 <- read.csv("../Data/sensitivity_res_relative_mitigation_Area2.csv")

  sensitivity_res1$Area <- "Area.1"
  sensitivity_res2$Area <- "Area.2"

  sensitivity_res <- rbind(sensitivity_res1, sensitivity_res2)
}

sensitivity_res %>%
  mutate(t_flux = t_flux / (t_rest + t_fallow),
         t_payback = t_payback / (t_rest + t_fallow)) %>%
  select(YC, Area, t_flux, t_payback) %>%
  pivot_longer(cols = c(t_flux, t_payback), names_to = "metric", values_to = "value") %>%
  ggplot(aes(x=YC, y=value)) +
  geom_boxplot(aes(group = factor(YC))) +
  geom_smooth(method="lm", se=F) +
  facet_grid(Area ~ metric) +
  labs(y = "Relative mitigation time") +
  theme_bw()

sensitivity_res %>%
  mutate(t_flux = t_flux / (t_rest + t_fallow),
         t_payback = t_payback / (t_rest + t_fallow)) %>%
  select(YC, Area, t_flux, t_payback) %>%
  pivot_longer(cols = c(t_flux, t_payback), names_to = "metric", values_to = "value") %>%
  ggplot(aes(x=YC, y=value)) +
  geom_boxplot(aes(group = factor(YC))) +
  geom_smooth(method="lm", se=F) +
  facet_grid(. ~ metric) +
  labs(y = "Relative mitigation time") +
  theme_bw()
