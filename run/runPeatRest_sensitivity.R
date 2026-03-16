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
input.dat <- input.dat[1:(length(input.dat)-1)] # drop area 2

growthYield.dat <- getGrowthYieldData()

## YC not passed by user, these are estimated from height/age data
if (any(sapply(map(input.dat[grep("Area", names(input.dat))], .f = "YC"), FUN = is.null))) {
  YC <- getYC(input.dat,
              growthYield.dat)
  for (i in 1:length(YC)) {
    input.dat[[names(YC)[i]]]$YC <- YC[[i]]
  }
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
  input_vars <- input_vars[!input_vars %in% c("A_windfarm",
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

# input.dat$Area.1$YC[1] <- 10

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
  sensitivity_resA <- foreach(i = input_vars,
                              .packages = c("readxl", "tidyverse", "purrr"),
                              .combine=rbind) %dopar% {

                                # input.dat_unchange <- input.dat
                                res_j <- c()

                                for (j in seq(0.2,2,by=0.2)) {

                                  # Find focal input var in nested list and modify (Exp value only)
                                  var_ind <- find_index_paths(input.dat, i)

                                  input.dat_j <- input.dat
                                  input.dat_j[[var_ind[[1]][1]]][[var_ind[[1]][2]]][1] <- j*input.dat_j[[var_ind[[1]][1]]][[var_ind[[1]][2]]][1]

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
                                  ########################## Emissions rates from soils ##########################
                                  ################################################################################

                                  R_tot <- Emissions_rates_soils_RM(input.dat_j)

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
                                    plotLCA_cs(res %>% filter(Est == "Exp"),
                                               t_payback_res %>% filter(Est == "Exp"),
                                               sum_areas = F,
                                               plabs = paste0("YC", input.dat_j$Area.1$YC[c(2,1,3)]))#)
                                  }

                                  res_j <- rbind(res_j,
                                                 data.frame(var = i,
                                                            sca = j,
                                                            val = input.dat_j[[var_ind[[1]][1]]][[var_ind[[1]][2]]][1],
                                                            t_flux = (t_payback_res %>%
                                                                        filter(Est == "Exp", metric == "t_flux"))$t,
                                                            t_payback = (t_payback_res %>%
                                                                           filter(Est == "Exp", metric == "t_payback"))$t))

                                }
                                rownames(res_j) <- NULL
                                res_j
                              }

  # Stop the cluster
  stopCluster(cl)

  ## Repeat for decay parameters

  # Initialise parallel implementation
  numCores <- min(2*nrow(input.dat$parms_decay), detectCores() - 1)
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

  sensitivity_resB <- foreach(i = 1:(2*nrow(input.dat$parms_decay)),
                              .packages = c("readxl", "tidyverse", "purrr"),
                              .combine=rbind) %dopar% {

                                res_j <- c()

                                if (i >= (nrow(input.dat$parms_decay)+1)) {
                                  k <- i - nrow(input.dat$parms_decay)
                                  l <- 6
                                } else {
                                  k <- i
                                  l <- 4
                                }

                                var_name <- input.dat$parms_decay$Var[k]
                                if (i > nrow(input.dat$parms_decay)) {
                                  var_name <- stringr::str_replace(var_name, "alpha", "delta")
                                }

                                alpha_df <- input.dat$parms_decay

                                for (j in seq(0.2,2,by=0.2)) {

                                  alpha_df_j <- alpha_df

                                  alpha_df_j[k,l] <- alpha_df_j[k,l] * j

                                  input.dat$parms_decay <- alpha_df_j

                                  ################################################################################
                                  ##################### CO2 sequestration loss from Forestry #####################
                                  ################################################################################

                                  S_forest <- ForestSequestrationMod(input.dat)

                                  ################################################################################
                                  ###################### CO2 loss from soils under Forestry ######################
                                  ################################################################################

                                  L_forest_soils <- ForestSoilsEmissionsMod(input.dat)

                                  ################################################################################
                                  ################ Aquatic carbon loss from soils under Forestry #################
                                  ################################################################################

                                  L_AqC_forest_soils <- AquaticCarbonMod(input.dat,
                                                                         L_forest_soils,
                                                                         forest_soils = T)

                                  ################################################################################
                                  ############ Harvesting/Restoration emissions and wood product decay ###########
                                  ################################################################################

                                  L_forest <- HarvestingManagementMod(input.dat,
                                                                      S_forest)

                                  ################################################################################
                                  ########################## Emissions rates from soils ##########################
                                  ################################################################################

                                  R_tot <- Emissions_rates_soils_RM(input.dat)

                                  ################################################################################
                                  ############################### Loss of Soil CO2 ###############################
                                  ################################################################################

                                  L_peatland <- PeatlandSoilsEmissionsMod(input.dat)

                                  ################################################################################
                                  ###################### Aquatic carbon loss from peatland #######################
                                  ################################################################################

                                  L_AqC_peatland <- AquaticCarbonMod(input.dat,
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
                                    p_L_forest <- plotL_forest(res %>% filter(Est == "Exp"))
                                    # p_L_forest[[1]]
                                    # p_L_forest[[2]]
                                    pLCA <- plotLCA(res %>% filter(Est == "Exp"), t_payback_res %>% filter(Est == "Exp"), sum_areas = F)
                                    pLCA_cs <- plotLCA_cs(res %>% filter(Est == "Exp"), t_payback_res %>% filter(Est == "Exp"), sum_areas = F)
                                    # pLCA
                                    # pLCA_cs
                                    grid.arrange(p_L_forest[[1]], pLCA_cs)
                                  }

                                  res_j <- rbind(res_j,
                                                 data.frame(var = var_name,
                                                            sca = j,
                                                            val = input.dat$parms_decay[k,l],
                                                            t_flux = (t_payback_res %>%
                                                                        filter(Est == "Exp", metric == "t_flux"))$t,
                                                            t_payback = (t_payback_res %>%
                                                                           filter(Est == "Exp", metric == "t_payback"))$t))

                                  input.dat$parms_decay <- alpha_df

                                }
                                rownames(res_j) <- NULL
                                res_j
                              }
  # Stop the cluster
  stopCluster(cl)

  sensitivity_res <- rbind(sensitivity_resA, sensitivity_resB)
  write.csv(sensitivity_res, "../Data/sensitivity_res_YC10.csv", row.names=F)
} else {
  sensitivity_res <- read.csv("../Data/sensitivity_res_YC8.csv")
}

sensitivity_res <- sensitivity_res %>%
  group_by(var) %>%
  mutate(t_flux_norm = (t_flux / t_flux[sca==1.0]),
         t_payback_norm = (t_payback / t_payback[sca==1.0]))

sensitivity_res <- sensitivity_res %>%
  filter(!var %in% c("d_biofuel",
                     "d_wpF",
                     "d_wpM",
                     "d_wpS",
                     "E_fert",
                     "E_mulch",
                     "E_turf_import",
                     "E_turf_local",
                     "p_biofuel"))

vars0 <- unique(sensitivity_res$var)
vars1 <- vars0
vars1[1] <- "E_grid"
vars1[9] <- "P_AqC-CO2"
vars1[10] <- "d_rmax"
vars1[11] <- "rho_rsoil"
vars1[12] <- "d_p"
vars1[14] <- "d_d"
vars1[15] <- "epsilon_P"
vars1[18] <- "epsilon_B"
vars1[19] <- "rho_C:B"
vars1[21] <- "t_rest"
vars1[22] <- "n_rest"
vars1[23] <- "W_drain"
vars1[24] <- "W_rest"
vars1 <- stringr::str_replace(vars1, "\\_", "\\[")
vars1[grep("\\[", vars1)] <- paste0(vars1[grep("\\[", vars1)],"]")
data.frame(vars0,vars1)

axis_labels <- parse(text = vars1)
names(axis_labels) <- vars0

axis_labels <- sapply(seq_along(vars1), FUN = function(x) {
  res <- vars1[x]
  names(res) <- vars0[x]
  return(res)
})

sensitivity_res_lin <- sensitivity_res %>%
  group_by(var) %>%
  summarise(sns_lin_flux = coef(lm(t_flux_norm ~ sca))[2],
            sns_lin_payback = coef(lm(t_payback_norm ~ sca))[2],
            sns_lin_flux_R = summary(lm(t_flux_norm ~ sca))$r.squared,
            sns_lin_payback_R = summary(lm(t_payback_norm ~ sca))$r.squared)

sensitivity_res_lin <- left_join(sensitivity_res_lin, data.frame(var=vars0, var1 = vars1), by="var")

sensitivity_res_lin_flux <- sensitivity_res_lin %>%
  arrange(desc(sns_lin_flux)) %>%
  mutate(var = factor(var, levels=unique(var))) %>%
  filter(sns_lin_flux != 0)

sensitivity_res_lin_payback <- sensitivity_res_lin %>%
  arrange(desc(sns_lin_payback)) %>%
  mutate(var = factor(var, levels=unique(var))) %>%
  filter(sns_lin_payback != 0)

p1 <- ggplot(sensitivity_res_lin_flux, aes(x=var, y=sns_lin_flux)) +
  geom_bar(stat="identity") +
  scale_x_discrete(labels = parse(text = sensitivity_res_lin_flux$var1)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4, hjust = 1)) +
  labs(x="", y=expression(paste(d,t[flux],"/",d,Iota))) +
  scale_y_continuous(limits=c(min(c(sensitivity_res_lin_flux$sns_lin_flux,sensitivity_res_lin_payback$sns_lin_payback)),max(c(sensitivity_res_lin_flux$sns_lin_flux,sensitivity_res_lin_payback$sns_lin_payback))))

p2 <- ggplot(sensitivity_res_lin_payback, aes(x=var, y=sns_lin_payback)) +
  geom_bar(stat="identity") +
  scale_x_discrete(labels = parse(text = sensitivity_res_lin_payback$var1)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4, hjust = 1)) +
  labs(x="", y=expression(paste(d,t[payback],"/",d,Iota))) +
  scale_y_continuous(limits=c(min(c(sensitivity_res_lin_flux$sns_lin_flux,sensitivity_res_lin_payback$sns_lin_payback)),max(c(sensitivity_res_lin_flux$sns_lin_flux,sensitivity_res_lin_payback$sns_lin_payback))))

png("../Figures/LCA implementation/V7/sensitivity_slopes_YC8.png", width=16, height=16, units="cm", res=300)
grid.arrange(p1,p2)
dev.off()

sc <- 1
png("../Figures/LCA implementation/V7/sensitivity_t_flux_YC8.png", width=sc*24, height=sc*28, units="cm", res=300)
ggplot(sensitivity_res %>%
         filter(var %in% sensitivity_res_lin_flux$var) %>%
         mutate(var = factor(var, levels = sensitivity_res_lin_flux$var)),
       aes(x=val, y=t_flux_norm)) +
  geom_line() +
  facet_wrap(var ~ .,
             scales="free",
             labeller = as_labeller(axis_labels[as.character(sensitivity_res_lin_flux$var)], default = label_parsed),
             ncol = 4) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 20),  # X-axis label font size
        axis.title.y = element_text(size = 20),
        axis.text.x  = element_text(size = 12),  # X-axis tick labels font size
        axis.text.y  = element_text(size = 12),
        strip.text = element_text(size = 16)) +
  labs(x="Value", y=expression(paste(t[flux1], "/", t[flux0]))) +
  geom_hline(yintercept = 1, col="grey", linetype=2)
dev.off()

png("../Figures/LCA implementation/V7/sensitivity_t_payback_YC8.png", width=sc*1.05*24, height=sc*(7/5)*28, units="cm", res=300)
ggplot(sensitivity_res %>%
         filter(var %in% sensitivity_res_lin_payback$var) %>%
         mutate(var = factor(var, levels = sensitivity_res_lin_payback$var)),
       aes(x=val, y=t_payback_norm)) +
  geom_line() +
  facet_wrap(var ~ .,
             scales="free",
             labeller = as_labeller(axis_labels[as.character(sensitivity_res_lin_payback$var)], default = label_parsed),
             ncol = 4) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 20),  # X-axis label font size
        axis.title.y = element_text(size = 20),
        axis.text.x  = element_text(size = 12),  # X-axis tick labels font size
        axis.text.y  = element_text(size = 12),
        strip.text = element_text(size = 16)) +
  labs(x="Value", y=expression(paste(t[payback1], "/", t[payback0]))) +
  geom_hline(yintercept = 1, col="grey", linetype=2)
dev.off()

