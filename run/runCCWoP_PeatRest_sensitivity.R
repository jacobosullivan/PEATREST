library(readxl) # read excel files (UI)
library(tidyverse) # dataframe manipulations
library(purrr) # nested list indexing
require(scales) # graphing colours
devtools::document()

################################################################################
############################# Load input data from UI ##########################
################################################################################

path <- "Templates/Full carbon calculator for windfarms on peatlands - Version 2.14.1.xlsx" # select user input spreadsheet

dat <- getData(path)

## I need tests to ensure all data has been passed. If not, return error messages
core.dat <- dat$core.dat
forestry.dat <- dat$forestry.dat
forestry.dat <- forestry.dat[1:(length(forestry.dat)-1)] # drop area 2
construct.dat <- dat$construct.dat
rm(dat)

growthYield.dat <- getGrowthYieldData()

## YC not passed by user, these are estimated from height/age data
if (any(sapply(map(forestry.dat[grep("Area", names(forestry.dat))], .f = "YC"), FUN = is.null))) {
  YC <- getYC(forestry.dat,
              growthYield.dat)
  for (i in 1:length(YC)) {
    forestry.dat[[names(YC)[i]]]$YC <- YC[[i]]
  }
}

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

input_vars <- get_all_names(forestry.dat)
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

if (0) {
  forestry.dat_unchange <- forestry.dat

  sensitivity_res <- c()

  for (i in input_vars) {
    for (j in seq(0.2,2,by=0.2)) { # V1
    # for (j in 2*10^seq(-1,2,length.out=10)) { # V2 logarithmic DOESN'T WORK, DRIVE SUB-MODELS INTO IMPOSSIBLE PARAMETER SPACE

      # Find focal input var in nested list and modify (Exp value only)
      var_ind <- find_index_paths(forestry.dat, i)
      forestry.dat[[var_ind[[1]][1]]][[var_ind[[1]][2]]][1] <- j*forestry.dat[[var_ind[[1]][1]]][[var_ind[[1]][2]]][1]

      ################################################################################
      ##################### CO2 sequestration loss from Forestry #####################
      ################################################################################

      S_forest <- C_sequest_in_trees_RM(forestry.dat)

      ################################################################################
      ###################### CO2 loss from soils under Forestry ######################
      ################################################################################

      L_forest_soils <- Emissions_rates_forestry_soils_RM(core.dat,
                                                          forestry.dat,
                                                          growthYield.dat)

      ################################################################################
      ################ Aquatic carbon loss from soils under Forestry #################
      ################################################################################

      L_AqC_forest_soils <- CO2_loss_DOC_POC_RM(forestry.dat,
                                                L_forest_soils,
                                                forest_soils = T)

      ################################################################################
      ############ Harvesting/Restoration emissions and wood product decay ###########
      ################################################################################

      L_forest <- Forestry_CO2_loss_detail_RM(core.dat,
                                              forestry.dat,
                                              growthYield.dat,
                                              S_forest)

      ################################################################################
      ########################## Emissions rates from soils ##########################
      ################################################################################

      R_tot <- Emissions_rates_soils_RM(core.dat = core.dat,
                                        forestry.dat = forestry.dat)

      ################################################################################
      ############################### Loss of Soil CO2 ###############################
      ################################################################################

      L_peatland <- CO2_loss_restoration(core.dat = core.dat,
                                         R_tot = R_tot)

      ################################################################################
      ###################### Aquatic carbon loss from peatland #######################
      ################################################################################

      L_AqC_peatland <- CO2_loss_DOC_POC_RM(forestry.dat,
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

      t_payback_res <- Carbon_payback_time(res, sum_areas = F)

      sensitivity_res <- rbind(sensitivity_res,
                               data.frame(var = i,
                                          sca = j,
                                          val = forestry.dat[[var_ind[[1]][1]]][[var_ind[[1]][2]]][1],
                                          t_flux = (t_payback_res %>%
                                                      filter(Est == "Exp", metric == "t_flux"))$t,
                                          t_payback = (t_payback_res %>%
                                                         filter(Est == "Exp", metric == "t_payback"))$t))

      # reset input vars
      forestry.dat <- forestry.dat_unchange
    }
  }

  write.csv(sensitivity_res, "../Data/sensitivity_res.csv", row.names=F)
} else {
  sensitivity_res <- read.csv("../Data/sensitivity_res.csv")
}

sensitivity_res <- sensitivity_res %>%
  group_by(var) %>%
  mutate(t_flux_norm = (t_flux / t_flux[sca==1.0]) - 1,
         t_payback_norm = (t_payback / t_payback[sca==1.0]) - 1)

sensitivity_res <- sensitivity_res %>%
  filter(!var %in% c("d_biofuel",
                     "d_wpF",
                     "d_wpM",
                     "d_wpS",
                     "dist_biofuel_plant",
                     "E_fert",
                     "E_mulch",
                     "E_turf_import",
                     "E_turf_local",
                     "p_biofuel"))

vars0 <- unique(sensitivity_res$var)
vars1 <- vars0
vars1[11] <- "epsilon_P"
vars1[12] <- "t_0"
vars1[14] <- "epsilon_B"
vars1[15] <- "rho_C:B"
vars1[17] <- "t_rest"
vars1[18] <- "n_rest"
vars1[19] <- "W_drain"
vars1[20] <- "W_rest"

vars1 <- stringr::str_replace(vars1, "\\_", "\\[")
vars1[grep("\\[", vars1)] <- paste0(vars1[grep("\\[", vars1)],"]")
axis_labels <- parse(text = vars1)
names(axis_labels) <- vars0

axis_labels <- as.list(sapply(seq_along(vars1), FUN = function(x) {
  res <- vars1[x]
  names(res) <- vars0[x]
  return(res)
}))

png("../Figures/sensitivity_t_flux.png", width=25, height=16, units="cm", res=300)
# ggplot(sensitivity_res, aes(x=val, y=100*t_flux_norm)) +
ggplot(sensitivity_res, aes(x=sca, y=100*t_flux_norm)) +
  geom_line() +
  facet_wrap(var ~ ., scales="free", labeller = as_labeller(axis_labels, default = label_parsed)) +
  theme_bw() +
  # labs(x="Value", y=expression(paste(Delta, t[flux], " (%)")))
  labs(x="Scaling parameter", y=expression(paste(Delta, t[flux], " (%)")))
dev.off()

png("../Figures/sensitivity_t_payback.png", width=25, height=16, units="cm", res=300)
# ggplot(sensitivity_res, aes(x=val, y=100*t_payback_norm)) +
ggplot(sensitivity_res, aes(x=sca, y=100*t_payback_norm)) +
  geom_line() +
  facet_wrap(var ~ ., scales="free") +
  theme_bw() +
  # labs(x="Value", y=expression(paste(Delta, t[payback], " (%)")))
  labs(x="Scaling parameter", y=expression(paste(Delta, t[payback], " (%)")))
dev.off()
