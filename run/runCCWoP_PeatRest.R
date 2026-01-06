library(readxl) # read excel files (UI)
library(tidyverse) # dataframe manipulations
library(purrr) # nested list indexing
devtools::document()

################################################################################
############################# Load input data from UI ##########################
################################################################################

path <- "Templates/Full carbon calculator for windfarms on peatlands - Version 2.14.1.xlsx" # select user input spreadsheet

dat <- getData(path)

## I need tests to ensure all data has been passed. If not, return error messages
core.dat <- dat$core.dat
forestry.dat <- dat$forestry.dat
construct.dat <- dat$construct.dat
rm(dat)

growthYield.dat <- getGrowthYieldData()

if (0) {
  ## Exploratory plots of GY table show issue with biomass of wood products

  ggplot(growthYield.dat %>% filter(Spp=="Scots_pine"),
         aes(x=Age, y=H, col=factor(YC))) +
    geom_line()


  pivot_longer(growthYield.dat %>%
                 select(Spp, YC, Age, rho_wpF, rho_wpM, rho_wpS),
               cols = starts_with("rho_"),
               names_prefix = "rho_",
               names_to = "wp",
               values_to = "rho") %>%
    filter(Spp=="Sitka_spruce") %>%
  ggplot(aes(x=Age, y=rho, linetype=wp, col=factor(YC))) +
    geom_line()

  ggplot(growthYield.dat %>%
           filter(Spp=="Sitka_spruce", YC==6), aes(x=Age, y=(B_wpF)/(B_s+B_c+B_r))) +
    geom_line()

  ggplot(growthYield.dat %>%
           filter(Spp=="Sitka_spruce", YC==6), aes(x=Age, y=(B_wpM)/(B_s+B_c+B_r))) +
    geom_line()

  ggplot(growthYield.dat %>%
           filter(Spp=="Sitka_spruce", YC==6), aes(x=Age, y=(B_wpS)/(B_s+B_c+B_r))) +
    geom_line()

  ggplot(growthYield.dat %>%
           filter(Spp=="Sitka_spruce", YC==6), aes(x=Age, y=(B_wpF+B_wpM+B_wpS)/(B_s+B_c+B_r))) +
    geom_line()

}

#################### NEW INPUT VARIABLES. NEED TO ADD THESE TO UI ##############
### These have been added to the UI already
# core.dat$Peatland.restoration$t_restore_microbes <- c(Exp = 10, Min = 8, Max = 12) # time to restoration of microbial function [yr]
# core.dat$Peatland.restoration$n_restore_microbes <- c(Exp = 2, Min = 2, Max = 2) # shape parameter for restoration of microbial function: 1 gives concave downward function (rapid increase in function)
# core.dat$Peatland.restoration$t_fallow <- c(Exp = 5, Min = 3, Max = 7) # time between harvesting and restoration [yr]
# core.dat$Peatland.restoration$n_restore <- c(Exp = 4, Min = 4, Max = 4) # shape parameter for restoration of bog plant function: 1 gives concave downward function (rapid increase in function)
#################### NEW INPUT VARIABLES. NEED TO ADD THESE TO UI ##############

## If YC not passed by user compute here and store in variable forestry.dat$Area.X$YC

################################################################################
##################### CO2 sequestration loss from Forestry #####################
################################################################################

S_forest <- C_sequest_in_trees_RM(core.dat,
                                  forestry.dat)

# This needs to be multiplied by 30.66 to get from units of C to CO2 for carbon payback!
# Leave in units C for now for the wood product modelling

if (0) {
  S_forest_df <- bind_rows(lapply(seq_along(S_forest), FUN = function(x) {
    res <- lapply(seq_along(S_forest[[x]]), FUN = function(y) {
      df <- as.data.frame(unname(S_forest[[x]][y])) %>%
        select(t, NPP, NPP_pa)
      df$Area <- names(S_forest)[x]
      df$Est <- names(S_forest[[x]])[y]
      return(df)
    })
    return(res)
  }))

  ggplot(S_forest_df, aes(x=t, y=NPP_pa, linetype=factor(Area))) +
    geom_line(col="green3") +
    scale_x_continuous(limits = c(min(S_forest_df$t), 200)) +
    facet_grid(Est ~ ., scales="free_y") +
    theme_bw() +
    labs(x="Time since harvesting [y]", y="Forestry sequestration [tCO2 ha-1]", col="n", linetype="")

  ggplot(S_forest_df, aes(x=t, y=NPP, linetype=factor(Area))) +
    geom_line(col="green3") +
    scale_x_continuous(limits = c(min(S_forest_df$t), 200)) +
    facet_grid(Est ~ ., scales="free_y") +
    theme_bw() +
    labs(x="Time since harvesting [y]", y="Forestry sequestration [tCO2]", col="n", linetype="")
}

################################################################################
############ Harvesting/Restoration emissions and wood product decay ###########
################################################################################

# JDebug: What is the appropriate volume for the harvesting? Above ground only?
L_forest <- Forestry_CO2_loss_detail_RM(core.dat,
                                        forestry.dat,
                                        growthYield.dat,
                                        S_forest)

if (0) {
  L_forest_df <- lapply(seq_along(L_forest), FUN = function(x) {
    res <- lapply(seq_along(L_forest[[x]]), FUN = function(y) {
      res2 <- lapply(seq_along(L_forest[[x]][[y]]), FUN = function(z) {
        df <- as.data.frame(unname(L_forest[[x]][[y]][z])) %>%
          pivot_longer(cols = starts_with(c("L_", "S_")),
                       names_to = "source")
        df$Area <- names(L_forest)[x]
        df$Est <- names(L_forest[[x]][[y]])[z]
        return(df)
      })
      return(res2)
    })
    return(res)
  })

  L_forest_df <- bind_rows(unlist(L_forest_df, recursive = F))

  L_forest_df$sink <- -1
  L_forest_df$sink[grep("S_", L_forest_df$source)] <- 1
  L_forest_df$discrete <- 0
  L_forest_df$discrete[L_forest_df$source %in% c("L_harv", "L_T_biofuel", "L_T_wpF", "L_T_wpM", "L_T_wpS", "S_biofuel")] <- 1
  L_forest_df <- L_forest_df %>%
    mutate(value = ifelse(value==0, NA, value))

  unique(L_forest_df$source)

  ## I don't know if this is working as I don't yet have correct values for wood product biomass allocation
  sources_cont <- c() #c("L_harv", "L_T_biofuel", "S_biofuel")
  sources_disc <- c("L_harv", "L_T_biofuel", "S_biofuel")
  ggplot(L_forest_df, aes(x=t, y=value*sink)) +
    geom_point(data = L_forest_df %>% filter(source %in% sources_disc), aes(shape=factor(source)), col="red3") +
    geom_line(data = L_forest_df %>% filter(source %in% sources_cont), aes(linetype=factor(source)), col="red3") +
    scale_x_continuous(limits = c(0, 200)) +
    facet_grid(Est ~ Area, scales="free_y") +
    theme_bw() +
    labs(x="Time since harvesting [y]", y="Wood product emissions [tCO2 ha-1]", linetype="", shape="")
}

################################################################################
########################### Bog plant sequestration ############################
################################################################################

S_bog_plants <- Bog_plant_sequestration_RM(core.dat,
                                           forestry.dat)

if (0) {
  n_restore <- map(forestry.dat[grep("Area", names(forestry.dat))], .f = "n_restore_plants")
  S_bog_plants_df <- bind_rows(lapply(seq_along(S_bog_plants), FUN = function(x) {
    res <- lapply(seq_along(S_bog_plants[[x]]), FUN = function(y) {
      df <- as.data.frame(S_bog_plants[[x]][y])
      colnames(df) <- "S"
      df$Area <- names(S_bog_plants)[x]
      df$n <- n_restore[[x]][y]
      df$t <- 0:(nrow(df)-1)
      df$Est <- names(S_bog_plants[[x]])[y]
      return(df)
    })
    return(res)
  }))

  ggplot(S_bog_plants_df, aes(x=t, y=S, col=factor(n))) +
    geom_line() +
    scale_x_continuous(limits = c(0, 200)) +
    facet_grid(Est ~ Area, scales="free_y") +
    theme_bw() +
    labs(x="Time since harvesting [y]", y="Bog plant sequestration [tCO2]", col="n", linetype="")
}

################################################################################
########################## Emissions rates from soils ##########################
################################################################################

R_tot <- Emissions_rates_soils_RM(core.dat = core.dat,
                                  construct.dat = construct.dat)

################################################################################
############################### Loss of Soil CO2 ###############################
################################################################################

# L_indirect <- CO2_loss_drained(core.dat = core.dat,
#                                AV_indirect = AV_indirect,
#                                R_tot = R_tot)
#
# L_direct <- CO2_loss_removed(core.dat = core.dat,
#                              AV_direct = AV_direct,
#                              L_indirect = L_indirect)
#
# L_soil <- CO2_loss_from_soil(L_direct = L_direct,
#                              L_indirect = L_indirect)

L_microbes <- CO2_loss_restoration(core.dat = core.dat,
                                   R_tot = R_tot)

if (0) {
  n_restore <- map(forestry.dat[grep("Area", names(forestry.dat))], .f = "n_restore_microbes")
  L_CO2_microbes_df <- bind_rows(lapply(seq_along(L_microbes$CO2), FUN = function(x) {
    res <- lapply(seq_along(L_microbes$CO2[[x]]), FUN = function(y) {
      df <- as.data.frame(L_microbes$CO2[[x]][y])
      colnames(df) <- "L"
      df$Area <- names(L_microbes$CO2)[x]
      df$n <- n_restore[[x]][y]
      df$t <- 0:(nrow(df)-1)
      df$Est <- names(L_microbes$CO2[[x]])[y]
      return(df)
    })
    return(res)
  }))

  L_CO2_microbes_df$C <- "CO2"

  L_CH4_microbes_df <- bind_rows(lapply(seq_along(L_microbes$CH4), FUN = function(x) {
    res <- lapply(seq_along(L_microbes$CH4[[x]]), FUN = function(y) {
      df <- as.data.frame(L_microbes$CH4[[x]][y])
      colnames(df) <- "L"
      df$Area <- names(L_microbes$CH4)[x]
      df$n <- n_restore[[x]][y]
      df$t <- 0:(nrow(df)-1)
      df$Est <- names(L_microbes$CH4[[x]])[y]
      return(df)
    })
    return(res)
  }))

  L_CH4_microbes_df$C <- "CH4"

  L_microbes_df <- rbind(L_CO2_microbes_df,
                         L_CH4_microbes_df)

  L_microbes_df$C <- factor(L_microbes_df$C, levels = c("CO2", "CH4"))
  ggplot(L_microbes_df, aes(x=t, y=L, col=factor(n), linetype=factor(C))) +
    geom_line() +
    scale_x_continuous(limits = c(0, 50)) +
    facet_grid(Est ~ Area, scales="free_y") +
    theme_bw() +
    labs(x="Time since harvesting [y]", y="Gaseous carbon emissions [tCO2 eq.]", col="n", linetype="")
}

################################################################################
######################### CO2 loss by DOC and POC loss #########################
################################################################################

L_DPOC <- CO2_loss_DOC_POC(core.dat = core.dat,
                           L_indirect = L_indirect,
                           L_improvement = L_improvement)



################################################################################
############################### Windspeed ratios ###############################
################################################################################

# Wrong with changes to inputs suggesting that conditionals may be failing
R_windspeed_all <- Wind_speed_ratios(core.dat = core.dat,
                                     forestry.dat = forestry.dat)

################################################################################
########################### Calculate capacity factor ##########################
################################################################################

if (core.dat$Windfarm$p_cap_in[1] == 2) { # capacity factor calculated from forestry module

  p_cap <- p_cap_windspeed(core.dat = core.dat,
                          forestry.dat = forestry.dat,
                          R_windspeed_all = R_windspeed_all)
  n_turb <- map(forestry.dat[grep("Area", names(forestry.dat))], .f = "n_turb")

} else { # user input capacity factor
  p_cap <- core.dat$Windfarm$p_cap
  n_turb <- core.dat$Windfarm$n_turb
}


################################################################################
#################### Calculate potential emissions savings #####################
################################################################################

## Total Windfarm energy output
e_out <- Windfarm_output(p_cap = p_cap,
                         n_turb = n_turb,
                         c_turb = core.dat$Windfarm$c_turb)

S_fuel <- Windfarm_emissions_saving(e_out = e_out,
                                    E_mat = E_mat)

################################################################################
######################## Payback time and CO2 emissions ########################
################################################################################

e_out_wf <- e_out * core.dat$Windfarm$t_wf

L_tot <- L_life + L_back + L_fix + L_soil + L_DPOC + L_forest

L_improvement_tot <- colSums(bind_rows(L_improvement$L_improvement))

L_tot_net <- L_tot - L_improvement_tot[c(1,3,2)] # re-order Min/Max

C_payback_time_tot <- matrix(rep(L_tot_net, each=3), 3, 3) / as.matrix(S_fuel[,-1])[,c(1,3,2)] # re-order Min/Max

C_payback_time_tot <- cbind(S_fuel$Fuel, data.frame(C_payback_time_tot))

r_CO2_to_pow <- (L_tot_net * 1000) / e_out_wf[c(1,3,2)] # re-order Min/Max

res <- cbind(source = c("L_life","L_back","L_fix","L_soil","L_DPOC","L_forest","L_improvement_tot"),
             bind_rows(L_life,
                       L_back,
                       L_fix,
                       L_soil,
                       L_DPOC,
                       L_forest,
                       L_improvement_tot))

res$source <- factor(res$source, levels = c("L_life","L_back","L_fix","L_soil","L_DPOC","L_forest","L_improvement_tot"))


PLOT_RES <- F # if FALSE, plot comparison between R version and Excel version

if (PLOT_RES) {

  ## Plotting
  ggplot(res, aes(x = source, y = Exp)) +
    geom_bar(stat = "identity") +
    geom_errorbar(aes(y = Exp, ymin = Min, ymax = Max), width = 0.2) +
    labs(y = "GH emissions (t CO2 eq.)") +
    theme_bw()

} else {
  ## Compare to spreadsheet

  L_ss <- read_excel(path,
                     sheet = "Payback Time and CO2 emissions",
                     range = "B14:E19",
                     col_names = c("source", "Exp","Min","Max"))
  L_ss$source <- c("L_life","L_back","L_fix","L_soil","L_DPOC","L_forest")

  G_improvement_ss <- -read_excel(path,
                                  sheet = "Payback Time and CO2 emissions",
                                  range = "C26:E26",
                                  col_names = c("Exp","Min","Max"))

  G_improvement_ss <- cbind(data.frame(source = "L_improvement_tot"), G_improvement_ss)

  res_ss <- rbind(L_ss,G_improvement_ss)

  res_comp <- left_join(pivot_longer(res,
                                     cols = c(Exp, Min, Max),
                                     names_to = "estimate",
                                     values_to = "val.R"),
                        pivot_longer(res_ss,
                                     cols = c(Exp, Min, Max),
                                     names_to = "estimate",
                                     values_to = "val.Excel"),
                        by = c("source", "estimate"))

  res_comp %>% print(n=Inf)

  ggplot(res_comp, aes(x = paste(source, estimate), y = (val.R - val.Excel))) +
    geom_bar(stat = "identity") +
    labs(x="") +
    theme(axis.text.x = element_text(
      angle = 90,
      hjust = 1,
      vjust = 1))
}
