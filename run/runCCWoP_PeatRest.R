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

#################### NEW INPUT VARIABLES. NEED TO ADD THESE TO UI ##############
### These have been added to the UI already
# core.dat$Peatland.restoration$t_restore_microbes <- c(Exp = 10, Min = 8, Max = 12) # time to restoration of microbial function [yr]
# core.dat$Peatland.restoration$n_restore_microbes <- c(Exp = 2, Min = 2, Max = 2) # shape parameter for restoration of microbial function: 1 gives concave downward function (rapid increase in function)
# core.dat$Peatland.restoration$t_fallow <- c(Exp = 5, Min = 3, Max = 7) # time between harvesting and restoration [yr]
# core.dat$Peatland.restoration$n_restore <- c(Exp = 4, Min = 4, Max = 4) # shape parameter for restoration of bog plant function: 1 gives concave downward function (rapid increase in function)
#################### NEW INPUT VARIABLES. NEED TO ADD THESE TO UI ##############

## If YC not passed by user compute here and store in variable forestry.dat$Area.X$YC

## Counterfactuals in matrix form for fast multiplication
# E_mat <- matrix(c(core.dat$Counterfactual$E_coal,
#                   core.dat$Counterfactual$E_grid_mix,
#                   core.dat$Counterfactual$E_fossil_mix),
#                 ncol=3,
#                 byrow = T,
#                 dimnames = list(c("coal", "grid_mix", "fossil_mix"),
#                                 names(core.dat$Counterfactual$E_coal)))

################################################################################
##################### CO2 sequestration loss from Forestry #####################
################################################################################

S_forest <- C_sequest_in_trees_RM(core.dat,
                                  forestry.dat)

# This needs to be multiplied by 30.66 to get from units of C to CO2 for carbon payback!
# Leave in units C for now for the wood product modelling

plot(NPP_pa ~ t, S_forest$Area.1$Exp, type='l')
plot(NPP ~ t, S_forest$Area.1$Exp, type='l')

plot(NPP_tot_pa ~ t, S_forest$Area.1$Exp, type='l')
plot(NPP_tot ~ t, S_forest$Area.1$Exp, type='l')


################################################################################
############ Harvesting/Restoration emissions and wood product decay ###########
################################################################################

# Q what is the appropriate volume for the harvesting? Above ground only?
L_forest <- Forestry_CO2_loss_detail_RM(core.dat,
                                        forestry.dat,
                                        growthYield.dat,
                                        S_forest)

################################################################################
############################ Volume of peat drained ############################
################################################################################

# AV_indirect <- AV_peat_drained(core.dat = core.dat,
#                                construct.dat = construct.dat)

################################################################################
############################ Volume of peat removed ############################
################################################################################

# AV_direct <- AV_peat_removed(core.dat = core.dat,
#                              construct.dat = construct.dat)

################################################################################
######################### Loss of CO2 fixing potential #########################
################################################################################

# L_fix <- Loss_of_CO2_fix_pot(core.dat = core.dat,
#                              AV_direct = AV_direct,
#                              AV_indirect = AV_indirect)

S_bog_sequ <- list()
# nn <- 1:5
nn <- c(1,2.25,5)
# nn <- c(1,3,8)
for (n in nn) {
  core.dat$Bog.plants$n_restore <- c(Exp = n, Min = n, Max = n) # shape parameter for restoration of hydrology: 1 gives concave downward function (rapid increase in function)
  S_bog_sequ[[length(S_bog_sequ)+1]] <- Bog_plant_restoration(core.dat, AV_indirect) # Replace AV_indirect with estimate of total area deforested from UI
}

S_bog_sequ_df <- bind_rows(lapply(seq(S_bog_sequ),
                                  FUN = function(x) {
                                    df <- as.data.frame(S_bog_sequ[[x]])
                                    df$n <- nn[x]
                                    df$t <- 0:(nrow(df)-1)
                                    return(df)
                                  }))


S_bog_sequ_df <- S_bog_sequ_df %>%
  pivot_longer(cols=c(Exp, Min, Max), names_to = "Est", values_to = "val")

p1 <- ggplot(S_bog_sequ_df, aes(x=t, y=val, col=factor(n), group_by=Est)) +
  geom_line() +
  scale_x_continuous(limits = c(0, 40)) +
  facet_grid(Est ~ ., scales="free_y") +
  theme_bw() +
  labs(x="Time since harvesting [y]", y="Bog plant sequestration [tCO2]", col="n")

################################################################################
########################## Emissions rates from soils ##########################
################################################################################

R_tot <- Emissions_rates_soils(core.dat = core.dat,
                               construct.dat = construct.dat,
                               AV_indirect = AV_indirect)

################################################################################
############################### Loss of Soil CO2 ###############################
################################################################################

L_indirect <- CO2_loss_drained(core.dat = core.dat,
                               AV_indirect = AV_indirect,
                               R_tot = R_tot)

L_direct <- CO2_loss_removed(core.dat = core.dat,
                             AV_direct = AV_direct,
                             L_indirect = L_indirect)

L_soil <- CO2_loss_from_soil(L_direct = L_direct,
                             L_indirect = L_indirect)

L_restoration <- list()
# nn <- 1:5
nn <- c(1,2.25,5)
# nn <- c(1,3,8)
for (n in nn) {
  core.dat$Peatland.restoration$n_restore_microbes <- c(Exp = n, Min = n, Max = n) # shape parameter for restoration of hydrology: 1 gives concave downward function (rapid increase in function)
  L_restoration[[length(L_restoration)+1]] <- CO2_loss_restoration(core.dat, AV_indirect, R_tot)
}

L_restoration_df <- bind_rows(lapply(seq(L_restoration),
                                     FUN = function(y) lapply(seq(L_restoration[[y]]),
                                                              FUN = function(x) {
                                                                source <- names(L_restoration[[y]])[x]
                                                                df <- as.data.frame(L_restoration[[y]][[x]])
                                                                df$source <- source
                                                                df$n <- nn[y]
                                                                df$t <- 0:(nrow(df)-1)
                                                                return(df)
                                                              })))

L_restoration_df$source <- factor(L_restoration_df$source, levels=c("CO2", "CH4", "Tot"))
L_restoration_df <- L_restoration_df %>%
  pivot_longer(cols=c(Exp, Min, Max), names_to = "Est", values_to = "val")

p2 <- ggplot(L_restoration_df, aes(x=t, y=val, col=factor(n), group_by=Est)) +
  geom_line() +
  scale_x_continuous(limits = c(0, 20)) +
  facet_grid(Est ~ source, scales="free_y") +
  theme_bw() +
  labs(x="Time since harvesting [y]", y="Gaseous C emissions [eq tCO2]", col="n")

p1
p2
################################################################################
####################### CO2 gain due to site improvement #######################
################################################################################

L_improvement <- CO_2_gain_site_improve(core.dat = core.dat)

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
