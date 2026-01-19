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

## YC not passed by user, these are estimated from height/age data
if (any(sapply(map(forestry.dat[grep("Area", names(forestry.dat))], .f = "YC"), FUN = is.null))) {
  YC <- getYC(forestry.dat,
              growthYield.dat)
  for (i in 1:length(YC)) {
    forestry.dat[[names(YC)[i]]]$YC <- YC[[i]]
  }
}

################################################################################
##################### CO2 sequestration loss from Forestry #####################
################################################################################

S_forest <- C_sequest_in_trees_RM(forestry.dat)

p_S_forest <- plotS_forest(S_forest)
p_S_forest

################################################################################
###################### CO2 loss from soils under Forestry ######################
################################################################################

L_forest_soils <- Emissions_rates_forestry_soils_RM(core.dat,
                                                    forestry.dat,
                                                    growthYield.dat)

p_L_forest_soils <- plotL_forest_soils(L_forest_soils)
p_L_forest_soils

################################################################################
############ Harvesting/Restoration emissions and wood product decay ###########
################################################################################

# JDebug: What is the appropriate volume for the harvesting? Above ground only?
L_forest <- Forestry_CO2_loss_detail_RM(core.dat,
                                        forestry.dat,
                                        growthYield.dat,
                                        S_forest)

p_L_forest <- plotL_forest(L_forest)

p_L_forest[[1]]
p_L_forest[[2]]

################################################################################
########################### Bog plant sequestration ############################
################################################################################

S_bog_plants <- Bog_plant_sequestration_RM(core.dat,
                                           forestry.dat)

p_S_bog_plants <- plotS_bog_plants(S_bog_plants)
p_S_bog_plants

################################################################################
########################## Emissions rates from soils ##########################
################################################################################

R_tot <- Emissions_rates_soils_RM(core.dat = core.dat,
                                  forestry.dat = forestry.dat)

################################################################################
############################### Loss of Soil CO2 ###############################
################################################################################

L_microbes <- CO2_loss_restoration(core.dat = core.dat,
                                   R_tot = R_tot)

p_L_microbes <- plotL_microbes(L_microbes)
p_L_microbes

################################################################################
######################### CO2 loss by DOC and POC loss #########################
################################################################################

L_DPOC <- CO2_loss_DOC_POC_RM(core.dat = core.dat,
                              L_microbes = L_microbes)

p_L_DPOC <- plotL_DPOC(L_DPOC)
p_L_DPOC

################################################################################
######################### CO2 payback time estimation ##########################
################################################################################

LCA <- Carbon_payback_time(S_forest,
                           R_tot_forestry,
                           S_bog_plants,
                           L_forest,
                           L_microbes,
                           L_DPOC)
LCA

################################################################################
################################# Save plots ###################################
################################################################################

ww <- 16
hh <- 12

png("../Figures/LCA implementation/S_forest.png",
    width=ww, height=hh, units="cm", res=300)
p_S_forest
dev.off()

png("../Figures/LCA implementation/L_forest_soils.png",
    width=ww, height=hh, units="cm", res=300)
p_L_forest_soils
dev.off()

png("../Figures/LCA implementation/L_forest_cont.png",
    width=ww, height=hh, units="cm", res=300)
p_L_forest[[1]]
dev.off()

png("../Figures/LCA implementation/L_forest_disc.png",
    width=ww, height=hh, units="cm", res=300)
p_L_forest[[2]]
dev.off()

png("../Figures/LCA implementation/S_bog_plants.png",
    width=ww, height=hh, units="cm", res=300)
p_S_bog_plants
dev.off()

png("../Figures/LCA implementation/L_peatland.png",
    width=ww, height=hh, units="cm", res=300)
p_L_microbes
dev.off()

png("../Figures/LCA implementation/L_DPOC.png",
    width=ww, height=hh, units="cm", res=300)
p_L_DPOC
dev.off()

png("../Figures/LCA implementation/LCA.png",
    width=ww, height=hh, units="cm", res=300)
LCA
dev.off()

