library(readxl) # read excel files (UI)
library(tidyverse) # dataframe manipulations
library(purrr) # nested list indexing
require(scales) # graphing colours
devtools::document()

################################################################################
############################# Load input data from UI ##########################
################################################################################

path <- "Templates/Full carbon calculator for windfarms on peatlands - Version 2.14.1.xlsx" # select user input spreadsheet

forestry.dat <- getData(path)$forestry.dat
forestry.dat <- forestry.dat[1:(length(forestry.dat)-1)] # drop area 2

growthYield.dat <- getGrowthYieldData()

## YC not passed by user, these are estimated from height/age data
if (any(sapply(map(forestry.dat[grep("Area", names(forestry.dat))], .f = "YC"), FUN = is.null))) {
  YC <- getYC(forestry.dat,
              growthYield.dat)
  for (i in 1:length(YC)) {
    forestry.dat[[names(YC)[i]]]$YC <- YC[[i]]
  }
}

# Fix input parameters for better comparison of model runs
repExpVal <- function(par) {
  par <- rep(par[1], 3)
  names(par) <- c("Exp", "Min", "Max")
  return(par)
}

forestry.dat$Aq.Carbon$rho_AqC <- repExpVal(forestry.dat$Aq.Carbon$rho_AqC)
forestry.dat$Aq.Carbon$R_AqC0 <- repExpVal(forestry.dat$Aq.Carbon$R_AqC0)
forestry.dat$Emissions$E_transport <- repExpVal(forestry.dat$Emissions$E_transport)
forestry.dat$Emissions$E_mulch <- repExpVal(forestry.dat$Emissions$E_mulch)
forestry.dat$Emissions$E_dam <- repExpVal(forestry.dat$Emissions$E_dam)
forestry.dat$Emissions$E_bund <- repExpVal(forestry.dat$Emissions$E_bund)
forestry.dat$Emissions$E_smooth <- repExpVal(forestry.dat$Emissions$E_smooth)
forestry.dat$Area.1$d_wt_restored <- repExpVal(forestry.dat$Area.1$d_wt_restored)

################################################################################
############################# Load decay parameters ############################
################################################################################

alpha_df <- read_xlsx("Templates/alpha_wp.xlsx",
                      sheet = "Sheet1",
                      range = "A1:F10",
                      progress = F)

################################################################################
##################### CO2 sequestration loss from Forestry #####################
################################################################################

S_forest <- ForestSequestrationMod(forestry.dat)

################################################################################
###################### CO2 loss from soils under Forestry ######################
################################################################################

L_forest_soils <- ForestSoilsEmissionsMod(forestry.dat,
                                          growthYield.dat)

################################################################################
################ Aquatic carbon loss from soils under Forestry #################
################################################################################

L_AqC_forest_soils <- AquaticCarbonMod(forestry.dat,
                                          L_forest_soils,
                                          forest_soils = T)

################################################################################
############ Harvesting/Restoration emissions and wood product decay ###########
################################################################################

L_forest <- HarvestingManagementMod(forestry.dat,
                                    growthYield.dat,
                                    S_forest,
                                    alpha_df)

################################################################################
########################## Emissions rates from soils ##########################
################################################################################

R_tot <- Emissions_rates_soils_RM(forestry.dat)

################################################################################
############################### Loss of Soil CO2 ###############################
################################################################################

L_peatland <- PeatlandSoilsEmissionsMod(forestry.dat)

################################################################################
###################### Aquatic carbon loss from peatland #######################
################################################################################

L_AqC_peatland <- AquaticCarbonMod(forestry.dat,
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

# res <- res %>% mutate(Est = ifelse(Est=="Exp", paste0("YC", forestry.dat$Area.1$YC["Exp"]), Est),
#                       Est = ifelse(Est=="Min", paste0("YC", forestry.dat$Area.1$YC["Min"]), Est),
#                       Est = ifelse(Est=="Max", paste0("YC", forestry.dat$Area.1$YC["Max"]), Est))

t_payback_res <- CarbonMitigationMod(res, sum_areas = F)

################################################################################
########################### Generate summary plots #############################
################################################################################

p_Counterfactual <- plotCounterfactual(res, plabs = paste0("YC", forestry.dat$Area.1$YC[c(2,1,3)]))
p_Counterfactual

p_L_forest <- plotL_forest(res %>% filter(Est == "Exp"), plabs = paste0("YC", forestry.dat$Area.1$YC[c(2,1,3)]))
p_L_forest[[1]]
p_L_forest[[2]]

p_L_peatland <- plotL_peatland(res %>% filter(Est == "Exp"), plabs = paste0("YC", forestry.dat$Area.1$YC[c(2,1,3)]))
p_L_peatland

pLCA <- plotLCA(res %>% filter(Est == "Exp"),
                t_payback_res %>% filter(Est == "Exp"),
                sum_areas = F,
                plabs = paste0("YC", forestry.dat$Area.1$YC[c(2,1,3)]))
pLCA <- pLCA + scale_y_continuous(limits=c(-50, NA))
pLCA
pLCA_cs <- plotLCA_cs(res %>% filter(Est == "Exp"),
                      t_payback_res %>% filter(Est == "Exp"),
                      sum_areas = F,
                      plabs = paste0("YC", forestry.dat$Area.1$YC[c(2,1,3)]))
pLCA_cs <- pLCA_cs + scale_x_continuous(limits=c(0, NA))
pLCA_cs

hh <- 8
ww0 <- 12
ww1 <- 20

png("../Figures/LCA implementation/Presentation/Counterfactual.png",
    width=1.2*ww1, height=hh, units="cm", res=300)
p_Counterfactual
dev.off()

png("../Figures/LCA implementation/Presentation/L_forest_cont.png",
    width=ww0, height=hh, units="cm", res=300)
p_L_forest[[1]]
dev.off()

png("../Figures/LCA implementation/Presentation/L_forest_disc.png",
    width=0.85*ww0, height=1.1*hh, units="cm", res=300)
p_L_forest[[2]]
dev.off()

png("../Figures/LCA implementation/Presentation/L_peatland.png",
    width=ww0, height=hh, units="cm", res=300)
p_L_peatland
dev.off()

png("../Figures/LCA implementation/Presentation/LCA.png",
    width=ww0, height=hh, units="cm", res=300)
pLCA
dev.off()

png("../Figures/LCA implementation/Presentation/LCA_cs.png",
    width=ww0, height=hh, units="cm", res=300)
pLCA_cs
dev.off()

