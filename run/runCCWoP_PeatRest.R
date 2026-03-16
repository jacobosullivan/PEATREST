library(readxl) # read excel files (UI)
library(tidyverse) # dataframe manipulations
library(purrr) # nested list indexing
require(scales) # graphing colours
devtools::document()

################################################################################
############################# Load input data from UI ##########################
################################################################################

path <- "Templates/PEATREST_input_scenario_modelling.xlsx" # select user input spreadsheet

forestry.dat <- getData(path)
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
forestry.dat$Aq.Carbon$pAqC_CO2 <- repExpVal(forestry.dat$Aq.Carbon$pAqC_CO2)
forestry.dat$Emissions$E_transport <- repExpVal(forestry.dat$Emissions$E_transport)
forestry.dat$Emissions$E_mulch <- repExpVal(forestry.dat$Emissions$E_mulch)
forestry.dat$Emissions$E_dam <- repExpVal(forestry.dat$Emissions$E_dam)
forestry.dat$Emissions$E_bund <- repExpVal(forestry.dat$Emissions$E_bund)
forestry.dat$Emissions$E_smooth <- repExpVal(forestry.dat$Emissions$E_smooth)
forestry.dat$Root.depth$d_root_max <- repExpVal(forestry.dat$Root.depth$d_root_max)
forestry.dat$Root.depth$rho_r_soil <- repExpVal(forestry.dat$Root.depth$rho_r_soil)
forestry.dat$Area.1$t_fallow <- repExpVal(forestry.dat$Area.1$t_fallow)
forestry.dat$Area.2$t_fallow <- repExpVal(forestry.dat$Area.2$t_fallow)
forestry.dat$Area.1$t_restore_peatland <- repExpVal(forestry.dat$Area.1$t_restore_peatland)
forestry.dat$Area.2$t_restore_peatland <- repExpVal(forestry.dat$Area.2$t_restore_peatland)
forestry.dat$Area.1$n_restore_peatland <- repExpVal(forestry.dat$Area.1$n_restore_peatland)
forestry.dat$Area.2$n_restore_peatland <- repExpVal(forestry.dat$Area.2$n_restore_peatland)
forestry.dat$Area.1$d_wt_drained <- repExpVal(forestry.dat$Area.1$d_wt_drained)
forestry.dat$Area.2$d_wt_drained <- repExpVal(forestry.dat$Area.2$d_wt_drained)

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

forestry.dat$parms_decay <- parms_decay
forestry.dat$parms_3PG <- parms_3PG
forestry.dat$parms_fE <- parms_fE

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
                                    S_forest)

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

# min((res %>%
#   filter(model == "Forest_soils", source == "L_CO2"))$value)
# res %>%
#   filter(model == "Forest_soils", source == "L_CO2") %>%
#   ggplot(aes(x=t, y=value, col=factor(Est), linetype=factor(Area))) +
  # geom_line()

res %>%
  filter(model=="Peatland")

# Check no change in result
res0 <- read.csv("../Data/res_final.csv")
par(mfrow=c(1,2))
hist(res$value-res0$value)
plot(res$value-res0$value)

t_payback_res <- CarbonMitigationMod(res, sum_areas = F)
# t_payback_res %>% filter(Area == "Area.1")
# t_payback_res %>% filter(Area == "Area.2")

################################################################################
########################### Generate summary plots #############################
################################################################################

p_S_forest <- plotS_forest(res %>% filter(Area == "Area.1"))
# p_S_forest

p_L_forest_soils <- plotL_forest_soils(res)
# p_L_forest_soils

p_L_AqC_forest_soils <- plotL_AqC_forest_soils(res)
# p_L_AqC_forest_soils

p_Counterfactual <- plotCounterfactual(res %>% filter(Area == "Area.1"))
# p_Counterfactual

p_L_forest <- plotL_forest(res)
# p_L_forest[[1]]
# p_L_forest[[2]]

p_L_peatland <- plotL_peatland(res)
# p_L_peatland

p_L_AqC_peatland <- plotL_AqC_peatland(res)
# p_L_AqC_peatland

pLCA <- plotLCA(res, t_payback_res, sum_areas = F)
pLCA_cs <- plotLCA_cs(res, t_payback_res, sum_areas = F)
# pLCA
# pLCA_cs

## Executive summary plot

pES <- plotES(res %>% filter(Area=="Area.1" & Est=="Exp" ),
              t_payback_res %>% filter(Area=="Area.1" & Est=="Exp"))

pWP <- ggplot(growthYield.dat %>% filter(YC %in% c(6, 14), Spp == "Sitka_spruce") %>%
                select(YC, Age, rho_Biofuel, rho_wpF, rho_wpM, rho_wpS, rho_wpO) %>%
                pivot_longer(cols = starts_with("rho_"),
                             names_to = "product") %>%
                mutate(product = factor(product, levels = c("rho_Biofuel", "rho_wpF", "rho_wpM", "rho_wpS", "rho_wpO"))),
              aes(x=Age, y=value, col=factor(product), linetype=factor(YC))) +
  geom_line() +
  scale_linetype_manual(values=c(2,1)) +
  scale_color_manual(values = hue_pal()(5),
                     labels = c(expression(rho[B]), expression(rho[F]), expression(rho[M]), expression(rho[S]), expression(rho[O]))) +
  theme_bw() +
  labs(x="Stand age (y)", y="Proportional allocation", col="", linetype="YC")

png("../Figures/LCA implementation/V7/wood_products.png",
    width=10, height=7.5, units="cm", res=300)
pWP
dev.off()

hh <- 10
ww <- 16

png("../Figures/LCA implementation/V7/S_forest.png",
    width=ww, height=6, units="cm", res=300)
p_S_forest
dev.off()

hh <- 10
ww <- 16

png("../Figures/LCA implementation/V7/ES_fig.png",
    width=ww, height=6.5, units="cm", res=300)
pES
dev.off()

png("../Figures/LCA implementation/V7/L_forest_soils.png",
    width=1.1*ww, height=hh, units="cm", res=300)
p_L_forest_soils
dev.off()

png("../Figures/LCA implementation/V7/Counterfactual.png",
    width=1.2*ww, height=6, units="cm", res=300)
p_Counterfactual
dev.off()

png("../Figures/LCA implementation/V7/L_forest_cont.png",
    width=1.2*ww, height=hh, units="cm", res=300)
p_L_forest[[1]]
dev.off()

png("../Figures/LCA implementation/V7/L_forest_disc.png",
    width=ww, height=hh, units="cm", res=300)
p_L_forest[[2]]
dev.off()

png("../Figures/LCA implementation/V7/L_peatland.png",
    width=1.1*ww, height=hh, units="cm", res=300)
p_L_peatland
dev.off()

png("../Figures/LCA implementation/V7/LCA.png",
    width=1.1*ww, height=hh, units="cm", res=300)
pLCA
dev.off()

png("../Figures/LCA implementation/V7/LCA_cs.png",
    width=1.1*ww, height=hh, units="cm", res=300)
pLCA_cs
dev.off()

