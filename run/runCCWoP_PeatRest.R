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
construct.dat <- dat$construct.dat
rm(dat)

# rm(core.dat)

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

# Check no change in result
res0 <- read.csv("../Data/res_final.csv")
par(mfrow=c(1,2))
hist(res$value-res0$value)
plot(res$value-res0$value)

t_payback_res <- Carbon_payback_time(res, sum_areas = F)
t_payback_res %>% filter(Area == "Area.1")
t_payback_res %>% filter(Area == "Area.2")

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

pES <- plotES(res %>% filter(Area=="Area.1" & Est=="Exp"),
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

png("../Figures/LCA implementation/V5/wood_products.png",
    width=10, height=7.5, units="cm", res=300)
pWP
dev.off()

hh <- 10
ww <- 16

png("../Figures/LCA implementation/V5/S_forest.png",
    width=ww, height=6, units="cm", res=300)
p_S_forest
dev.off()

hh <- 10
ww <- 16

png("../Figures/LCA implementation/V5/ES_fig.png",
    width=ww, height=6.5, units="cm", res=300)
pES
dev.off()

png("../Figures/LCA implementation/V5/L_forest_soils.png",
    width=1.1*ww, height=hh, units="cm", res=300)
p_L_forest_soils
dev.off()

png("../Figures/LCA implementation/V5/Counterfactual.png",
    width=1.2*ww, height=6, units="cm", res=300)
p_Counterfactual
dev.off()

png("../Figures/LCA implementation/V5/L_forest_cont.png",
    width=1.2*ww, height=hh, units="cm", res=300)
p_L_forest[[1]]
dev.off()

png("../Figures/LCA implementation/V5/L_forest_disc.png",
    width=ww, height=hh, units="cm", res=300)
p_L_forest[[2]]
dev.off()

png("../Figures/LCA implementation/V5/L_peatland.png",
    width=1.1*ww, height=hh, units="cm", res=300)
p_L_peatland
dev.off()

png("../Figures/LCA implementation/V5/LCA.png",
    width=1.1*ww, height=hh, units="cm", res=300)
pLCA
dev.off()

png("../Figures/LCA implementation/V5/LCA_cs.png",
    width=1.1*ww, height=hh, units="cm", res=300)
pLCA_cs
dev.off()

