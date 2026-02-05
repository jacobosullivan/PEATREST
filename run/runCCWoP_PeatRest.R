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

################################################################################
###################### CO2 loss from soils under Forestry ######################
################################################################################

L_forest_soils <- Emissions_rates_forestry_soils_RM(core.dat,
                                                    forestry.dat,
                                                    growthYield.dat)

################################################################################
############ Harvesting/Restoration emissions and wood product decay ###########
################################################################################

# JDebug: What is the appropriate volume for the harvesting? Above ground only?
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
######################### CO2 loss by DOC and POC loss #########################
################################################################################

L_DPOC <- CO2_loss_DOC_POC_RM(core.dat = core.dat,
                              L_peatland = L_peatland)

################################################################################
######################### CO2 payback time estimation ##########################
################################################################################

res <- getCarbonDf(S_forest,
                   R_tot_forestry,
                   L_forest,
                   L_peatland,
                   L_DPOC)

t_payback_res <- Carbon_payback_time(res, sum_areas = F)
t_payback_res

################################################################################
########################### Generate summary plots #############################
################################################################################

p_S_forest <- plotS_forest(res %>% filter(Area == "Area.1"))
p_S_forest

p_L_forest_soils <- plotL_forest_soils(res)
p_L_forest_soils

p_Counterfactual <- plotCounterfactual(res %>% filter(Area == "Area.1"))
p_Counterfactual

p_L_forest <- plotL_forest(res)
p_L_forest[[1]]
p_L_forest[[2]]

p_L_peatland <- plotL_peatland(res)
p_L_peatland

p_L_DPOC <- plotL_DPOC(res)
p_L_DPOC

pLCA <- plotLCA(res, t_payback_res, sum_areas = F)
pLCA_cs <- plotLCA_cs(res, t_payback_res, sum_areas = F)
pLCA
pLCA_cs

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

png("../Figures/LCA implementation/V4/wood_products.png",
    width=10, height=7.5, units="cm", res=300)
pWP
dev.off()

hh <- 10
ww <- 16

png("../Figures/LCA implementation/V4/S_forest.png",
    width=ww, height=6, units="cm", res=300)
p_S_forest
dev.off()

png("../Figures/LCA implementation/V4/L_forest_soils.png",
    width=1.1*ww, height=hh, units="cm", res=300)
p_L_forest_soils
dev.off()

png("../Figures/LCA implementation/V4/Counterfactual.png",
    width=1.2*ww, height=6, units="cm", res=300)
p_Counterfactual
dev.off()

png("../Figures/LCA implementation/V4/L_forest_cont.png",
    width=1.2*ww, height=hh, units="cm", res=300)
p_L_forest[[1]]
dev.off()

png("../Figures/LCA implementation/V4/L_forest_disc.png",
    width=ww, height=hh, units="cm", res=300)
p_L_forest[[2]]
dev.off()

png("../Figures/LCA implementation/V4/L_peatland.png",
    width=1.1*ww, height=hh, units="cm", res=300)
p_L_peatland
dev.off()

png("../Figures/LCA implementation/V4/L_DPOC.png",
    width=1.1*ww, height=hh, units="cm", res=300)
p_L_DPOC
dev.off()

png("../Figures/LCA implementation/V4/LCA.png",
    width=1.1*ww, height=hh, units="cm", res=300)
pLCA
dev.off()

png("../Figures/LCA implementation/V4/LCA_cs.png",
    width=1.1*ww, height=hh, units="cm", res=300)
pLCA_cs
dev.off()
