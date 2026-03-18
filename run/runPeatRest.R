library(readxl) # read excel files (UI)
library(tidyverse) # dataframe manipulations
library(purrr) # nested list indexing
require(scales) # graphing colours
devtools::document()

################################################################################
############################# Load input data from UI ##########################
################################################################################

path <- "Templates/PEATREST_input_scenario_modelling.xlsx" # select user input spreadsheet

input.dat <- getData(path)

growthYield <- getGrowthYieldData()

## YC not passed by user, these are estimated from height/age data
if (any(sapply(map(input.dat[grep("Area", names(input.dat))], .f = "YC"), FUN = is.null))) {
  YC <- getYC(input.dat,
              growthYield)
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

input.dat$parms_decay <- parms_decay
input.dat$parms_3PG <- parms_3PG
input.dat$parms_fE <- parms_fE
input.dat$growthYield <- growthYield

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
t_payback_res

################################################################################
########################### Generate summary plots #############################
################################################################################

p_S_forest <- plotS_forest(res %>% filter(Area == "Area.1"))
p_S_forest

p_L_forest_soils <- plotL_forest_soils(res)
p_L_forest_soils

p_L_AqC_forest_soils <- plotL_AqC_forest_soils(res)
p_L_AqC_forest_soils

p_Counterfactual <- plotCounterfactual(res %>% filter(Area == "Area.1"))
p_Counterfactual

p_L_forest <- plotL_forest(res)
p_L_forest[[1]]
p_L_forest[[2]]

p_L_peatland <- plotL_peatland(res)
p_L_peatland

p_L_AqC_peatland <- plotL_AqC_peatland(res)
p_L_AqC_peatland

pLCA <- plotLCA(res, t_payback_res, sum_areas = F)
pLCA_cs <- plotLCA_cs(res, t_payback_res, sum_areas = F)
pLCA
pLCA_cs

pES <- plotES(res %>% filter(Area=="Area.1" & Est=="Exp" ),
              t_payback_res %>% filter(Area=="Area.1" & Est=="Exp"))
pES
