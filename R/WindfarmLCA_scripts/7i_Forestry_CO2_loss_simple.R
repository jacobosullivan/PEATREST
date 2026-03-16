## 7i. Forestry CO2 loss - simple

#' Forestry_CO2_loss_simple
#' @param core.dat UI data
#' @return Estimated lifetime loss of carbon sequestration
#' @export
Forestry_CO2_loss_simple <- function(core.dat) {


  # THIS FUNCTION...

  # Extract input variable for easy access
  A_felled <- core.dat$Forestry$A_felled
  G_forest <- core.dat$Forestry$G_forest
  t_wf <- core.dat$Windfarm$t_wf

  CO2_C <- 3.667 # Molecular weight ratio C to CO2

  L_forest <- A_felled * G_forest * t_wf * CO2_C

  return(L_forest)
}
