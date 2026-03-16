## 5b. CO2 loss from removed peat

#' CO2_loss_removed
#' @param core.dat UI data
#' @param AV_direct Area/Volume of removed peat
#' @param L_indirect CO2 from drained peat
#' @param pCO2_lost percent Carbon lost as CO2
#' @return Hypothetical carbon fixation of drained/removed peat
#' @export
CO2_loss_removed <- function(core.dat,
                             AV_direct,
                             L_indirect,
                             pCO2_lost = 100) {

  # THIS FUNCTION...

  # Extract input variables for easy access
  pC_dry_peat <- core.dat$Peatland$pC_dry_peat # Carbon content of dry peat
  BD_dry_soil <- core.dat$Peatland$BD_dry_soil # Dry soil bulk density
  A_direct <- AV_direct$Total$a / 10000 # Area peat removed in ha
  V_direct <- AV_direct$Total$v # Volume peat removed
  A_indirect <- AV_indirect$Total$a / 10000 # Area peat drained in ha

  # Here $Tot is NOT used since this may have been computed as (CO2_C / 100) * pC_dry_peat * BD_dry_soil * V_indirect
  L_undrained_pa <- (L_indirect$L_undrained$CO2 + L_indirect$L_undrained$CH4) / A_indirect # Emissions from undrained peat per unit area

  CO2_C <- 3.667 # Molecular weight ratio C to CO2

  # Total GHG emissions from removed land
  L_removed <- (pCO2_lost / 100) * (CO2_C / 100) * pC_dry_peat * BD_dry_soil * V_direct

  L_undrained <- L_undrained_pa * A_direct # unit conversion to hectares cancels here so not needed

  L_direct <- L_removed - L_undrained

  return(L_direct)
}
