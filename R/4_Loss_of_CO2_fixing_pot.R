## 4. Loss of CO2 fixing pot.

#' Loss_of_CO2_fix_pot
#' @param A_direct Area of removed peat
#' @param A_indirect Area of drained peat
#' @param G_bog Peatland carbon accumulation rate
#' @param t_wf Windfarm life time
#' @param t_restore Time to restoration after decomissioning
#' @return L_fix
#' @export
Loss_of_CO2_fix_pot <- function(core.dat,
                                AV_direct,
                                AV_indirect) {

  # THIS FUNCTION...

  CO2_C <- 3.667 # Molecular weight ratio C to CO2

  # Extract input variables for easy access
  G_bog <- core.dat$Bog.plants$G_bog
  t_wf <- core.dat$Windfarm$t_wf
  t_restore <- core.dat$Bog.plants$t_restore
  A_direct <- AV_direct$Total$a
  A_indirect <- AV_indirect$Total$a

  C_accumultion_per_area <- G_bog * (t_wf + t_restore) * CO2_C # convert into units CO2
  L_fix <- C_accumultion_per_area * (A_direct + A_indirect)/10000 # normalisation by 10000 converts from m2 to ha

  return(L_fix)
}
