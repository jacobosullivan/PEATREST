## 3. CO2 loss due to backup

#' Backup_emissions
#' @param core.dat UI data
#' @param E_mat counterfactuals in matrix form with IDs as rownames
#' @return Backup lifetime emissions
#' @export
Backup_emissions <- function(core.dat,
                             E_mat) {

  # THIS FUNCTION...

  # Extract input variables for easy access
  c_turb <- core.dat$Windfarm$c_turb
  n_turb <- core.dat$Windfarm$n_turb
  p_therm <- core.dat$Windfarm$p_therm
  p_back <- core.dat$Windfarm$p_back
  E_mat <- E_mat
  t_wf <- core.dat$Windfarm$t_wf

  if (0) {
    # This version reproduces the 3x3 output in the spreadsheet LCA, however
    # only the fossil_mix counterfactual is ultimately used in the payback time
    L_back_tot <- matrix((p_therm/100) * (24*365) * n_turb * c_turb * (p_back/100) * t_wf,
                         nrow = nrow(E_mat),
                         ncol = length(p_therm),
                         byrow = T,
                         dimnames = dimnames(E_mat))

    L_back <- L_back_tot * E_mat
  } else {
    # Back up emissions assuming fossil_mix counterfactual
    L_back <- (p_therm/100) * (24*365) * n_turb * c_turb * (p_back/100) * t_wf * E_mat["fossil_mix",]
  }

  return(L_back)
}
