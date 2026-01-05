## 4. Loss of CO2 fixing pot.

#' Loss_of_CO2_fix_pot
#' @param core.dat UI data
#' @param AV_direct Area of removed peat
#' @param AV_indirect Area of drained peat
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

#' Bog_plant_sequestration_RM
#' @param core.dat UI data
#' @param forestry.dat UI forestry data
#' @return S_bog_plants
#' @export
Bog_plant_sequestration_RM <- function(core.dat,
                                       forestry.dat) {

  # THIS FUNCTION...

  CO2_C <- 3.667 # Molecular weight ratio C to CO2
  conv_val <- 0.99

  # Extract input variables for easy access

  ## For now assume fixed rate per unit area sequestration
  G_bog <- core.dat$Bog.plants$G_bog

  ## If using ECOSSE or SCOTIA this will be a function of environmental inputs (MAKE A DUMMY VERSION OF THIS)

  t_restore <- core.dat$Bog.plants$t_restore[c(1,3,2)] # Reorder Min Max
  n_restore <- core.dat$Peatland.restoration$n_restore # NEW UI VARIABLE - shape parameter restoration of bog plant sequestration
  t_fallow <- core.dat$Peatland.restoration$t_fallow[c(1,3,2)] # NEW UI VARIABLE - time between felling and restoration; Reorder Min Max
  A_harv <- map(forestry.dat[grep("Area", names(forestry.dat))], .f = "A_harv") # in units ha

  # JDebug:
  ## At present restoration times and shape parameters are not area dependent
  ## This may need to be changed if interventions differ by area and if we can meaningfully capture those differences in our estimates

  # Compute restoration dynamics (arbitrary asymptotic function)
  S_bog_plants <- lapply(seq_along(A_harv), FUN = function(x) {
    res <- lapply(seq_along(A_harv[[x]]), FUN = function(y) {
      rest_dyn_mod(t = 1:t_restore[y], # NOT area dependent
                   n = n_restore[y], # NOT area dependent
                   ymin = 0, # assume no bog plant sequestration prior to restoration intervention
                   ymax = CO2_C * A_harv[[x]][y] * G_bog[y], # convert into units CO2
                   convThresh = conv_val)
    })
    names(res) <- names(t_restore)
    return(res)
  })

  # Add fallow period to time series
  S_bog_plants <- lapply(seq_along(S_bog_plants), FUN = function(x) {
    res <- lapply(seq_along(S_bog_plants[[x]]), FUN = function(y) {
      return(c(rep(0, t_fallow[y]), unname(unlist(S_bog_plants[[x]][y]))))
    })
    names(res) <- names(S_bog_plants[[x]])
    return(res)
  })

  # Extend time series to 500 years (if t_payback > 500, these will need to be extended again in the run script)
  S_bog_plants <- lapply(seq(S_bog_plants), FUN = function(x) {
    res <- lapply(seq_along(S_bog_plants[[x]]), FUN = function(y) {
      ext <- 500 - length(unlist(S_bog_plants[[x]][y]))
      return(unname(c(unlist(S_bog_plants[[x]][y]), rep(CO2_C * A_harv[[x]][y] * G_bog[y], ext))))
    })
    names(res) <- names(S_bog_plants[[x]])
    return(res)
  })

  names(S_bog_plants) <- names(A_harv)

  return(S_bog_plants)
}
