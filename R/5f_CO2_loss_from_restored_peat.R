## 5f. CO2 loss from restored peatland

#' CO2_loss_restoration
#' @param core.dat UI data
#' @param AV_indirect Area/Volume of drained peat
#' @param R_tot estimated emissions rates
#' @return L_indirect
#' @export
CO2_loss_restoration <- function(core.dat, R_tot) {

  ## This function will estimate the emissions from the site following harvesting and restoration interventions
  ## assuming a non-linear restoration of ecosystem function parameterised by the user

  CO2_C <- 3.667 # Molecular weight ratio C to CO2
  conv_val <- 0.99 # convergence value required by arbitrary convergent function

  # Extract input variables for easy access
  peat_type <- core.dat$Peatland$peat_type # may not be required, depends if ECOSSE can resolve peat type
  A_harv <- map(forestry.dat[grep("Area", names(forestry.dat))], .f = "A_harv") # in units ha
  t_fallow <- map(forestry.dat[grep("Area", names(forestry.dat))], .f = "t_fallow") # time between felling and restoration
  t_restore <- map(forestry.dat[grep("Area", names(forestry.dat))], .f = "t_restore_microbes") # time to restoration of microbial function
  n_restore <- map(forestry.dat[grep("Area", names(forestry.dat))], .f = "n_restore_microbes") # shape parameter for restoration of microbial function

  ## Compute emissions rates from deforested, unrestored peatland
  D_f <- 0 # Assume flooded days per year D_f = 0 for drained, unrestored peats
  pD_f <- D_f / 365

  CO2_dry <- lapply(seq_along(A_harv), FUN = function(x) {
    res <- A_harv[[x]] * ((R_tot[[x]]$R_CO2_wet * pD_f) + (R_tot[[x]]$R_CO2_dry * (1 - pD_f)))
    return(res)
  })
  names(CO2_dry) <- names(A_harv)

  CH4_dry <- lapply(seq_along(A_harv), FUN = function(x) {
    res <- A_harv[[x]] * ((R_tot[[x]]$R_CH4_wet * pD_f) + (R_tot[[x]]$R_CH4_dry * (1 - pD_f)))
    return(res)
  })
  names(CH4_dry) <- names(A_harv)

  ## Compute emissions rates from deforested, restored peatland

  if (peat_type[1] == 1) { # Acid bog selected
    D_f <- 178
  } else {
    D_f <- 169
  }
  pD_f <- D_f / 365

  CO2_wet <- lapply(seq_along(A_harv), FUN = function(x) {
    res <- A_harv[[x]] * ((R_tot[[x]]$R_CO2_wet * pD_f) + (R_tot[[x]]$R_CO2_dry * (1 - pD_f)))
    return(res)
  })
  names(CO2_wet) <- names(A_harv)

  CH4_wet <- lapply(seq_along(A_harv), FUN = function(x) {
    res <- A_harv[[x]] * ((R_tot[[x]]$R_CH4_wet * pD_f) + (R_tot[[x]]$R_CH4_dry * (1 - pD_f)))
    return(res)
  })
  names(CH4_wet) <- names(A_harv)

  ## Interpolate emissions across restoration phase (arbitrary asymptotic function)
  L_CO2_microbes <- lapply(seq_along(A_harv), FUN = function(x) {
    res <- lapply(seq_along(A_harv[[x]]), FUN = function(y) {
      rest_dyn_mod(t = 1:t_restore[[x]][y],
                   n = n_restore[[x]][y],
                   ymin = CO2_dry[[x]][y], # pre-restoration CO2 emissions rate scaled by area (units CO2)
                   ymax = CO2_wet[[x]][y], # pre-restoration CO2 emissions rate scaled by area (units CO2)
                   convThresh = conv_val)
    })
    names(res) <- names(A_harv[[x]])
    return(res)
  })

  L_CH4_microbes <- lapply(seq_along(A_harv), FUN = function(x) {
    res <- lapply(seq_along(A_harv[[x]]), FUN = function(y) {
      rest_dyn_mod(t = 1:t_restore[[x]][y],
                   n = n_restore[[x]][y],
                   ymin = CH4_dry[[x]][y], # pre-restoration CO2 emissions rate scaled by area (units CO2)
                   ymax = CH4_wet[[x]][y], # pre-restoration CO2 emissions rate scaled by area (units CO2)
                   convThresh = conv_val)
    })
    names(res) <- names(A_harv[[x]])
    return(res)
  })

  # Add fallow period to time series
  L_CO2_microbes <- lapply(seq_along(L_CO2_microbes), FUN = function(x) {
    res <- lapply(seq_along(L_CO2_microbes[[x]]), FUN = function(y) {
      return(c(rep(CO2_dry[[x]][y], t_fallow[[x]][y]), unname(unlist(L_CO2_microbes[[x]][y]))))
    })
    names(res) <- names(L_CO2_microbes[[x]])
    return(res)
  })

  L_CH4_microbes <- lapply(seq_along(L_CH4_microbes), FUN = function(x) {
    res <- lapply(seq_along(L_CH4_microbes[[x]]), FUN = function(y) {
      return(c(rep(CH4_dry[[x]][y], t_fallow[[x]][y]), unname(unlist(L_CH4_microbes[[x]][y]))))
    })
    names(res) <- names(L_CH4_microbes[[x]])
    return(res)
  })

  # Extend time series to 500 years (if t_payback > 500, these will need to be extended again in the run script)
  L_CO2_microbes <- lapply(seq(L_CO2_microbes), FUN = function(x) {
    res <- lapply(seq_along(L_CO2_microbes[[x]]), FUN = function(y) {
      ext <- 501 - length(unlist(L_CO2_microbes[[x]][y]))
      return(unname(c(unlist(L_CO2_microbes[[x]][y]), rep(CO2_wet[[x]][y], ext))))
    })
    names(res) <- names(L_CO2_microbes[[x]])
    return(res)
  })

  L_CH4_microbes <- lapply(seq(L_CH4_microbes), FUN = function(x) {
    res <- lapply(seq_along(L_CH4_microbes[[x]]), FUN = function(y) {
      ext <- 501 - length(unlist(L_CH4_microbes[[x]][y]))
      return(unname(c(unlist(L_CH4_microbes[[x]][y]), rep(CH4_wet[[x]][y], ext))))
    })
    names(res) <- names(L_CH4_microbes[[x]])
    return(res)
  })

  names(L_CO2_microbes) <- names(A_harv)
  names(L_CH4_microbes) <- names(A_harv)

  return(L_microbes = list(CO2 = L_CO2_microbes,
                           CH4 = L_CH4_microbes))

  # L_peat_rest_phase <- list(Tot = list_op(l1 = L_CO2_rest_phase,
  #                                         l2 = L_CH4_rest_phase,
  #                                         func = "+"),
  #                           CO2 = L_CO2_rest_phase,
  #                           CH4 = L_CH4_rest_phase)

  # return(L_peat_rest_phase)
}
