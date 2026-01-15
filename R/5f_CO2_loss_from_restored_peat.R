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

  AVG_WTD <- T # if TRUE remove partitioning into flooded and unflooded days and assume that annual average WTD can be used
  # Averages will work so long as R_CX ~ WTD is approximately linear. This is true for CO2 in the range WTD = [0,1m] but NOT for CH4
  # Thus, average WTD will systematically under estimate methane emissions

  # Extract input variables for easy access
  peat_type <- core.dat$Peatland$peat_type # may not be required, depends if ECOSSE can resolve peat type
  A_harv <- map(forestry.dat[grep("Area", names(forestry.dat))], .f = "A_harv") # in units ha
  t_fallow <- map(forestry.dat[grep("Area", names(forestry.dat))], .f = "t_fallow") # time between felling and restoration
  t_restore <- map(forestry.dat[grep("Area", names(forestry.dat))], .f = "t_restore_microbes") # time to restoration of microbial function
  n_restore <- map(forestry.dat[grep("Area", names(forestry.dat))], .f = "n_restore_microbes") # shape parameter for restoration of microbial function

  ## Compute emissions rates from deforested, unrestored peatland

  if (!AVG_WTD) { # partition into flooded and unflooded days. 2 values of WTD needed in principle

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

  } else { # take the annual average water table depth (robust in linear region of emissions function)

    CO2_dry <- lapply(seq_along(A_harv), FUN = function(x) {
      res <- A_harv[[x]] * R_tot[[x]]$R_CO2_dry
      return(res)
    })
    names(CO2_dry) <- names(A_harv)

    CH4_dry <- lapply(seq_along(A_harv), FUN = function(x) {
      res <- A_harv[[x]] * R_tot[[x]]$R_CH4_dry
      return(res)
    })
    names(CH4_dry) <- names(A_harv)

  }

  ## Compute emissions rates from deforested, restored peatland

  if (!AVG_WTD) { # partition into flooded and unflooded days. 2 values of WTD needed in principle

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

  } else { # take the annual average water table depth (robust in linear region of emissions function)

    CO2_wet <- lapply(seq_along(A_harv), FUN = function(x) {
      res <- A_harv[[x]] * R_tot[[x]]$R_CO2_wet
      return(res)
    })
    names(CO2_wet) <- names(A_harv)

    CH4_wet <- lapply(seq_along(A_harv), FUN = function(x) {
      res <- A_harv[[x]] * R_tot[[x]]$R_CH4_wet
      return(res)
    })
    names(CH4_wet) <- names(A_harv)

  }

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

  # Update data structure to timeseries/dataframes
  L_CO2_microbes <- lapply(seq_along(L_CO2_microbes), FUN = function (x) {
    L_CO2_microbes_a <- lapply(seq_along(L_CO2_microbes[[x]]), FUN = function(y) {
      L <- data.frame(t = 0:(length(L_CO2_microbes[[x]][[y]])-1),
                      L_CO2 = L_CO2_microbes[[x]][[y]])
      return(L)
    })
    names(L_CO2_microbes_a) <- names(L_CO2_microbes[[x]])
    return(L_CO2_microbes_a)
  })

  L_CH4_microbes <- lapply(seq_along(L_CH4_microbes), FUN = function (x) {
    L_CH4_microbes_a <- lapply(seq_along(L_CH4_microbes[[x]]), FUN = function(y) {
      L <- data.frame(t = 0:(length(L_CH4_microbes[[x]][[y]])-1),
                      L_CH4 = L_CH4_microbes[[x]][[y]])
      return(L)
    })
    names(L_CH4_microbes_a) <- names(L_CH4_microbes[[x]])
    return(L_CH4_microbes_a)
  })

  names(L_CO2_microbes) <- names(A_harv)
  names(L_CH4_microbes) <- names(A_harv)

  L_microbes <- lapply(seq_along(L_CO2_microbes), FUN = function (x) {
    L_microbes_a <- lapply(seq_along(L_CO2_microbes[[x]]), FUN = function(y) {
      L <- L_CO2_microbes[[x]][[y]]
      L$L_CH4 <- L_CH4_microbes[[x]][[y]]$L_CH4
      return(L)
    })
    names(L_microbes_a) <- names(L_CO2_microbes[[x]])
    return(L_microbes_a)
  })

  names(L_microbes) <- names(A_harv)

  return(L_microbes)
}
