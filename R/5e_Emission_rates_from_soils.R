## 5e. Emission rates from soils

#' IPCC_CO2
#' @return CO2 emissions rate IPCC Acid bog AND Fen
#' @export
IPCC_CO2 <- function() {
  R_CO2 <- 9.6 * (12 + 16 + 16) / 12
  return(list(R_CO2 = c(Exp = R_CO2, Min = R_CO2, Max = R_CO2))) # Identical for both peat types
}

#' IPCC_CH4_AB
#' @param CH4_CO2 CH4 to CO2 conversion factor
#' @return CH4 emissions rate IPCC Acid bog
#' @export
IPCC_CH4_AB <- function(CH4_CO2) {
  R_CH4 <- CH4_CO2 * (11 / 1000000000) * 10000 * 365
  return(list(R_CH4 = c(Exp = R_CH4, Min = R_CH4, Max = R_CH4)))
}

#' IPCC_CH4_F
#' @param CH4_CO2 CH4 to CO2 conversion factor
#' @return CH4 emissions rate IPCC Fen
#' @export
IPCC_CH4_F <- function(CH4_CO2) {
  R_CH4 = CH4_CO2 * (60 / 1000000000) * 10000 * 365
  return(list(R_CH4 = c(Exp = R_CH4, Min = R_CH4, Max = R_CH4)))
}

#' METAR_CO2_AB
#' @param T_air Average air temperature
#' @param d_wt Average water table depth (undrained)
#' @param CO2_C Molecular weight ratio C to CO2
#' @return CO2 emissions rate meta-analytic regression Acid bog
#' @export
METAR_CO2_AB <- function(CO2_C, d_wt, T_air) {
  return(list(R_CO2 = (CO2_C/1000) * ((6700 * exp(-0.26 * exp(-0.05153 * ((100*d_wt)-50)))) + ((72.54 * T_air) - 800))))
}

#' METAR_CH4_AB
#' @param T_air Average air temperature
#' @param d_wt Average water table depth (undrained)
#' @param CH4_CO2 CH4 to CO2 conversion factor
#' @return CH4 emissions rate meta-analytic regression Acid bog
#' @export
METAR_CH4_AB <- function(CH4_CO2, d_wt, T_air) { # converts into CO2 eq units
  return(list(R_CH4 = (CH4_CO2/1000) * (500 * exp(-0.1234 * (100*d_wt)) + ((3.529*T_air) - 36.67))))
}

#' METAR_CO2_F
#' @param T_air Average air temperature
#' @param d_wt Average water table depth (undrained)
#' @param CO2_C Molecular weight ratio C to CO2
#' @return CO2 emissions rate meta-analytic regression Fen
#' @export
METAR_CO2_F <- function(CO2_C, d_wt, T_air) {
  return(list(R_CO2 = (CO2_C/1000) * (16244 * exp(-0.17594 * exp(-0.07346 * ((d_wt*100)-50))) + (153.234*T_air))))
}

#' METAR_CH4_F
#' @param T_air Average air temperature
#' @param d_wt Average water table depth (undrained)
#' @param CH4_CO2 CH4 to CO2 conversion factor
#' @return CO2 emissions rate meta-analytic regression Fen
#' @export
METAR_CH4_F <- function(CH4_CO2, d_wt, T_air) { # converts into CO2 eq units
  return(list(R_CH4 = (CH4_CO2/1000) * (-10 + 563.6253 * exp(-0.09702 * (100*d_wt)) + (0.662183*T_air))))
}

#' ECOSSER_CO2_AB
#' @param T_air Average air temperature
#' @param d_wt Average water table depth (undrained)
#' @param CO2_C Molecular weight ratio C to CO2
#' @return CO2 emissions rate ECOSSE regression Acid bog
#' @export
ECOSSER_CO2_AB <- function(CO2_C, d_wt, T_air) {
  return(list(R_CO2 = (CO2_C/1000) * ((6700 * exp(-0.26 * exp(-0.05153 * ((100*d_wt)-50)))) + ((72.54 * T_air) - 800))))
}

#' ECOSSER_CH4_AB
#' @param T_air Average air temperature
#' @param d_wt Average water table depth (undrained)
#' @param CH4_CO2 CH4 to CO2 conversion factor
#' @return CH4 emissions rate ECOSSE regression Acid bog
#' @export
ECOSSER_CH4_AB <- function(CH4_CO2, d_wt, T_air) { # converts into CO2 eq units
  return(list(R_CH4 = (CH4_CO2/1000) * (500 * exp(-0.1234 * (100*d_wt)) + ((3.529*T_air) - 36.67))))
}

#' ECOSSER_CO2_F
#' @param T_air Average air temperature
#' @param d_wt Average water table depth (undrained)
#' @param CO2_C Molecular weight ratio C to CO2
#' @return CO2 emissions rate ECOSSE regression Fen
#' @export
ECOSSER_CO2_F <- function(CO2_C, d_wt, T_air) {
  return(list(R_CO2 = (CO2_C/1000) * (16244 * exp(-0.17594 * exp(-0.07346 * ((d_wt*100)-50))) + (153.234*T_air))))
}

#' ECOSSER_CH4_F
#' @param T_air Average air temperature
#' @param d_wt Average water table depth (undrained)
#' @param CH4_CO2 CH4 to CO2 conversion factor
#' @return CO2 emissions rate ECOSSE regression Fen
#' @export
ECOSSER_CH4_F <- function(CH4_CO2, d_wt, T_air) { # converts into CO2 eq units
  return(list(R_CH4 = (CH4_CO2/1000) * (-10 + 563.6253 * exp(-0.09702 * (100*d_wt)) + (0.662183*T_air))))
}

#' SCOTIAR_CO2_AB
#' @param T_air Average air temperature
#' @param d_wt Average water table depth (undrained)
#' @param CO2_C Molecular weight ratio C to CO2
#' @return CO2 emissions rate SCOTIA regression Acid bog
#' @export
SCOTIAR_CO2_AB <- function(CO2_C, d_wt, T_air) {
  return(list(R_CO2 = (CO2_C/1000) * ((6700 * exp(-0.26 * exp(-0.05153 * ((100*d_wt)-50)))) + ((72.54 * T_air) - 800))))
}

#' SCOTIAR_CH4_AB
#' @param T_air Average air temperature
#' @param d_wt Average water table depth (undrained)
#' @param CH4_CO2 CH4 to CO2 conversion factor
#' @return CH4 emissions rate SCOTIA regression Acid bog
#' @export
SCOTIAR_CH4_AB <- function(CH4_CO2, d_wt, T_air) { # converts into CO2 eq units
  return(list(R_CH4 = (CH4_CO2/1000) * (500 * exp(-0.1234 * (100*d_wt)) + ((3.529*T_air) - 36.67))))
}

#' SCOTIAR_CO2_F
#' @param T_air Average air temperature
#' @param d_wt Average water table depth (undrained)
#' @param CO2_C Molecular weight ratio C to CO2
#' @return CO2 emissions rate SCOTIA regression Fen
#' @export
SCOTIAR_CO2_F <- function(CO2_C, d_wt, T_air) {
  return(list(R_CO2 = (CO2_C/1000) * (16244 * exp(-0.17594 * exp(-0.07346 * ((d_wt*100)-50))) + (153.234*T_air))))
}

#' SCOTIAR_CH4_F
#' @param T_air Average air temperature
#' @param d_wt Average water table depth (undrained)
#' @param CH4_CO2 CH4 to CO2 conversion factor
#' @return CO2 emissions rate SCOTIA regression Fen
#' @export
SCOTIAR_CH4_F <- function(CH4_CO2, d_wt, T_air) { # converts into CO2 eq units
  return(list(R_CH4 = (CH4_CO2/1000) * (-10 + 563.6253 * exp(-0.09702 * (100*d_wt)) + (0.662183*T_air))))
}

#' Emissions_rates_soils
#' @param core.dat UI data
#' @param construct.dat UI construction data
#' @param AV_indirect Area/Volume of drained peat
#' @return Emissions rates from drained and undrained peatland
#' @export
Emissions_rates_soils <- function(core.dat,
                                  construct.dat,
                                  AV_indirect) {

  # THIS FUNCTION...
  CO2_C <- 3.667 # Molecular weight ratio C to CO2
  CH4_CO2 <- 30.66667 # CH4 to CO2 conversion factor

  # Extract inputs for easy access
  em_factor_meth_in <- core.dat$Em.factor.meth$em_factor_meth_in # Select IPCC default or ECOSSE model
  peat_type <- core.dat$Peatland$peat_type # Select acid bog or fen
  A_indirect <- AV_indirect$Total$a # Area peat drained
  V_indirect <- AV_indirect$Total$v # Volume peat drained
  T_air <- core.dat$Peatland$T_air # Average air temperature
  d_wt <- core.dat$Peatland$d_wt[c(1,3,2)] # Average water table depth pre-development/restoration, re-order Min/Max
  names(d_wt) <- c("Exp", "Min", "Max")

  if (em_factor_meth_in[1] == 1) { # IPCC default calculation used

    if (peat_type[1] == 1) { # Acid bog selected
      R_CH4_wet <- IPCC_CH4_AB(CH4_CO2)
    } else { # Fen selected
      R_CH4_wet <- IPCC_CH4_F(CH4_CO2)
    }

    R_CO2_dry <- IPCC_CO2() # Identical for both peat types
    R_CO2_wet <- list(R_CO2 = c(Exp = 0, Min = 0, Max = 0)) # Assumption
    R_CH4_dry <- list(R_CH4 = c(Exp = 0, Min = 0, Max = 0)) # Assumption

  } else { # Site specific calculation using meta-analytic regression method

    d_wt_drained <- apply(cbind(V_indirect/A_indirect,d_wt), MAR=1, FUN=max)[c(1,3,2)] # re-order Min/Max
    names(d_wt_drained) <- c("Exp", "Min", "Max")

    if (peat_type[1] == 1) { # Acid bog selected

      R_CO2_dry <- METAR_CO2_AB(CO2_C, d_wt_drained, T_air)
      R_CO2_wet <- METAR_CO2_AB(CO2_C, d_wt, T_air)

      R_CH4_dry <- METAR_CH4_AB(CH4_CO2, d_wt_drained, T_air)
      R_CH4_wet <- METAR_CH4_AB(CH4_CO2, d_wt, T_air)

    } else { # Fen selected

      R_CO2_dry <- METAR_CO2_F(CO2_C, d_wt_drained, T_air)
      R_CO2_wet <- METAR_CO2_F(CO2_C, d_wt, T_air)

      R_CH4_dry <- METAR_CH4_F(CH4_CO2, d_wt_drained, T_air)
      R_CH4_wet <- METAR_CH4_F(CH4_CO2, d_wt, T_air)

    }

  }

  R_tot <- list(R_CO2_dry=R_CO2_dry$R_CO2,
                R_CO2_wet=R_CO2_wet$R_CO2,
                R_CH4_dry=R_CH4_dry$R_CH4,
                R_CH4_wet=R_CH4_wet$R_CH4)

  return(R_tot)
}

#' Emissions_rates_soils
#' @param core.dat UI data
#' @param construct.dat UI construction data
#' @return Emissions rates from drained and undrained peatland
#' @export
Emissions_rates_soils_RM <- function(core.dat,
                                     construct.dat) {

  # THIS FUNCTION...
  CO2_C <- 3.667 # Molecular weight ratio C to CO2
  CH4_CO2 <- 30.66667 # CH4 to CO2 conversion factor

  # Extract inputs for easy access
  em_factor_meth_in <- core.dat$Em.factor.meth$em_factor_meth_in # Select IPCC default or ECOSSE model
  peat_type <- core.dat$Peatland$peat_type # Select acid bog or fen
  T_air <- core.dat$Peatland$T_air # Average air temperature

  d_wt_drained <- map(forestry.dat[grep("Area", names(forestry.dat))], .f = "d_wt_drained") # User estimated average water table depth pre-restoration
  d_wt_drained <- lapply(d_wt_drained, FUN = function(x) {
    x <- x[c(1,3,2)] # re-order min/max
    names(x) <- c("Exp", "Min", "Max")
    return(x)
  })

  d_wt_restored <- map(forestry.dat[grep("Area", names(forestry.dat))], .f = "d_wt_restored") # User estimated average water table depth post-restoration
  d_wt_restored <- lapply(d_wt_restored, FUN = function(x) {
    x <- x[c(1,3,2)] # re-order min/max
    names(x) <- c("Exp", "Min", "Max")
    return(x)
  })

  if (em_factor_meth_in[1] == 1) { # IPCC default calculation used

    R_CO2_wet <- vector(mode = "list", length = length(A_harv))
    R_CO2_dry <- vector(mode = "list", length = length(A_harv))

    R_CH4_wet <- vector(mode = "list", length = length(A_harv))
    R_CH4_dry <- vector(mode = "list", length = length(A_harv))

    names(R_CO2_wet) <- names(A_harv)
    names(R_CO2_dry) <- names(A_harv)
    names(R_CH4_wet) <- names(A_harv)
    names(R_CH4_dry) <- names(A_harv)

    for (i in 1:length(A_harv)) {
      if (peat_type[1] == 1) { # Acid bog selected
        R_CH4_wet[[i]] <- IPCC_CH4_AB(CH4_CO2)
      } else { # Fen selected
        R_CH4_wet[[i]] <- IPCC_CH4_F(CH4_CO2)
      }
      R_CO2_dry[[i]] <- IPCC_CO2() # Identical for both peat types
      R_CO2_wet[[i]] <- list(R_CO2 = c(Exp = 0, Min = 0, Max = 0)) # Assumption
      R_CH4_dry[[i]] <- list(R_CH4 = c(Exp = 0, Min = 0, Max = 0)) # Assumption
    }

  } else if (em_factor_meth_in[1] == 2) { # Site specific calculation using meta-analysic regression method

    if (peat_type[1] == 1) { # Acid bog selected

      R_CO2_dry <- lapply(d_wt_drained, FUN = function(x) {
        METAR_CO2_AB(CO2_C, x, T_air)
      })

      R_CO2_wet <- lapply(d_wt_restored, FUN = function(x) {
        METAR_CO2_AB(CO2_C, x, T_air)
      })

      R_CH4_dry <- lapply(d_wt_drained, FUN = function(x) {
        METAR_CH4_AB(CH4_CO2, x, T_air)
      })

      R_CH4_wet <- lapply(d_wt_restored, FUN = function(x) {
        METAR_CH4_AB(CH4_CO2, x, T_air)
      })

    } else { # Fen selected

      R_CO2_dry <- lapply(d_wt_drained, FUN = function(x) {
        METAR_CO2_F(CO2_C, x, T_air)
      })

      R_CO2_wet <- lapply(d_wt_restored, FUN = function(x) {
        METAR_CO2_F(CO2_C, x, T_air)
      })

      R_CH4_dry <- lapply(d_wt_drained, FUN = function(x) {
        METAR_CH4_F(CH4_CO2, x, T_air)
      })

      R_CH4_wet <- lapply(d_wt_restored, FUN = function(x) {
        METAR_CH4_F(CH4_CO2, x, T_air)
      })

    }

  } else if (em_factor_meth_in[1] == 3) { # Site specific calculation using ECOSSE regression method

    if (peat_type[1] == 1) { # Acid bog selected

      R_CO2_dry <- lapply(d_wt_drained, FUN = function(x) {
        ECOSSER_CO2_AB(CO2_C, x, T_air)
      })

      R_CO2_wet <- lapply(d_wt_restored, FUN = function(x) {
        ECOSSER_CO2_AB(CO2_C, x, T_air)
      })

      R_CH4_dry <- lapply(d_wt_drained, FUN = function(x) {
        ECOSSER_CH4_AB(CH4_CO2, x, T_air)
      })

      R_CH4_wet <- lapply(d_wt_restored, FUN = function(x) {
        ECOSSER_CH4_AB(CH4_CO2, x, T_air)
      })

    } else { # Fen selected

      R_CO2_dry <- lapply(d_wt_drained, FUN = function(x) {
        ECOSSER_CO2_F(CO2_C, x, T_air)
      })

      R_CO2_wet <- lapply(d_wt_restored, FUN = function(x) {
        ECOSSER_CO2_F(CO2_C, x, T_air)
      })

      R_CH4_dry <- lapply(d_wt_drained, FUN = function(x) {
        ECOSSER_CH4_F(CH4_CO2, x, T_air)
      })

      R_CH4_wet <- lapply(d_wt_restored, FUN = function(x) {
        ECOSSER_CH4_F(CH4_CO2, x, T_air)
      })

    }

  } else if (em_factor_meth_in[1] == 4) { # Site specific calculation using SCOTIA regression method

    if (peat_type[1] == 1) { # Acid bog selected

      R_CO2_dry <- lapply(d_wt_drained, FUN = function(x) {
        SCOTIAR_CO2_AB(CO2_C, x, T_air)
      })

      R_CO2_wet <- lapply(d_wt_restored, FUN = function(x) {
        SCOTIAR_CO2_AB(CO2_C, x, T_air)
      })

      R_CH4_dry <- lapply(d_wt_drained, FUN = function(x) {
        SCOTIAR_CH4_AB(CH4_CO2, x, T_air)
      })

      R_CH4_wet <- lapply(d_wt_restored, FUN = function(x) {
        SCOTIAR_CH4_AB(CH4_CO2, x, T_air)
      })

    } else { # Fen selected

      R_CO2_dry <- lapply(d_wt_drained, FUN = function(x) {
        SCOTIAR_CO2_F(CO2_C, x, T_air)
      })

      R_CO2_wet <- lapply(d_wt_restored, FUN = function(x) {
        SCOTIAR_CO2_F(CO2_C, x, T_air)
      })

      R_CH4_dry <- lapply(d_wt_drained, FUN = function(x) {
        SCOTIAR_CH4_F(CH4_CO2, x, T_air)
      })

      R_CH4_wet <- lapply(d_wt_restored, FUN = function(x) {
        SCOTIAR_CH4_F(CH4_CO2, x, T_air)
      })

    }

  }

  # Invert data structure
  R_tot <- vector(mode = "list", length = length(A_harv))
  names(R_tot) <- names(A_harv)
  for (i in 1:length(R_tot)) {
    R_tot[[i]]$R_CO2_dry <- R_CO2_dry[[i]]$R_CO2
    R_tot[[i]]$R_CO2_wet <- R_CO2_wet[[i]]$R_CO2
    R_tot[[i]]$R_CH4_dry <- R_CH4_dry[[i]]$R_CH4
    R_tot[[i]]$R_CH4_wet <- R_CH4_wet[[i]]$R_CH4
  }

  return(R_tot)
}
