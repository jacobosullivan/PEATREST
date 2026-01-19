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

#' METAR21_CO2
#' @param d_wt Average water table depth (undrained)
#' @param CO2_C Molecular weight ratio C to CO2
#' @return CO2 emissions rate meta-analytic regression Acid bog
#' @export
METAR21_CO2 <- function(CO2_C, d_wt) {
  # Evans et al. 2021 https://doi.org/10.1038/s41586-021-03523-1
  return(list(R_CO2 = CO2_C * (0.1341 * (d_wt*100) - 1.73)))
}

#' METAR21_CH4
#' @param d_wt Average water table depth (undrained)
#' @param CH4_CO2 CH4 to CO2 conversion factor
#' @return CH4 emissions rate meta-analytic regression Acid bog
#' @export
METAR21_CH4 <- function(CO2_C, d_wt) {
  # Evans et al. 2021 https://doi.org/10.1038/s41586-021-03523-1
  return(list(R_CH4 = CO2_C * (0.334 * 0.5^(((d_wt*100) + 5) / 6.31))))
}

#' METAR97_CO2_AB
#' @param T_air Average air temperature
#' @param d_wt Average water table depth (undrained)
#' @param CO2_C Molecular weight ratio C to CO2
#' @return CO2 emissions rate meta-analytic regression Acid bog
#' @export
METAR97_CO2_AB <- function(CO2_C, d_wt, T_air) {
  return(list(R_CO2 = (CO2_C/1000) * ((6700 * exp(-0.26 * exp(-0.05153 * ((100*d_wt)-50)))) + ((72.54 * T_air) - 800))))
}

#' METAR97_CH4_AB
#' @param T_air Average air temperature
#' @param d_wt Average water table depth (undrained)
#' @param CH4_CO2 CH4 to CO2 conversion factor
#' @return CH4 emissions rate meta-analytic regression Acid bog
#' @export
METAR97_CH4_AB <- function(CH4_CO2, d_wt, T_air) { # converts into CO2 eq units
  return(list(R_CH4 = (CH4_CO2/1000) * (500 * exp(-0.1234 * (100*d_wt)) + ((3.529*T_air) - 36.67))))
}

#' METAR97_CO2_F
#' @param T_air Average air temperature
#' @param d_wt Average water table depth (undrained)
#' @param CO2_C Molecular weight ratio C to CO2
#' @return CO2 emissions rate meta-analytic regression Fen
#' @export
METAR97_CO2_F <- function(CO2_C, d_wt, T_air) {
  return(list(R_CO2 = (CO2_C/1000) * (16244 * exp(-0.17594 * exp(-0.07346 * ((d_wt*100)-50))) + (153.234*T_air))))
}

#' METAR97_CH4_F
#' @param T_air Average air temperature
#' @param d_wt Average water table depth (undrained)
#' @param CH4_CO2 CH4 to CO2 conversion factor
#' @return CO2 emissions rate meta-analytic regression Fen
#' @export
METAR97_CH4_F <- function(CH4_CO2, d_wt, T_air) { # converts into CO2 eq units
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

      if (0) {
        R_CO2_dry <- METAR97_CO2_AB(CO2_C, d_wt_drained, T_air)
        R_CO2_wet <- METAR97_CO2_AB(CO2_C, d_wt, T_air)

        R_CH4_dry <- METAR97_CH4_AB(CH4_CO2, d_wt_drained, T_air)
        R_CH4_wet <- METAR97_CH4_AB(CH4_CO2, d_wt, T_air)
      } else {
        R_CO2_dry <- METAR21_CO2(CO2_C, d_wt_drained)
        R_CO2_wet <- METAR21_CO2(CO2_C, d_wt)

        R_CH4_dry <- METAR21_CH4(CO2_C, d_wt_drained)
        R_CH4_wet <- METAR21_CH4(CO2_C, d_wt)
      }


    } else { # Fen selected

      if (0) {
        R_CO2_dry <- METAR97_CO2_F(CO2_C, d_wt_drained, T_air)
        R_CO2_wet <- METAR97_CO2_F(CO2_C, d_wt, T_air)

        R_CH4_dry <- METAR97_CH4_F(CH4_CO2, d_wt_drained, T_air)
        R_CH4_wet <- METAR97_CH4_F(CH4_CO2, d_wt, T_air)
      } else {
        R_CO2_dry <- METAR21_CO2(CO2_C, d_wt_drained)
        R_CO2_wet <- METAR21_CO2(CO2_C, d_wt)

        R_CH4_dry <- METAR21_CH4(CO2_C, d_wt_drained)
        R_CH4_wet <- METAR21_CH4(CO2_C, d_wt)
      }

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
#' @param forestry.dat UI forestry data
#' @return Emissions rates from drained and undrained peatland
#' @export
Emissions_rates_soils_RM <- function(core.dat,
                                     forestry.dat) {

  # THIS FUNCTION...
  CO2_C <- 3.667 # Molecular weight ratio C to CO2
  CH4_CO2 <- 30.66667 # CH4 to CO2 conversion factor

  # Extract inputs for easy access
  em_factor_meth_in <- core.dat$Em.factor.meth$em_factor_meth_in # Select IPCC default or ECOSSE model
  peat_type <- core.dat$Peatland$peat_type # Select acid bog or fen
  T_air <- core.dat$Peatland$T_air # Average air temperature

  # JDebug: This may need to be replaced with d_peat / maxrootdepth inputs for forestry WTD
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

    R_CO2_wet <- vector(mode = "list", length = length(d_wt_drained))
    R_CO2_dry <- vector(mode = "list", length = length(d_wt_drained))

    R_CH4_wet <- vector(mode = "list", length = length(d_wt_drained))
    R_CH4_dry <- vector(mode = "list", length = length(d_wt_drained))

    names(R_CO2_wet) <- names(d_wt_drained)
    names(R_CO2_dry) <- names(d_wt_drained)
    names(R_CH4_wet) <- names(d_wt_drained)
    names(R_CH4_dry) <- names(d_wt_drained)

    for (i in 1:length(d_wt_drained)) {
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

      if (0) {
        R_CO2_dry <- lapply(d_wt_drained, FUN = function(x) {
          METAR97_CO2_AB(CO2_C, x, T_air)
        })

        R_CO2_wet <- lapply(d_wt_restored, FUN = function(x) {
          METAR97_CO2_AB(CO2_C, x, T_air)
        })

        R_CH4_dry <- lapply(d_wt_drained, FUN = function(x) {
          METAR97_CH4_AB(CH4_CO2, x, T_air)
        })

        R_CH4_wet <- lapply(d_wt_restored, FUN = function(x) {
          METAR97_CH4(CH4_CO2, x, T_air)
        })
      } else {
        R_CO2_dry <- lapply(d_wt_drained, FUN = function(x) {
          METAR21_CO2(CO2_C, x)
        })

        R_CO2_wet <- lapply(d_wt_restored, FUN = function(x) {
          METAR21_CO2(CO2_C, x)
        })

        R_CH4_dry <- lapply(d_wt_drained, FUN = function(x) {
          METAR21_CH4(CO2_C, x)
        })

        R_CH4_wet <- lapply(d_wt_restored, FUN = function(x) {
          METAR21_CH4(CO2_C, x)
        })
      }

    } else { # Fen selected

      if (0) {
        R_CO2_dry <- lapply(d_wt_drained, FUN = function(x) {
          METAR97_CO2_F(CO2_C, x, T_air)
        })

        R_CO2_wet <- lapply(d_wt_restored, FUN = function(x) {
          METAR97_CO2_F(CO2_C, x, T_air)
        })

        R_CH4_dry <- lapply(d_wt_drained, FUN = function(x) {
          METAR97_CH4_F(CH4_CO2, x, T_air)
        })

        R_CH4_wet <- lapply(d_wt_restored, FUN = function(x) {
          METAR97_CH4_F(CH4_CO2, x, T_air)
        })
      } else {
        R_CO2_dry <- lapply(d_wt_drained, FUN = function(x) {
          METAR21_CO2(CO2_C, x, T_air)
        })

        R_CO2_wet <- lapply(d_wt_restored, FUN = function(x) {
          METAR21_CO2(CO2_C, x, T_air)
        })

        R_CH4_dry <- lapply(d_wt_drained, FUN = function(x) {
          METAR21_CH4(CO2_C, x, T_air)
        })

        R_CH4_wet <- lapply(d_wt_restored, FUN = function(x) {
          METAR21_CH4(CO2_C, x, T_air)
        })
      }

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
  R_tot <- vector(mode = "list", length = length(d_wt_drained))
  names(R_tot) <- names(d_wt_drained)
  for (i in 1:length(R_tot)) {
    R_tot[[i]]$R_CO2_dry <- R_CO2_dry[[i]]$R_CO2
    R_tot[[i]]$R_CO2_wet <- R_CO2_wet[[i]]$R_CO2
    R_tot[[i]]$R_CH4_dry <- R_CH4_dry[[i]]$R_CH4
    R_tot[[i]]$R_CH4_wet <- R_CH4_wet[[i]]$R_CH4
  }

  return(R_tot)
}

#' Emissions_rates_forestry_soils_RM
#' @param core.dat UI data
#' @param forestry.dat UI forestry data
#' @param growthYield.dat growth and yield data (estimated from CARBINE runs)
#' @return Emissions rates from drained soils under trees
#' @export
Emissions_rates_forestry_soils_RM <- function(core.dat,
                                              forestry.dat,
                                              growthYield.dat) {

  # THIS FUNCTION...
  CO2_C <- 3.667 # Molecular weight ratio C to CO2
  CH4_CO2 <- 30.66667 # CH4 to CO2 conversion factor

  # Volume explored by 1kg of root biomass set to a preliminary value that keeps d_wt <1m as observed
  # Need to find an actual estimate for this or tune to fit some data
  # DO NOT set lower that 0.1 as it leads to fitting errors in the nls.
  # Increasing increases the rate of the logistic increase in root depth to maximum but obviously not the asymptote so outcomes are not very sensitive to this value
  sigma_zR <- c(Scots_pine = 0.2,
                Sitka_spruce = 0.1) * 1000 # m3/kg Volume explored by 1kg of root biomass (taken from FR 3PG pars), converted to m3/t

  # Extract inputs for easy access
  em_factor_meth_in <- core.dat$Em.factor.meth$em_factor_meth_in # Select IPCC default or ECOSSE model
  peat_type <- core.dat$Peatland$peat_type # Select acid bog or fen
  d_peat <- map(forestry.dat[grep("Area", names(forestry.dat))], .f = "d_peat")
  T_air <- core.dat$Peatland$T_air # Average air temperature
  Spp <- map(forestry.dat[grep("Area", names(forestry.dat))], .f = "species")
  t_harv <- map(forestry.dat[grep("Area", names(forestry.dat))], .f = "t_harv")
  A_harv <- map(forestry.dat[grep("Area", names(forestry.dat))], .f = "A_harv")
  YC <- map(forestry.dat[grep("Area", names(forestry.dat))], .f = "YC") # if not passed by user, already computed elsewhere from Growth and yield tables
  species <- c("Scots_pine", "Sitka_spruce")

  # Get rooting depths for water table estimates
  d_wt <- lapply(seq_along(YC), FUN = function(x) {
    d_wt_a <- lapply(seq_along(YC[[x]]), FUN = function(y) {
      Spp_a <- species[Spp[[x]][1]]
      YC_a0 <- YC[[x]][y]
      t_harv_a <- t_harv[[x]][y]

      ## Deal with missing YC values from GY table
      YC_avail <- unlist(growthYield.dat %>% filter(Spp == Spp_a) %>% select(YC) %>% unique())

      ### If YC_a is not available, set to closest value.
      ### If equidistant from multiple available values, maximum is used (conservative estimate of payback time)
      YC_a <- max(YC_avail[which(abs(YC_a0 - YC_avail) == min(abs(YC_a0 - unlist(YC_avail))))])

      res <- growthYield.dat %>%
        filter(Spp == Spp_a,
               YC == YC_a) %>%
        select(Age, B_r) %>%
        mutate(V_r = B_r * sigma_zR[Spp_a],
               d_wt = V_r/10000,
               YC = YC_a0,
               YC_avail = YC_a) #%>%
        # mutate(d_wt = ifelse(d_wt > d_peat[[x]][y], d_peat[[x]][y], d_wt))

      res <- rbind(data.frame(Age = 0,
                              B_r = 0,
                              V_r = 0,
                              d_wt = 0,
                              YC = YC_a0,
                              YC_avail = YC_a),
                   res)

      res$Area <- names(YC)[x]

      return(res)
    })

    d_wt_a <- bind_rows(d_wt_a)
    return(d_wt_a)
  })

  names(d_wt) <- names(YC)

  d_wt_pred <- lapply(seq_along(YC), FUN = function(x) {
    d_wt_pred_a <- lapply(seq_along(unique(d_wt[[x]]$YC)), FUN = function(y) {

      YC_a <- unique(d_wt[[x]]$YC)[y]

      fit <- nls(
        d_wt ~ d_peat[[x]][y] / (1 + exp(-k * (Age - x0))),
        data = d_wt[[x]] %>% filter(YC==YC_a),
        start = list(k = 1, # growth rate
                     x0 = 50), # midpoint
        control = nls.control(maxiter = 100, warnOnly = TRUE)
      )

      Age_pred <- 0:(500+t_harv[[x]][y])
      pred <- data.frame(Age = Age_pred)
      pred$Area = names(YC)[x]
      pred$YC = YC[[x]][y]
      pred$YC_avail = unique(d_wt[[x]]$YC)[y]
      # pred$YC = unique(d_wt[[x]]$YC)[y]
      pred$d_wt <- predict(fit, newdata = pred)

      if (0) {
        ggplot(d_wt[[x]] %>% filter(YC==YC_a),
               aes(x=Age, y=d_wt)) +
          geom_point() +
          geom_line(data=pred)
      }

      # return(df)
      return(pred)
    })
    names(d_wt_pred_a) <- names(YC[[x]])
    return(d_wt_pred_a)
  })
  names(d_wt_pred) <- names(YC)

  if (0) {
    p <- ggplot(bind_rows(d_wt) %>% filter(Area == "Area.2", Age %% 3 == 0),
           aes(x=Age, y=d_wt, col=factor(YC))) +
      geom_point() +
      scale_x_continuous(limits=c(NA, 200)) +
      geom_line(data=bind_rows(lapply(d_wt_pred, FUN=bind_rows)) %>% filter(Area == "Area.2")) +
      # facet_wrap(~ Area) +
      theme_bw() +
      labs(x = "Stand age (yr)", y = "[Root / Water table] depth (m)", col="YC")

    ww <- 11*0.8
    hh <- 8*0.8
    png("../Figures/root_depth.png",
        width=ww, height=hh, units="cm", res=300)
    p
    dev.off()

  }

  if (em_factor_meth_in[1] == 1) { # IPCC default calculation used

    R_CO2_dry <- vector(mode = "list", length = length(d_wt_pred))
    R_CH4_dry <- vector(mode = "list", length = length(d_wt_pred))

    names(R_CO2_dry) <- names(d_wt)
    names(R_CH4_dry) <- names(d_wt)

    for (i in 1:length(d_wt)) {
      R_CO2 <- IPCC_CO2() # Identical for both peat types
      R_CO2_dry[[i]] <- list(Exp = data.frame(Age = d_wt_pred[[i]]$Exp$Age,
                                              R_CH4 = unname(R_CO2$R_CO2[1])),
                             Min = data.frame(Age = d_wt_pred[[i]]$Min$Age,
                                              R_CH4 = unname(R_CO2$R_CO2[2])),
                             Max = data.frame(Age = d_wt_pred[[i]]$Max$Age,
                                              R_CH4 = unname(R_CO2$R_CO2[3])))
      R_CH4_dry[[i]] <- list(R_CH4 = c(Exp = 0, Min = 0, Max = 0)) # Assumption
    }

  } else if (em_factor_meth_in[1] == 2) { # Site specific calculation using meta-analysic regression method

    if (peat_type[1] == 1) { # Acid bog selected

      R_CO2_dry <- lapply(seq_along(d_wt_pred), FUN = function(x) {
        R_CO2_dry_a <- lapply(seq_along(d_wt_pred[[x]]), FUN = function(y) {
          if (0) {
            res <- METAR97_CO2_AB(CO2_C, d_wt_pred[[x]][[y]]$d_wt, T_air[y])
          } else {
            res <- METAR21_CO2(CO2_C, d_wt_pred[[x]][[y]]$d_wt)
          }

          df <- data.frame(t = d_wt_pred[[x]][[y]]$Age-t_harv[[x]][y],
                           d_wt = d_wt_pred[[x]][[y]]$d_wt,
                           R_CO2 = unname(res$R_CO2))
          df$T_air <- unname(T_air[y])
          df$Est <- names(d_wt_pred[[x]])[y]
          df$Area <- names(d_wt_pred)[x]
          return(df)
        })

        names(R_CO2_dry_a) <- names(d_wt_pred[[x]])

        return(R_CO2_dry_a)
      })
      names(R_CO2_dry) <- names(d_wt_pred)

      R_CH4_dry <- lapply(seq_along(d_wt_pred), FUN = function(x) {
        R_CH4_dry_a <- lapply(seq_along(d_wt_pred[[x]]), FUN = function(y) {
          if (0) {
            res <- METAR97_CH4_AB(CH4_CO2, d_wt_pred[[x]][[y]]$d_wt, T_air[y])
          } else {
            res <- METAR21_CH4(CO2_C, d_wt_pred[[x]][[y]]$d_wt)
          }

          df <- data.frame(t = d_wt_pred[[x]][[y]]$Age-t_harv[[x]][y],
                           d_wt = d_wt_pred[[x]][[y]]$d_wt,
                           R_CH4 = unname(res$R_CH4))
          df$T_air <- unname(T_air[y])
          df$Est <- names(d_wt_pred[[x]])[y]
          df$Area <- names(d_wt_pred)[x]
          return(df)
        })
        names(R_CH4_dry_a) <- names(d_wt_pred[[x]])

        return(R_CH4_dry_a)
      })
      names(R_CH4_dry) <- names(d_wt_pred)

    } else if (peat_type[2] == 1) { # Fen selected

      R_CO2_dry <- lapply(seq_along(d_wt_pred), FUN = function(x) {
        R_CO2_dry_a <- lapply(seq_along(d_wt_pred[[x]]), FUN = function(y) {
          if (0) {
            res <- METAR97_CO2_F(CO2_C, d_wt_pred[[x]][[y]]$d_wt, T_air[y])
          } else {
            res <- METAR21_CO2(CO2_C, d_wt_pred[[x]][[y]]$d_wt)
          }

          df <- data.frame(t = d_wt_pred[[x]][[y]]$Age-t_harv[[x]][y],
                           d_wt = d_wt_pred[[x]][[y]]$d_wt,
                           R_CO2 = unname(res$R_CO2))
          df$T_air <- unname(T_air[y])
          df$Est <- names(d_wt_pred[[x]])[y]
          df$Area <- names(d_wt_pred)[x]
          return(df)
        })

        names(R_CO2_dry_a) <- names(d_wt_pred[[x]])

        return(R_CO2_dry_a)
      })
      names(R_CO2_dry) <- names(d_wt_pred)

      R_CH4_dry <- lapply(seq_along(d_wt_pred), FUN = function(x) {
        R_CH4_dry_a <- lapply(seq_along(d_wt_pred[[x]]), FUN = function(y) {
          if (0) {
            res <- METAR97_CH4_F(CH4_CO2, d_wt_pred[[x]][[y]]$d_wt, T_air[y])
          } else {
            res <- METAR21_CH4(CO2_C, d_wt_pred[[x]][[y]]$d_wt)
          }

          df <- data.frame(t = d_wt_pred[[x]][[y]]$Age-t_harv[[x]][y],
                           d_wt = d_wt_pred[[x]][[y]]$d_wt,
                           R_CH4 = unname(res$R_CH4))
          df$T_air <- unname(T_air[y])
          df$Est <- names(d_wt_pred[[x]])[y]
          df$Area <- names(d_wt_pred)[x]
          return(df)
        })
        names(R_CH4_dry_a) <- names(d_wt_pred[[x]])

        return(R_CH4_dry_a)
      })
      names(R_CH4_dry) <- names(d_wt_pred)

    }

  } else if (em_factor_meth_in[1] == 3) { # Site specific calculation using ECOSSE regression method

    if (peat_type[1] == 1) { # Acid bog selected

      R_CO2_dry <- lapply(seq_along(d_wt_pred), FUN = function(x) {
        R_CO2_dry_a <- lapply(seq_along(d_wt_pred[[x]]), FUN = function(y) {
          res <- ECOSSER_CO2_AB(CO2_C, d_wt_pred[[x]][[y]]$d_wt, T_air[y])
          df <- data.frame(t = d_wt_pred[[x]][[y]]$Age-t_harv[[x]][y],
                           d_wt = d_wt_pred[[x]][[y]]$d_wt,
                           R_CO2 = unname(res$R_CO2))
          df$T_air <- unname(T_air[y])
          df$Est <- names(d_wt_pred[[x]])[y]
          df$Area <- names(d_wt_pred)[x]
          return(df)
        })

        names(R_CO2_dry_a) <- names(d_wt_pred[[x]])

        return(R_CO2_dry_a)
      })
      names(R_CO2_dry) <- names(d_wt_pred)

      R_CH4_dry <- lapply(seq_along(d_wt_pred), FUN = function(x) {
        R_CH4_dry_a <- lapply(seq_along(d_wt_pred[[x]]), FUN = function(y) {
          res <- ECOSSER_CH4_AB(CH4_CO2, d_wt_pred[[x]][[y]]$d_wt, T_air[y])
          df <- data.frame(t = d_wt_pred[[x]][[y]]$Age-t_harv[[x]][y],
                           d_wt = d_wt_pred[[x]][[y]]$d_wt,
                           R_CH4 = unname(res$R_CH4))
          df$T_air <- unname(T_air[y])
          df$Est <- names(d_wt_pred[[x]])[y]
          df$Area <- names(d_wt_pred)[x]
          return(df)
        })
        names(R_CH4_dry_a) <- names(d_wt_pred[[x]])

        return(R_CH4_dry_a)
      })
      names(R_CH4_dry) <- names(d_wt_pred)

    } else if (peat_type[2] == 1) { # Fen selected

      R_CO2_dry <- lapply(seq_along(d_wt_pred), FUN = function(x) {
        R_CO2_dry_a <- lapply(seq_along(d_wt_pred[[x]]), FUN = function(y) {
          res <- ECOSSER_CO2_F(CO2_C, d_wt_pred[[x]][[y]]$d_wt, T_air[y])
          df <- data.frame(t = d_wt_pred[[x]][[y]]$Age-t_harv[[x]][y],
                           d_wt = d_wt_pred[[x]][[y]]$d_wt,
                           R_CO2 = unname(res$R_CO2))
          df$T_air <- unname(T_air[y])
          df$Est <- names(d_wt_pred[[x]])[y]
          df$Area <- names(d_wt_pred)[x]
          return(df)
        })

        names(R_CO2_dry_a) <- names(d_wt_pred[[x]])

        return(R_CO2_dry_a)
      })
      names(R_CO2_dry) <- names(d_wt_pred)

      R_CH4_dry <- lapply(seq_along(d_wt_pred), FUN = function(x) {
        R_CH4_dry_a <- lapply(seq_along(d_wt_pred[[x]]), FUN = function(y) {
          res <- ECOSSER_CH4_F(CH4_CO2, d_wt_pred[[x]][[y]]$d_wt, T_air[y])
          df <- data.frame(t = d_wt_pred[[x]][[y]]$Age-t_harv[[x]][y],
                           d_wt = d_wt_pred[[x]][[y]]$d_wt,
                           R_CH4 = unname(res$R_CH4))
          df$T_air <- unname(T_air[y])
          df$Est <- names(d_wt_pred[[x]])[y]
          df$Area <- names(d_wt_pred)[x]
          return(df)
        })
        names(R_CH4_dry_a) <- names(d_wt_pred[[x]])

        return(R_CH4_dry_a)
      })
      names(R_CH4_dry) <- names(d_wt_pred)

    }

  } else if (em_factor_meth_in[1] == 4) { # Site specific calculation using SCOTIA regression method

    if (peat_type[1] == 1) { # Acid bog selected

      R_CO2_dry <- lapply(seq_along(d_wt_pred), FUN = function(x) {
        R_CO2_dry_a <- lapply(seq_along(d_wt_pred[[x]]), FUN = function(y) {
          res <- SCOTIAR_CO2_AB(CO2_C, d_wt_pred[[x]][[y]]$d_wt, T_air[y])
          df <- data.frame(t = d_wt_pred[[x]][[y]]$Age-t_harv[[x]][y],
                           d_wt = d_wt_pred[[x]][[y]]$d_wt,
                           R_CO2 = unname(res$R_CO2))
          df$T_air <- unname(T_air[y])
          df$Est <- names(d_wt_pred[[x]])[y]
          df$Area <- names(d_wt_pred)[x]
          return(df)
        })

        names(R_CO2_dry_a) <- names(d_wt_pred[[x]])

        return(R_CO2_dry_a)
      })
      names(R_CO2_dry) <- names(d_wt_pred)

      R_CH4_dry <- lapply(seq_along(d_wt_pred), FUN = function(x) {
        R_CH4_dry_a <- lapply(seq_along(d_wt_pred[[x]]), FUN = function(y) {
          res <- SCOTIAR_CH4_AB(CH4_CO2, d_wt_pred[[x]][[y]]$d_wt, T_air[y])
          df <- data.frame(t = d_wt_pred[[x]][[y]]$Age-t_harv[[x]][y],
                           d_wt = d_wt_pred[[x]][[y]]$d_wt,
                           R_CH4 = unname(res$R_CH4))
          df$T_air <- unname(T_air[y])
          df$Est <- names(d_wt_pred[[x]])[y]
          df$Area <- names(d_wt_pred)[x]
          return(df)
        })
        names(R_CH4_dry_a) <- names(d_wt_pred[[x]])

        return(R_CH4_dry_a)
      })
      names(R_CH4_dry) <- names(d_wt_pred)

    } else if (peat_type[2] == 1) { # Fen selected

      R_CO2_dry <- lapply(seq_along(d_wt_pred), FUN = function(x) {
        R_CO2_dry_a <- lapply(seq_along(d_wt_pred[[x]]), FUN = function(y) {
          res <- SCOTIAR_CO2_F(CO2_C, d_wt_pred[[x]][[y]]$d_wt, T_air[y])
          df <- data.frame(t = d_wt_pred[[x]][[y]]$Age-t_harv[[x]][y],
                           d_wt = d_wt_pred[[x]][[y]]$d_wt,
                           R_CO2 = unname(res$R_CO2))
          df$T_air <- unname(T_air[y])
          df$Est <- names(d_wt_pred[[x]])[y]
          df$Area <- names(d_wt_pred)[x]
          return(df)
        })

        names(R_CO2_dry_a) <- names(d_wt_pred[[x]])

        return(R_CO2_dry_a)
      })
      names(R_CO2_dry) <- names(d_wt_pred)

      R_CH4_dry <- lapply(seq_along(d_wt_pred), FUN = function(x) {
        R_CH4_dry_a <- lapply(seq_along(d_wt_pred[[x]]), FUN = function(y) {
          res <- SCOTIAR_CH4_F(CH4_CO2, d_wt_pred[[x]][[y]]$d_wt, T_air[y])
          df <- data.frame(t = d_wt_pred[[x]][[y]]$Age-t_harv[[x]][y],
                           d_wt = d_wt_pred[[x]][[y]]$d_wt,
                           R_CH4 = unname(res$R_CH4))
          df$T_air <- unname(T_air[y])
          df$Est <- names(d_wt_pred[[x]])[y]
          df$Area <- names(d_wt_pred)[x]
          return(df)
        })
        names(R_CH4_dry_a) <- names(d_wt_pred[[x]])

        return(R_CH4_dry_a)
      })
      names(R_CH4_dry) <- names(d_wt_pred)

    }

  }

  if (0) {
    ggplot(R_CO2_dry$Area.1$Max, aes(x=t)) +
      geom_line(aes(y=R_CO2)) +
      geom_line(aes(y=d_wt))
  }

  # Invert data structure
  L_tot <- vector(mode = "list", length = length(d_wt))
  names(L_tot) <- names(YC)

  for (i in 1:length(R_tot)) {
    L_tot[[i]] <- list(Exp = left_join(R_CO2_dry[[i]]$Exp,
                                       R_CH4_dry[[i]]$Exp,
                                       by = c("t", "d_wt", "T_air", "Est", "Area")) %>%
                         mutate(R_tot = R_CO2 + R_CH4) %>%
                         mutate(L_tot = R_tot * A_harv[[i]][1],
                                L_CO2 = R_CO2 * A_harv[[i]][1],
                                L_CH4 = R_CH4 * A_harv[[i]][1]) %>%
                         select(-c(R_tot, R_CO2, R_CH4)) %>%
                         pivot_longer(cols = c(L_CO2, L_CH4, L_tot), names_to = "source", values_to = "value"),
                       Min = left_join(R_CO2_dry[[i]]$Min,
                                       R_CH4_dry[[i]]$Min,
                                       by = c("t", "d_wt", "T_air", "Est", "Area")) %>%
                         mutate(R_tot = R_CO2 + R_CH4) %>%
                           mutate(L_tot = R_tot * A_harv[[i]][2],
                                  L_CO2 = R_CO2 * A_harv[[i]][2],
                                  L_CH4 = R_CH4 * A_harv[[i]][2]) %>%
                           select(-c(R_tot, R_CO2, R_CH4)) %>%
                         pivot_longer(cols = c(L_CO2, L_CH4, L_tot), names_to = "source", values_to = "value"),
                       Max = left_join(R_CO2_dry[[i]]$Max,
                                       R_CH4_dry[[i]]$Max,
                                       by = c("t", "d_wt", "T_air", "Est", "Area")) %>%
                         mutate(R_tot = R_CO2 + R_CH4) %>%
                           mutate(L_tot = R_tot * A_harv[[i]][3],
                                  L_CO2 = R_CO2 * A_harv[[i]][3],
                                  L_CH4 = R_CH4 * A_harv[[i]][3]) %>%
                           select(-c(R_tot, R_CO2, R_CH4)) %>%
                         pivot_longer(cols = c(L_CO2, L_CH4, L_tot), names_to = "source", values_to = "value"))
  }

  return(L_tot)
  # return(L_tot = list(R_CO2 = R_CO2_dry,
                      # R_CH4 = R_CH4_dry))
}

if (0) {
  ## 1997 meta-analytic regression model
  d_wt <- seq(0, 1, length.out=101)
  TT <- seq(0, 10, length.out=5)
  CO2_C <- 3.667 # Molecular weight ratio C to CO2
  CH4_CO2 <- 30.66667 # CH4 to CO2 conversion factor
  R_df <- c()

  for (i in 1:length(d_wt)) {
    for (j in 1:length(TT)) {
      R_df <- rbind(R_df,
                    data.frame(d_wt = d_wt[i],
                               TT = TT[j],
                               source = "CO2",
                               R = unlist(METAR97_CO2_F(CO2_C,
                                                      d_wt[i],
                                                      TT[j])),
                               peat_type = "F"),
                    data.frame(d_wt = d_wt[i],
                               TT = TT[j],
                               source = "CO2",
                               R = unlist(METAR97_CO2_AB(CO2_C,
                                                       d_wt[i],
                                                       TT[j])),
                               peat_type = "AB"),
                    data.frame(d_wt = d_wt[i],
                               TT = TT[j],
                               source = "CH4",
                               R = unlist(METAR97_CH4_F(CH4_CO2,
                                                      d_wt[i],
                                                      TT[j])),
                               peat_type = "F"),
                    data.frame(d_wt = d_wt[i],
                               TT = TT[j],
                               source = "CH4",
                               R = unlist(METAR97_CH4_AB(CH4_CO2,
                                                       d_wt[i],
                                                       TT[j])),
                               peat_type = "AB"))
    }
  }

  ggplot(R_df %>%
           mutate(source = factor(source, levels = c("CO2", "CH4"))) %>%
           filter(d_wt <= 1, source == "CH4"),
         aes(x=d_wt, y=R, col=TT, group_by=factor(TT), linetype=factor(source))) +
    geom_line() +
    facet_wrap(~ peat_type, scales="free_y") +
    theme_bw() +
    labs(x="Water table depth [m]", y="Emissions rate [t CO2 eq. ha-1 yr-1]", linetype="", col="T [C]")


  p0 <- ggplot(R_df %>%
                 mutate(source = factor(source, levels = c("CO2", "CH4"))) %>%
                 filter(d_wt <= 1),
               aes(x=d_wt, y=R, col=TT, group_by=factor(TT), linetype=factor(source))) +
    geom_line() +
    facet_wrap(~ peat_type, scales="free_y") +
    theme_bw() +
    labs(x="Water table depth [m]", y="Emissions rate [t CO2 eq. ha-1 yr-1]", linetype="", col="T [C]")

  ww <- 18
  hh <- 8
  png("../Figures/meta_analytic_emissions_rates.png", width=ww, height=hh, units="cm", res=300)
  p0
  dev.off()
}

if (0) {
  ## 2021 meta-analytic regression model
  d_wt <- seq(0, 1, length.out=101)
  CO2_C <- 3.667 # Molecular weight ratio C to CO2
  R_df <- c()

  for (i in 1:length(d_wt)) {
    R_df <- rbind(R_df,
                  data.frame(d_wt = d_wt[i],
                             source = "CO2",
                             R = unlist(METAR21_CO2(CO2_C, d_wt[i]))),
                  data.frame(d_wt = d_wt[i],
                             source = "CH4",
                             R = unlist(METAR21_CH4(CO2_C, d_wt[i]))))

  }

  p0 <- ggplot(R_df %>%
                 mutate(source = factor(source, levels = c("CO2", "CH4"))) %>%
                 filter(d_wt <= 1),
               aes(x=d_wt, y=R)) +
    geom_line() +
    facet_wrap(~ source, scales="free_y") +
    theme_bw() +
    labs(x="Water table depth [m]", y="Emissions rate [t CO2 eq. ha-1 yr-1]", linetype="", col="T [C]")

  ww <- 16
  hh <- 8
  png("../Figures/meta_analytic_emissions_rates_Evans_2021.png", width=ww, height=hh, units="cm", res=300)
  p0
  dev.off()
}
