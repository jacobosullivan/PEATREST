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

#' ECOSSR_CO2_AB
#' @param T_air Average air temperature
#' @param d_wt Average water table depth (undrained)
#' @param CO2_C Molecular weight ratio C to CO2
#' @return CO2 emissions rate ECOSSE Acid bog
#' @export
ECOSSR_CO2_AB <- function(CO2_C, d_wt, T_air) {
  return(list(R_CO2 = (CO2_C/1000) * ((6700 * exp(-0.26 * exp(-0.05153 * ((100*d_wt)-50)))) + ((72.54 * T_air) - 800))))
}

#' ECOSSR_CH4_AB
#' @param T_air Average air temperature
#' @param d_wt Average water table depth (undrained)
#' @param CH4_CO2 CH4 to CO2 conversion factor
#' @return CH4 emissions rate ECOSSE Acid bog
#' @export
ECOSSR_CH4_AB <- function(CH4_CO2, d_wt, T_air) { # converts into CO2 eq units
  return(list(R_CH4 = (CH4_CO2/1000) * (500 * exp(-0.1234 * (100*d_wt)) + ((3.529*T_air) - 36.67))))
}

#' ECOSSR_CO2_F
#' @param T_air Average air temperature
#' @param d_wt Average water table depth (undrained)
#' @param CO2_C Molecular weight ratio C to CO2
#' @return CO2 emissions rate ECOSSE Fen
#' @export
ECOSSR_CO2_F <- function(CO2_C, d_wt, T_air) {
  return(list(R_CO2 = (CO2_C/1000) * (16244 * exp(-0.17594 * exp(-0.07346 * ((d_wt*100)-50))) + (153.234*T_air))))
}

#' ECOSSR_CH4_F
#' @param T_air Average air temperature
#' @param d_wt Average water table depth (undrained)
#' @param CH4_CO2 CH4 to CO2 conversion factor
#' @return CO2 emissions rate ECOSSE Fen
#' @export
ECOSSR_CH4_F <- function(CH4_CO2, d_wt, T_air) { # converts into CO2 eq units
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
  d_wt <- core.dat$Peatland$d_wt[c(1,3,2)] # Average water table depth (undrained), re-order Min/Max
  names(d_wt) <- c("Exp", "Min", "Max")

  if (em_factor_meth_in[1] == 1) { # IPCC default calculation used

    if (peat_type[1] == 1) { # Acid bog selected
      R_CH4_undrained <- IPCC_CH4_AB(CH4_CO2)
    } else { # Fen selected
      R_CH4_undrained <- IPCC_CH4_F(CH4_CO2)
    }

    R_CO2_drained <- IPCC_CO2() # Identical for both peat types
    R_CO2_undrained <- list(R_CO2 = c(Exp = 0, Min = 0, Max = 0)) # Assumption
    R_CH4_drained <- list(R_CH4 = c(Exp = 0, Min = 0, Max = 0)) # Assumption

  } else { # Site specific calculation using ECOSSE method

    d_wt_drained <- apply(cbind(V_indirect/A_indirect,d_wt), MAR=1, FUN=max)[c(1,3,2)] # re-order Min/Max
    names(d_wt_drained) <- c("Exp", "Min", "Max")

    if (peat_type[1] == 1) { # Acid bog selected

      R_CO2_drained <- ECOSSR_CO2_AB(CO2_C, d_wt_drained, T_air)
      R_CO2_undrained <- ECOSSR_CO2_AB(CO2_C, d_wt, T_air)

      R_CH4_drained <- ECOSSR_CH4_AB(CH4_CO2, d_wt_drained, T_air)
      R_CH4_undrained <- ECOSSR_CH4_AB(CH4_CO2, d_wt, T_air)

    } else { # Fen selected

      R_CO2_drained <- ECOSSR_CO2_F(CO2_C, d_wt_drained, T_air)
      R_CO2_undrained <- ECOSSR_CO2_F(CO2_C, d_wt, T_air)

      R_CH4_drained <- ECOSSR_CH4_F(CH4_CO2, d_wt_drained, T_air)
      R_CH4_undrained <- ECOSSR_CH4_F(CH4_CO2, d_wt, T_air)

    }

  }

  R_tot <- list(R_CO2_drained=R_CO2_drained$R_CO2,
                R_CO2_undrained=R_CO2_undrained$R_CO2,
                R_CH4_drained=R_CH4_drained$R_CH4,
                R_CH4_undrained=R_CH4_undrained$R_CH4)

  return(R_tot)
}
