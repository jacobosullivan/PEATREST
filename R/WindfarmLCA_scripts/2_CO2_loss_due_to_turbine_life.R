## 2. CO2 loss due to turbine life

#' Lifetime_emissions
#' @param core.dat UI data
#' @param construct.dat UI construction data
#' @param E_cement emissions per unit volume cement
#' @return Windfarm lifetime emissions
#' @export
Lifetime_emissions <- function(core.dat,
                               construct.dat,
                               E_concrete=0.316) {

  ## This function computes the emissions associated with production, transportation, erection, operation and decomissioning of the windfarm
  ## either based on user inputs or regression coefficients

  if (core.dat$Windfarm$L_life_in[1] == 2) { # lifetime emissions calculated from linear regression coefficients

    # Extract input variables for easy access
    c_turb <- core.dat$Windfarm$c_turb
    n_turb <- core.dat$Windfarm$n_turb
    c_turb_thresh <- 1

    # Regression coefficients of L_life_turb ~ c_turb
    coefs <- data.frame(m=c(517.62,
                            934.35),
                        c=c(-0.1788,
                            -467.55))

    # Total volume cement used
    V_concrete <- Reduce("+", map(construct.dat, "V_concrete")) # sum elements between list elements

    # sapply call required in case c_turb includes values both above and below the threshold
    L_life_turb <- sapply(c_turb, FUN = function(x) {
      if (x <= c_turb_thresh) {
        return(coefs$m[1]*x+coefs$c[1])
      } else {
        return(coefs$m[2]*x+coefs$c[2])
      }
    })

    L_life_emissions <- n_turb * L_life_turb

    # Note apparent mis-match in naming as input variable called 'Volume of concrete used (m3)' used in assessment of cement emissions
    # #'concrete' used in variable naming
    L_concrete <- E_concrete * V_concrete

    L_life <- L_life_emissions + L_concrete

  } else {
    L_life <- core.dat$Windfarm$L_life * core.dat$Windfarm$n_turb * core.dat$Windfarm$c_turb
  }

  return(L_life)
}
