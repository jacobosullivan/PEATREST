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

#' Bog_plant_restoration
#' @param core.dat UI data
#' @param AV_indirect Area of drained peat
#' @return L_fix
#' @export
Bog_plant_restoration <- function(core.dat,
                                  AV_indirect) {

  # THIS FUNCTION...

  CO2_C <- 3.667 # Molecular weight ratio C to CO2
  conv_val <- 0.999

  # Extract input variables for easy access
  G_bog <- core.dat$Bog.plants$G_bog
  t_restore <- core.dat$Bog.plants$t_restore[c(1,3,2)] # Reorder Min Max
  n_restore <- core.dat$Peatland.restoration$n_restore # NEW UI VARIABLE - shape parameter restoration of bog plant sequestration
  t_fallow <- core.dat$Peatland.restoration$t_fallow[c(1,3,2)] # NEW UI VARIABLE - time between felling and restoration; Reorder Min Max
  A_indirect <- AV_indirect$Total$a

  S_bog_sequ_rest_phase <- lapply(seq(G_bog),
                                  FUN = function(x) {
                                    rest_dyn_mod(t = 1:t_restore[x],
                                                 n = n_restore[x],
                                                 ymin = 0, # assume no bog plant sequestration prior to restoration intervention
                                                 ymax = CO2_C * (A_indirect[x] / 10000) * G_bog[x], # convert into units CO2
                                                 convThresh = conv_val)
                                  })


  # Add fallow period to time series
  S_bog_sequ_rest_phase <- lapply(seq(S_bog_sequ_rest_phase),
                                  FUN = function(x) c(rep(0, t_fallow[x]), S_bog_sequ_rest_phase[[x]]))

  # Extend time series to 100 years (if t_payback > 100, these will be extended again in the run script)
  S_bog_sequ_rest_phase <- lapply(seq(S_bog_sequ_rest_phase),
                                  FUN = function(x) {
                                    ext <- 101 - length(S_bog_sequ_rest_phase[[x]])
                                    return(c(S_bog_sequ_rest_phase[[x]], rep(unname(CO2_C * (A_indirect[x] / 10000) * G_bog[x]), ext)))
                                  })

  names(S_bog_sequ_rest_phase) <- c("Exp", "Min", "Max")

  if (0) {
    maxYear <- max(t_fallow) + max(t_restore) + 10
    par(mfrow=c(1,1))
    plot(x=seq(max(t_restore)),
         y=rep(max(unlist(S_bog_sequ_rest_phase)), max(t_restore)),
         xlim=c(0,maxYear),
         ylim=c(min(unlist(S_bog_sequ_rest_phase)),max(unlist(S_bog_sequ_rest_phase))),
         xlab="Time since restoration [yr]",
         ylab="Total CO2 sequestration [tCO2]",
         type='n')
    for (i in 1:length(S_bog_sequ_rest_phase)) {
      lines(x=0:100,
            y=S_bog_sequ_rest_phase[[i]])
      points(x=0,
             y=S_bog_sequ_rest_phase[[i]][1],
             col="black")
      points(x=t_fallow[i]-1,
             y=S_bog_sequ_rest_phase[[i]][t_fallow[i]],
             col="red")
      points(x=t_fallow[i]+t_restore[i]+1,
             y=S_bog_sequ_rest_phase[[i]][t_fallow[i]+t_restore[i]+1],
             col="blue")
    }
  }

  return(S_bog_sequ_rest_phase)
}
