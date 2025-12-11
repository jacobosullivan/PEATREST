## 5f. CO2 loss from restored peatland

#' CO2_loss_restoration
#' @param core.dat UI data
#' @param AV_indirect Area/Volume of drained peat
#' @param R_tot estimated emissions rates
#' @return L_indirect
#' @export
CO2_loss_restoration <- function(core.dat, AV_indirect, R_tot) {

  ## This function will estimate the emissions from the site following harvesting and restoration interventions
  ## assuming a non-linear restoration of ecosystem function parameterised by the user

  CO2_C <- 3.667 # Molecular weight ratio C to CO2
  conv_val <- 0.99 # convergence value required by arbitrary convergent function

  # Extract input variables for easy access
  peat_type <- core.dat$Peatland$peat_type # may not be required, depends if ECOSSE can resolve peat type
  A_indirect <- AV_indirect$Total$a
  t_fallow <- core.dat$Peatland.restoration$t_fallow # NEW UI VARIABLE - time between felling and restoration
  t_restore_microbes <- core.dat$Peatland.restoration$t_restore_microbes # NEW UI VARIABLE - time to restoration of microbial function
  n_restore_microbes <- core.dat$Peatland.restoration$n_restore_microbes # NEW UI VARIABLE - shape parameter for restoration of microbial function

  ## Compute emissions rates from deforested, unrestored peatland
  D_f <- 0 # Assume D_f = 0 for drained peats
  pD_f <- D_f / 365

  CO2_drained <- (A_indirect / 10000) * ((R_tot$R_CO2_drained * pD_f) + (R_tot$R_CO2_drained * (1 - pD_f)))
  CH4_drained <- (A_indirect / 10000) * ((R_tot$R_CH4_drained * pD_f) + (R_tot$R_CH4_drained * (1 - pD_f)))

  ## Compute emissions rates from deforested, restored peatland

  if (peat_type[1] == 1) { # Acid bog selected
    D_f <- 178
  } else {
    D_f <- 169
  }
  pD_f <- D_f / 365

  CO2_restored <- (A_indirect / 10000) * ((R_tot$R_CO2_undrained * pD_f) + (R_tot$R_CO2_undrained * (1 - pD_f)))
  CH4_restored <- (A_indirect / 10000) * ((R_tot$R_CH4_undrained * pD_f) + (R_tot$R_CH4_undrained * (1 - pD_f)))

  ## Interpolate emissions across restoration phase
  L_CO2_rest_phase <- lapply(seq(CO2_drained),
                             FUN = function(x) {
                               rest_dyn_mod(t = 1:t_restore_microbes[x],
                                            n = n_restore_microbes[x],
                                            ymin = CO2_drained[x],
                                            ymax = CO2_restored[x],
                                            convThresh = conv_val)
                             })

  L_CH4_rest_phase <- lapply(seq(CO2_drained),
                             FUN = function(x) {
                               rest_dyn_mod(t = 1:t_restore_microbes[x],
                                            n = n_restore_microbes[x],
                                            ymin = CH4_drained[x],
                                            ymax = CH4_restored[x],
                                            convThresh = conv_val)
                             })

  # Add fallow period to time series
  L_CO2_rest_phase <- lapply(seq(L_CO2_rest_phase),
                             FUN = function(x) c(rep(unname(CO2_drained[x]), t_fallow[x]), L_CO2_rest_phase[[x]]))

  L_CH4_rest_phase <- lapply(seq(L_CH4_rest_phase),
                             FUN = function(x) c(rep(unname(CH4_drained[x]), t_fallow[x]), L_CH4_rest_phase[[x]]))

  # Extend time series to 100 years (if t_payback > 100, these will be extended again in the run script)
  L_CO2_rest_phase <- lapply(seq(L_CO2_rest_phase),
                             FUN = function(x) {
                               ext <- 101 - length(L_CO2_rest_phase[[x]])
                               return(c(L_CO2_rest_phase[[x]], rep(unname(CO2_restored[x]), ext)))
                             })

  L_CH4_rest_phase <- lapply(seq(L_CH4_rest_phase),
                             FUN = function(x) {
                               ext <- 101 - length(L_CH4_rest_phase[[x]])
                               return(c(L_CH4_rest_phase[[x]], rep(unname(CH4_restored[x]), ext)))
                             })

  names(L_CO2_rest_phase) <- c("Exp", "Min", "Max")
  names(L_CH4_rest_phase) <- c("Exp", "Min", "Max")

  L_peat_rest_phase <- list(Tot = list_op(l1 = L_CO2_rest_phase,
                                          l2 = L_CH4_rest_phase,
                                          func = "+"),
                            CO2 = L_CO2_rest_phase,
                            CH4 = L_CH4_rest_phase)


  if (0) {
    maxYear <- max(t_fallow) + max(t_restore_microbes) + 10
    par(mfrow=c(1,3))
    plot(x=seq(max(t_restore_microbes)),
         y=rep(max(unlist(L_peat_rest_phase$CO2)), max(t_restore_microbes)),
         xlim=c(0,maxYear),
         ylim=c(min(unlist(L_peat_rest_phase$CO2)),max(unlist(L_peat_rest_phase$CO2))),
         xlab="Time since restoration [yr]",
         ylab="Total CO2 emissions [tCO2]",
         type='n')
    for (i in 1:length(L_peat_rest_phase$CO2)) {
      lines(x=0:100,
            y=L_peat_rest_phase$CO2[[i]])
      points(x=0,
             y=L_peat_rest_phase$CO2[[i]][1],
             col="black")
      points(x=t_fallow[i]-1,
             y=L_peat_rest_phase$CO2[[i]][t_fallow[i]],
             col="red")
      points(x=t_fallow[i]+t_restore_microbes[i]+1,
             y=L_peat_rest_phase$CO2[[i]][t_fallow[i]+t_restore_microbes[i]+1],
             col="blue")
    }

    plot(x=seq(max(t_restore_microbes)),
         y=rep(max(unlist(L_peat_rest_phase$CH4)), max(t_restore_microbes)),
         xlim=c(0,maxYear),
         ylim=c(min(unlist(L_peat_rest_phase$CH4)),max(unlist(L_peat_rest_phase$CH4))),
         xlab="Time since restoration [yr]",
         ylab="Total CH4 emissions [eq. tCO2]",
         type='n')
    for (i in 1:length(L_peat_rest_phase$CH4)) {
      lines(x=0:100,
            y=L_peat_rest_phase$CH4[[i]])
      points(x=0,
             y=L_peat_rest_phase$CH4[[i]][1],
             col="black")
      points(x=t_fallow[i]-1,
             y=L_peat_rest_phase$CH4[[i]][t_fallow[i]],
             col="red")
      points(x=t_fallow[i]+t_restore_microbes[i]+1,
             y=L_peat_rest_phase$CH4[[i]][t_fallow[i]+t_restore_microbes[i]+1],
             col="blue")
    }

    plot(x=seq(max(t_restore_microbes)),
         y=rep(max(unlist(L_peat_rest_phase$Tot)), max(t_restore_microbes)),
         xlim=c(0,maxYear),
         ylim=c(min(unlist(L_peat_rest_phase$Tot)),max(unlist(L_peat_rest_phase$Tot))),
         xlab="Time since restoration [yr]",
         ylab="Total gaseous C emissions [eq. tCO2]",
         type='n')
    for (i in 1:length(L_peat_rest_phase$Tot)) {
      lines(x=0:100,
            y=L_peat_rest_phase$Tot[[i]])
      points(x=0,
             y=L_peat_rest_phase$Tot[[i]][1],
             col="black")
      points(x=t_fallow[i]-1,
             y=L_peat_rest_phase$Tot[[i]][t_fallow[i]],
             col="red")
      points(x=t_fallow[i]+t_restore_microbes[i]+1,
             y=L_peat_rest_phase$Tot[[i]][t_fallow[i]+t_restore_microbes[i]+1],
             col="blue")
    }
  }

  return(L_peat_rest_phase)
}
