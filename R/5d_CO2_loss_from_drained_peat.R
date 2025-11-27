## 5d. CO2 loss from drained peat

#' CO2_loss_drained
#' @param core.dat UI data
#' @param AV_indirect Area/Volume of drained peat
#' @param R_tot estimated emissions rates
#' @return L_indirect
#' @export
CO2_loss_drained <- function(core.dat, AV_indirect, R_tot) {

  # THIS FUNCTION...

  CO2_C <- 3.667 # Molecular weight ratio C to CO2

  # Extract input variables for easy access
  pC_dry_peat <- core.dat$Peatland$pC_dry_peat
  BD_dry_soil <- core.dat$Peatland$BD_dry_soil
  peat_type <- core.dat$Peatland$peat_type
  restore_hydr_in <- core.dat$Site.restoration$restore_hydr_in
  restore_hab_in <- core.dat$Site.restoration$restore_hab_in
  A_indirect <- AV_indirect$Total$a
  V_indirect <- AV_indirect$Total$v
  t_wf <- core.dat$Windfarm$t_wf
  t_restore <- core.dat$Bog.plants$t_restore

  if (peat_type[1] == 1) { # Acid bog selected
    D_f <- 178
  } else {
    D_f <- 169
  }

  # If restored emissions from drained and counterfactual (undrained) calculated using ECOSSE factors
  A_t <- (A_indirect / 10000) * (t_wf + t_restore)  # convert area to ha
  pD_f <- D_f / 365

  CO2_drained_rest <- A_t * (R_tot$R_CO2_drained)
  CH4_drained_rest <- A_t * (R_tot$R_CH4_drained)
  L_drained_rest <- list(Tot = CO2_drained_rest + CH4_drained_rest,
                         CO2 = CO2_drained_rest,
                         CH4 = CH4_drained_rest) # D_f = 0

  CO2_undrained_rest <- A_t * ((R_tot$R_CO2_undrained * pD_f) + (R_tot$R_CO2_drained * (1 - pD_f)))
  CH4_undrained_rest <- A_t * ((R_tot$R_CH4_undrained * pD_f) + (R_tot$R_CH4_drained * (1 - pD_f)))
  L_undrained_rest <- list(Tot = CO2_undrained_rest + CH4_undrained_rest,
                           CO2 = CO2_undrained_rest,
                           CH4 = CH4_undrained_rest)


  p_undrained <- L_undrained_rest$Tot / L_drained_rest$Tot # proportion used in case not restored

  # If not restored, all carbon assumed lost, counterfactual given by ratio computed above
  L_drained_no_rest <- list(Tot = (CO2_C / 100) * pC_dry_peat * BD_dry_soil * V_indirect)
  L_undrained_no_rest <- list(Tot = L_drained_no_rest$Tot * p_undrained)

  L_drained <- L_drained_rest # first assume restoration
  L_undrained <- L_undrained_rest

  if (any(restore_hab_in == 1 | restore_hydr_in == 1)) {
    # replace non-restored instances with results without restoration
    L_drained$Tot[restore_hab_in == 1 | restore_hydr_in == 1] <- L_drained_no_rest$Tot[restore_hab_in == 1 | restore_hydr_in == 1]
    L_undrained$Tot[restore_hab_in == 1 | restore_hydr_in == 1] <- L_undrained_no_rest$Tot[restore_hab_in == 1 | restore_hydr_in == 1]
  }

  L_indirect <- c(Exp = L_drained$Tot[1] - L_undrained$Tot[1],
                  Min = min(L_drained$Tot[2:3] - L_undrained$Tot[2:3]),
                  Max = max(L_drained$Tot[2:3] - L_undrained$Tot[2:3]))

  L_indirect <- list(L_drained = L_drained,
                     L_undrained = L_undrained,
                     L_indirect = L_indirect)

  return(L_indirect)
}
