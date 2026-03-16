## 8. CO2 gain - site improvement

#' CO_2_gain_site_improve
#' @param core.dat UI data
#' @return L_improvement
#' @export
CO_2_gain_site_improve <- function(core.dat) {

  # Wrapper function for the CO_2_gain_site_improve0() module
  # THIS FUNCTION...

  if (core.dat$Site.restoration$restore_hab_in[1] == 2 & core.dat$Site.restoration$restore_hydr_in[1] == 2) { # habitat AND hydrology restored
    feature <- dat_improvement(core.dat)

    # Compute emissions savings
    L_improvement <- CO_2_gain_site_improve0(T_air = core.dat$Peatland$T_air,
                                             d_feature = feature$d,
                                             a_feature = feature$a,
                                             t_feature = feature$t,
                                             em_factor_meth_in = core.dat$Em.factor.meth$em_factor_meth_in,
                                             peat_type = core.dat$Peatland$peat_type)
  } else {
    no_imp <- c(Exp = 0, Min = 0, Max = 0)
    L_improvement <- list(L_improvement = list(Degraded.bog = no_imp,
                                               Felled.forestry = no_imp,
                                               Borrow.pits = no_imp,
                                               Found.hard = no_imp))
  }

  return(L_improvement)

}

#' dat_improvement
#' @param core.dat UI data
#' @return d/a/t_feature
#' @export
dat_improvement <- function(core.dat) {
  # Inputs required: depth of peat ABOVE WATER TABLE, area of improvement and guaranteed lifetime of improved peatland
  # These need to be generated from user inputs:
  d_peat_above_wt <- function(d_peat, d_wt) {

    # THIS FUNCTION..
    d <- apply(cbind(d_peat, d_wt), MAR = 1, FUN = min)

    return(d)
  }

  t_rest_guar <- function(t_rest, t_guar) {

    # THIS FUNCTION..
    t <- apply(cbind(t_rest, t_guar),
               MAR = 1, FUN = function(x) {
                 y <- x[2] - x[1]
                 y[y<0] <- 0
                 return(y)
               })
    return(t)
  }

  a_rest <- function(d_wt_bfr, d_wt_aft, a_impr) {

    # THIS FUNCTION...
    a <- apply(cbind(d_wt_bfr,
                     d_wt_aft,
                     a_impr),
               MAR = 1, FUN = function(x) {
                 if (x[1] > x[2]) {
                   return(x[3])
                 } else {
                   return(0)
                 }
               })

    return(a)
  }

  # Min/max emissions saved do not necessarily occur for min/max input variables
  # some variable need to be reorded e.g. if max Val produces min emissions saved
  min_max_reorder <- c(1,3,2)

  # Depth above water table
  d_feature = list(Degraded.bog = list(d_bfr = d_peat_above_wt(core.dat$Peatland$d_peat,
                                                               core.dat$Seq.improvement$d_wt_bog_bfr[min_max_reorder]),
                                       d_aft = d_peat_above_wt(core.dat$Peatland$d_peat,
                                                               core.dat$Seq.improvement$d_wt_bog_aft[min_max_reorder])),
                   Felled.forestry = list(d_bfr = d_peat_above_wt(core.dat$Peatland$d_peat,
                                                                  core.dat$Seq.improvement$d_wt_felled_bfr[min_max_reorder]),
                                          d_aft = d_peat_above_wt(core.dat$Peatland$d_peat,
                                                                  core.dat$Seq.improvement$d_wt_felled_aft[min_max_reorder])),
                   Borrow.pits = list(d_bfr = d_peat_above_wt(core.dat$Borrow.pits$d_pit,
                                                              core.dat$Seq.improvement$d_wt_pit_bfr[min_max_reorder]),
                                      d_aft = d_peat_above_wt(core.dat$Borrow.pits$d_pit,
                                                              core.dat$Seq.improvement$d_wt_pit_aft[min_max_reorder])),
                   Found.hard = list(d_bfr = d_peat_above_wt(core.dat$Peatland$d_peat,
                                                             core.dat$Seq.improvement$d_wt_fhs_bfr[min_max_reorder]),
                                     d_aft = d_peat_above_wt(core.dat$Peatland$d_peat,
                                                             core.dat$Seq.improvement$d_wt_fhs_aft[min_max_reorder])))

  # Guaranteed lifetime
  t_feature = list(Degraded.bog = list(t = t_rest_guar(core.dat$Seq.improvement$t_restore_hab_bog,
                                                       core.dat$Seq.improvement$t_restore_guar_bog[min_max_reorder])),
                   Felled.forestry = list(t = t_rest_guar(core.dat$Seq.improvement$t_restore_hab_felled,
                                                          core.dat$Seq.improvement$t_restore_guar_felled[min_max_reorder])),
                   Borrow.pits = list(t = t_rest_guar(core.dat$Seq.improvement$t_restore_hab_pit,
                                                      core.dat$Seq.improvement$t_restore_guar_pit[min_max_reorder])),
                   Found.hard = list(t = t_rest_guar(core.dat$Seq.improvement$t_restore_hydr_full,
                                                     core.dat$Windfarm$t_wf[min_max_reorder])))
  t_feature <- lapply(t_feature, FUN=function(x) return(list(t=x[[1]][min_max_reorder])))

  # area of improvement
  a_feature = list(Degraded.bog = list(a = a_rest(core.dat$Seq.improvement$d_wt_bog_bfr,
                                                  core.dat$Seq.improvement$d_wt_bog_aft,
                                                  core.dat$Seq.improvement$A_bog_improve)),
                   Felled.forestry = list(a = a_rest(core.dat$Seq.improvement$d_wt_felled_bfr,
                                                     core.dat$Seq.improvement$d_wt_felled_aft,
                                                     core.dat$Seq.improvement$A_felled_improve)),
                   Borrow.pits = list(a = a_rest(core.dat$Seq.improvement$d_wt_pit_bfr,
                                                 core.dat$Seq.improvement$d_wt_pit_aft,
                                                 core.dat$Seq.improvement$A_pit_improve)),
                   Found.hard = list(a = a_rest(core.dat$Seq.improvement$d_wt_fhs_bfr,
                                                core.dat$Seq.improvement$d_wt_fhs_aft,
                                                AV_indirect$Foundations$a/10000))) # unit conversion

  return(list(d = d_feature,
              a = a_feature,
              t = t_feature))
}

#' Emissions_saved
#' @param T_air Average air temperature
#' @param d_feature Before and after improvement water table depths
#' @param a_feature Area of improvement
#' @param t_feature Guaranteed improvement lifetime
#' @param em_factor_meth_in Select IPCC default or ECOSSE model
#' @param peat_type Select acid bog or fen
#' @return CO2 emissions rate ECOSSE Fen
#' @export
CO_2_gain_site_improve0 <- function(T_air,
                                    d_feature,
                                    a_feature,
                                    t_feature,
                                    em_factor_meth_in,
                                    peat_type) {

  # THIS FUNCTION...

  CO2_C <- 3.667 # Molecular weight ratio C to CO2
  CH4_CO2 <- 30.66667 # CH4 to CO2 conversion factor

  if (peat_type[1] == 1) { # Acid bog selected
    D_f <- 178
  } else {
    D_f <- 169
  }

  ## Calculate emissions rates depending on peat type and sub-model selected (IPCC/ECOSSE)
  ## Note both IPCC and ECOSSE functions return CH4 rates in units of CO2 eq.

  if (em_factor_meth_in[1] == 1) { # IPCC default calculation used

    ## Note R_CH4_bfr and R_CO2_bfr are assumed to be zero so not computed here
    ## In IPCC model, emissions rates DO NOT depend on depth and are therefore identical for all features
    if (peat_type[1] == 1) { # Acid bog selected
      R_CH4 <- IPCC_CH4_AB()
    } else { # Fen selected
      R_CH4 <- IPCC_CH4_F()
    }
    R_CO2 <- IPCC_CO2()

    ## Match required formatting
    R_CH4_aft <- vector(mode = "list", length=length(a_feature))
    R_CH4_aft <- lapply(R_CH4_aft, FUN = function(x) return(R_CH4))
    names(R_CH4_aft) <- names(a_feature)

    R_CO2_bfr <- vector(mode = "list", length=length(a_feature))
    R_CO2_bfr <- lapply(R_CO2_bfr, FUN = function(x) return(R_CO2))
    names(R_CO2_bfr) <- names(a_feature)

  } else { # Site specific calculation using ECOSSE method

    ## Note R_CH4_bfr is assumed to be zero so not computed here but R_CO2_aft is non-zero
    ## In this case, emissions rates DO depend on depth so vary for each feature
    if (peat_type[1] == 1) { # Acid bog selected

      R_CO2_bfr <- lapply(map(d_feature, "d_bfr"), FUN = function(x) ECOSSR_CO2_AB(CO2_C = CO2_C,
                                                                                   d_wt = x,
                                                                                   T_air = T_air))

      R_CH4_aft <- lapply(map(d_feature, "d_aft"), FUN = function(x) ECOSSR_CH4_AB(CH4_CO2 = CH4_CO2,
                                                                      d_wt = x,
                                                                      T_air = T_air))

      R_CO2_aft <- lapply(map(d_feature, "d_aft"), FUN = function(x) ECOSSR_CO2_AB(CO2_C = CO2_C,
                                                                               d_wt = x,
                                                                               T_air = T_air))
    } else { # Fen selected

      R_CO2_bfr <- lapply(map(d_feature, "d_bfr"), FUN = function(x) ECOSSR_CO2_F(CO2_C = CO2_C,
                                                                                  d_wt = x,
                                                                                  T_air = T_air))

      R_CH4_aft <- lapply(map(d_feature, "d_aft"), FUN = function(x) ECOSSR_CH4_F(CH4_CO2 = CH4_CO2,
                                                                              d_wt = x,
                                                                              T_air = T_air))

      R_CO2_aft <- lapply(map(d_feature, "d_aft"), FUN = function(x) ECOSSR_CO2_F(CO2_C = CO2_C,
                                                                              d_wt = x,
                                                                              T_air = T_air))
    }
  }

  ## Compute total emissions by scaling rates by area, improved time and proportion of flooded days
  E_CO2_bfr <- list_op(l1 = map(a_feature, "a"), l2 = map(t_feature, "t"), l3 = map(R_CO2_bfr, "R_CO2"), func = "*") # Assume D_f = 0

  E_CH4_aft <- list_op(l1 = map(a_feature, "a"), l2 = map(t_feature, "t"), l3 = map(R_CH4_aft, "R_CH4"), func = "*")
  E_CH4_aft <- lapply(E_CH4_aft, FUN=function(x) x * D_f / 365)

  E_CH4_bfr <- vector(mode = "list", length=length(E_CH4_aft))
  E_CH4_bfr <- lapply(E_CH4_bfr, FUN = function(x) return(c(Exp=0, Min=0, Max=0))) # Assume D_f = 0; E_CH4 = 0
  names(E_CH4_bfr) <- names(E_CH4_aft)

  if (em_factor_meth_in[1] == 1) { # IPCC selected
    # Here E_CO2 after improvement (rewetting) is assumed to be zero
    E_CO2_aft <- E_CH4_bfr # Assume: no CO2 emissions after re-wetting
  } else {
    # Here E_CO2 after improvement (rewetting) is modelled as a function of D_f
    E_CO2_aft <- list_op(l1 = map(a_feature, "a"), l2 = map(t_feature, "t"), l3 = map(R_CO2_aft, "R_CO2"), func = "*")
    E_CO2_aft <- lapply(E_CO2_aft, FUN=function(x) x * (365 - D_f) / 365)
  }

  ## Sum CO2 and CH4 contributions
  E_tot_bfr <- list_op(l1 = E_CO2_bfr, l2 = E_CH4_bfr, func = "+")
  E_tot_aft <- list_op(l1 = E_CO2_aft, l2 = E_CH4_aft, func = "+")
  L_improvement <- list_op(l1 = E_tot_bfr, l2 = E_tot_aft, func = "-")

  ## Estimate emissions savings from difference between pre- and post-improvement totals
  L_improvement <- list(E_CO2_bfr = E_CO2_bfr,
                        E_CO2_aft = E_CO2_aft,
                        E_CH4_bfr = E_CH4_bfr,
                        E_CH4_aft = E_CH4_aft,
                        L_improvement = L_improvement)

  return(L_improvement)
}
