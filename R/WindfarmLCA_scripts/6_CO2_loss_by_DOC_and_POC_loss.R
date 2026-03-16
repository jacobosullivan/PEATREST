## 6. CO2 loss by DOC & POC loss

#' CO2_loss_DOC_POC
#' @param core.dat UI data
#' @param L_indirect CO2 losses due to drainage
#' @param L_improvement CO2 gains due to restoration
#' @param pC_DOC estimated percent of total carbon losses lost as DOC
#' @param pDOC_CO2 percent DOC ultimately lost as CO2
#' @param pC_POC estimated percent of total carbon losses lost as POC
#' @param pPOC_CO2 percent POC ultimately lost as CO2
#' @return L_DPOC
#' @export
CO2_loss_DOC_POC <- function(core.dat,
                             L_indirect,
                             L_improvement,
                             pC_DOC = c(Exp = 26, Min = 7, Max = 40),
                             pDOC_CO2 = 100,
                             pC_POC = c(Exp = 8, Min = 4, Max = 10),
                             pPOC_CO2 = 100) {

  # THIS FUNCTION...

  CO2_C <- 3.667 # Molecular weight ratio C to CO2
  CH4_CO2 <- 30.66667 # CH4 to CO2 conversion factor
  pC_CH4 <- 0.75 # proportion of molecular weight of CH4 that is Carbon (12/16)

  if (core.dat$Site.restoration$restore_hab_in[1] == 2 & core.dat$Site.restoration$restore_hydr_in[1] == 2) {

    L_CO2_drain_rest <- L_indirect$L_drained$CO2 - L_indirect$L_undrained$CO2
    L_CO2_drain_rest[L_CO2_drain_rest<0] <- 0

    L_CH4_drain_rest <- L_indirect$L_drained$CH4 - L_indirect$L_undrained$CH4
    L_CH4_drain_rest[L_CH4_drain_rest<0] <- 0

    # Get difference in emissions before/after improvement
    L_CO2_improved <- list_op(l1 = L_improvement$E_CO2_aft,
                              l2 = L_improvement$E_CO2_bfr,
                              func = "-")

    L_CO2_improved <- lapply(L_CO2_improved, FUN = function(x) { # remove negative values
                        x[x<0] <- 0
                        return(x)
                      })

    L_CH4_improved <- list_op(l1 = L_improvement$E_CH4_aft,
                              l2 = L_improvement$E_CH4_bfr,
                              func = "-")

    L_CH4_improved <- lapply(L_CH4_improved, FUN = function(x) { # remove negative values
      x[x<0] <- 0
      return(x)
    })

    L_CO2_improved$Restored <- L_CO2_drain_rest # NULL if not restored
    L_CH4_improved$Restored <- L_CH4_drain_rest # NULL if not restored

    # Compute totals
    L_C_Tot <- colSums(bind_rows(L_CO2_improved) / CO2_C) + colSums(bind_rows(L_CH4_improved) * pC_CH4 / CH4_CO2)

    # Compute D/POC loss via empirically estimated ratios to total gaseous loss
    L_C_DOC <- L_C_Tot * pC_DOC / 100
    L_C_DOC[L_C_DOC<0] <- 0
    L_C_POC <- L_C_Tot * pC_POC / 100
    L_C_POC[L_C_POC<0] <- 0

    L_DOC <- L_C_DOC * (pDOC_CO2 / 100) * CO2_C
    L_POC <- L_C_POC * (pPOC_CO2 / 100) * CO2_C

    L_DPOC <- L_DOC + L_POC
  } else { # if not restoring, DOC and POC are already accounted for (assuming all C lost)
    L_DPOC <- c(Exp = 0, Min = 0, Max = 0)
  }

  return(L_DPOC)
}

#' CO2_loss_DOC_POC
#' @param forestry.dat UI forestry data
#' @param L_gaseous Emissions object (forest soils or peatland)
#' @param forest_soils Select input from forest soils model
#' @return L_DPOC
#' @export
CO2_loss_DOC_POC_RM <- function(forestry.dat,
                                L_gaseous,
                                forest_soils = T) {

  # THIS FUNCTION...

  rho_AqC <- forestry.dat$Aq.Carbon$rho_AqC # ratios from Smith et al. summed across DOC/POC
  R_AqC0 <- forestry.dat$Aq.Carbon$R_AqC0
  pAqC_CO2 <- forestry.dat$Aq.Carbon$pAqC_CO2
  A_harv <- map(forestry.dat[grep("Area", names(forestry.dat))], .f = "A_harv") # in units ha

  if (forest_soils) { # select data structure emissions from forest soils
    L_AqC <- lapply(seq_along(L_gaseous), FUN = function(x) {
      res <- lapply(seq_along(L_gaseous[[x]]), FUN = function(y) {
        df <- L_gaseous[[x]][[y]]
        df <- df %>%
          select(t, source, value) %>%
          group_by(t) %>%
          summarise(L_tot = sum(value)) %>%
          mutate(L_AqC = L_tot * rho_AqC[y]) %>%
          mutate(L_AqC = ifelse(L_AqC < R_AqC0[y], R_AqC0[y], L_AqC)) %>%
          mutate(L_AqC = L_AqC * pAqC_CO2[y]) %>% # multiply by proportion of AqC that ends up as CO2
          select(-L_tot) %>%

        return(df)
      })
      names(res) <- names(L_gaseous[[x]])
      return(res)
    })

    names(L_AqC) <- names(L_gaseous)

  } else { # select data structure emissions from peatlands

    L_AqC <- lapply(seq_along(L_gaseous), FUN = function(x) {
      res <- lapply(seq_along(L_gaseous[[x]]), FUN = function(y) {
        df <- L_gaseous[[x]][[y]]
        df <- df %>%
          mutate(L_tot = L_CO2 + L_CH4) %>%
          mutate(L_AqC = L_tot * rho_AqC[y]) %>%
          mutate(L_AqC = ifelse(L_AqC < R_AqC0[y] * A_harv[[x]][y], R_AqC0[y] * A_harv[[x]][y], L_AqC)) %>%
          mutate(L_AqC = L_AqC * pAqC_CO2[y]) %>% # multiply by proportion of AqC that ends up as CO2
          select(t, L_AqC)

          return(df)
      })
      names(res) <- names(L_gaseous[[x]])
      return(res)
    })

    names(L_AqC) <- names(L_gaseous)

  }

  return(L_AqC)
}


