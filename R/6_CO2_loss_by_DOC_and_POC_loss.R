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
#' @param core.dat UI data
#' @param L_peatland Peatland emissions object
#' @param pC_DOC estimated percent of total carbon losses lost as DOC
#' @param pDOC_CO2 percent DOC ultimately lost as CO2
#' @param pC_POC estimated percent of total carbon losses lost as POC
#' @param pPOC_CO2 percent POC ultimately lost as CO2
#' @return L_DPOC
#' @export
CO2_loss_DOC_POC_RM <- function(core.dat,
                                L_peatland,
                                pC_DOC = c(Exp = 26, Min = 7, Max = 40),
                                pDOC_CO2 = 100,
                                pC_POC = c(Exp = 8, Min = 4, Max = 10),
                                pPOC_CO2 = 100) {

  # THIS FUNCTION...

  # CO2_C <- 3.667 # Molecular weight ratio C to CO2
  # CH4_CO2 <- 30.66667 # CH4 to CO2 conversion factor
  # pC_CH4 <- 0.75 # proportion of molecular weight of CH4 that is Carbon (12/16)

  # Total gaseous carbon losses due to microbial metabolism IN UNITS CO2 eq.
  L_C_Tot <- lapply(seq_along(L_peatland), FUN = function(x) {
    res <- lapply(seq_along(L_peatland[[x]]), FUN = function(y) {
      df <- L_peatland[[x]][[y]]
      df <- df %>%
        mutate(L_C = L_CO2 + L_CH4) %>%
        select(t, L_C)
    })
    names(res) <- names(L_peatland[[x]])
    return(res)
  })

  names(L_C_Tot) <- names(L_peatland)

  # Compute D/POC loss via empirically estimated ratios to total gaseous loss
  L_DPOC <- lapply(seq_along(L_C_Tot), FUN = function(x) {
    res <- lapply(seq_along(L_C_Tot[[x]]), FUN = function(y) {
      df <- L_C_Tot[[x]][[y]]
      df <- df %>%
        mutate(L_DOC = L_C * (pC_DOC[y] / 100) * (pDOC_CO2 / 100),
               L_POC = L_C * (pC_POC[y] / 100) * (pPOC_CO2 / 100)) %>%
        select(-L_C)
      return(df)
    })
    names(res) <- names(L_C_Tot[[x]])
    return(res)
  })

  names(L_DPOC) <- names(L_C_Tot)

  return(L_DPOC)
}
