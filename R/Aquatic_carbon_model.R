#' AquaticCarbonMod
#' @param input.dat UI forestry data
#' @param L_gaseous Emissions object (forest soils or peatland)
#' @param forest_soils Select input from forest soils model
#' @return L_DPOC
#' @export
AquaticCarbonMod <- function(input.dat,
                             L_gaseous,
                             forest_soils = T) {

  # This function models the aquatic carbon losses from either forest soils or
  # restored peatland using a bounded linear scaling of the total gaseous emissions

  rho_AqC <- input.dat$Aq.Carbon$rho_AqC # ratios from Smith et al. summed across DOC/POC
  R_AqC0 <- input.dat$Aq.Carbon$R_AqC0
  pAqC_CO2 <- input.dat$Aq.Carbon$pAqC_CO2
  A_harv <- map(input.dat[grep("Area", names(input.dat))], .f = "A_harv") # in units ha

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
