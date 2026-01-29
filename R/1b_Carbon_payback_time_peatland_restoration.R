## 1b. Carbon payback time peatland restoration

#' Carbon_payback_time
#' @param S_forest Forest sequestration
#' @param R_tot_forestry Emissions from soils under forestry
#' @param L_forest Forest product losses
#' @param L_peatland Emissions from peatland
#' @param L_DPOC D/POC losses
#' @return Total peatland restoration carbon accounting
#' @export
Carbon_payback_time <- function(S_forest,
                                R_tot_forestry,
                                L_forest,
                                L_peatland,
                                L_DPOC) {

  # THIS FUNCTION..

  CO2_C <- 3.667 # Molecular weight ratio C to CO2

  # First convert C sequestration into units CO2
  S_forest_CO2 <- lapply(seq_along(S_forest), FUN = function(x) {
    res <- lapply(seq_along(S_forest[[x]]), FUN = function(y) {
      S <- S_forest[[x]][[y]]
      S <- S %>%
        select(t, NPP) %>%
        rename(S_forest = NPP) %>%
        mutate(S_forest = S_forest * CO2_C)
      return(S)
    })
    names(res) <- names(S_forest[[x]])
    return(res)
  })

  names(S_forest_CO2) <- names(S_forest)

  ## Extract dataframes from list objects
  S_forest_df <- getDf1(S_forest_CO2)
  L_peatland_df <- getDf2(L_peatland)
  L_DPOC_df <- getDf2(L_DPOC)
  L_forest_df <- getDf3(L_forest)
  L_forest_soils_df <- bind_rows(lapply(L_forest_soils, FUN = bind_rows)) %>%
    filter(source == "L_tot") %>%
    select(colnames(L_forest_df))

  S_forest_df$treatment <- "CF"
  L_forest_soils_df$treatment <- "CF"
  L_peatland_df$treatment <- "PR"
  L_DPOC_df$treatment <- "PR"
  L_forest_df$treatment <- "PR"

  ## Merge dataframes
  res <- rbind(S_forest_df,
               L_forest_soils_df,
               L_peatland_df,
               L_DPOC_df,
               L_forest_df)

  res <- res %>%
    mutate(value = ifelse(grepl("L_", source), -value, value))

  # res %>%
  #   arrange(t, treatment) %>%
  #   head(20)

  res_sum <- res %>%
    group_by(treatment, t, Area, Est) %>%
    summarise(value = sum(value))

  # res_sum %>%
  #   arrange(t, treatment) %>%
  #   head(20)

  p <- ggplot(res_sum, aes(x=t, y=value, col=treatment)) +
    geom_line() +
    scale_x_continuous(limits = c(NA, 200)) +
    geom_hline(yintercept = 0) +
    facet_grid(Est ~ Area, scales="free_y") +
    theme_bw() +
    labs(x="Time since harvesting [y]", y="Carbon sequestration [tCO2 eq.]", col="", title="LCA summary")

  return(p)
}
