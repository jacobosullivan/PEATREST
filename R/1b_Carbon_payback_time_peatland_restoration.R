## 1b. Carbon payback time peatland restoration

#' Carbon_payback_time
#' @param energy_output output of Windfarm_output
#' @param E_mat counterfactuals in matrix form with IDs as rownames
#' @return Total Windfarm CO2 emissions savings
#' @export
Carbon_payback_time <- function(S_forest,
                                R_tot_forestry,
                                S_bog_plants,
                                L_forest,
                                L_microbes,
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

  S_bog_plants_CO2 <- lapply(seq_along(S_bog_plants), FUN = function(x) {
    res <- lapply(seq_along(S_bog_plants[[x]]), FUN = function(y) {
      S <- S_bog_plants[[x]][[y]]
      S <- S %>%
        mutate(S_bog_plants = S_bog_plants * CO2_C)
      return(S)
    })
    names(res) <- names(S_bog_plants[[x]])
    return(res)
  })

  names(S_bog_plants_CO2) <- names(S_bog_plants)

  ## Extract dataframes from list objects
  S_forest_df <- getDf1(S_forest_CO2)
  S_bog_plants_df <- getDf1(S_bog_plants_CO2)
  L_microbes_df <- getDf2(L_microbes)
  L_DPOC_df <- getDf2(L_DPOC)
  L_forest_df <- getDf3(L_forest)

  S_forest_df$treatment <- "counterfactual"
  S_bog_plants_df$treatment <- "restoration"
  L_microbes_df$treatment <- "restoration"
  L_DPOC_df$treatment <- "restoration"
  L_forest_df$treatment <- "restoration"

  ## Merge dataframes
  res <- rbind(S_forest_df,
               S_bog_plants_df,
               L_microbes_df,
               L_DPOC_df,
               L_forest_df)

  res <- res %>%
    mutate(value = ifelse(grepl("L_", source), -value, value))

  res_sum <- res %>%
    group_by(treatment, t, Area, Est) %>%
    summarise(value = sum(value))

  ggplot(res_sum, aes(x=t, y=value, col=treatment)) +
    geom_line() +
    scale_x_continuous(limits = c(min(res_sum$t), 100)) +
    facet_grid(Est ~ Area, scales="free_y") +
    theme_bw() +
    labs(x="Time since harvesting [y]", y="D/POC leaching [tCO2 eq.]", col="n", linetype="")

}
