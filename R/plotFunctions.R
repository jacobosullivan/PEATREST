#' getDf1
#' @param list_ob output of intermediate calculations in nested list format (Area, Exp/Min/Max)
#' @return dataframe
#' @export
getDf1 <- function(list_ob) {
  list_ob_df <- bind_rows(lapply(seq_along(list_ob), FUN = function(x) {
    res <- lapply(seq_along(list_ob[[x]]), FUN = function(y) {
      df <- list_ob[[x]][[y]]
      df$source <- colnames(df)[2]
      colnames(df)[2] <- "value"
      df <- df[,c(1,3,2)]
      df$Area <- names(list_ob)[x]
      df$Est <- names(list_ob[[x]])[y]
      return(df)
    })
    return(res)
  }))
  return(list_ob_df)
}

#' getDf2
#' @param list_ob output of intermediate calculations in nested list format (Area, Exp/Min/Max)
#' @return dataframe
#' @export
getDf2 <- function(list_ob) {
  list_ob_df <- bind_rows(lapply(seq_along(list_ob), FUN = function(x) {
    res <- lapply(seq_along(list_ob[[x]]), FUN = function(y) {
      df <- list_ob[[x]][[y]] %>%
        pivot_longer(cols = starts_with(c("L_")),
                     names_to = "source")
      df$Area <- names(list_ob)[x]
      df$Est <- names(list_ob[[x]])[y]
      return(df)
    })
    return(res)
  }))
}

#' getDf3
#' @param list_ob output of intermediate calculations in nested list format (Area, Exp/Min/Max)
#' @return dataframe
#' @export
getDf3 <- function(list_ob) {
  # For e.g. L_forest (multi-source output)
  list_ob_df <- lapply(seq_along(list_ob), FUN = function(x) {
    res <- lapply(seq_along(list_ob[[x]]), FUN = function(y) {
      res2 <- lapply(seq_along(list_ob[[x]][[y]]), FUN = function(z) {
        df <- as.data.frame(unname(list_ob[[x]][[y]][z])) %>%
          pivot_longer(cols = starts_with(c("L_", "S_")),
                       names_to = "source")
        df$Area <- names(list_ob)[x]
        df$Est <- names(list_ob[[x]][[y]])[z]
        return(df)
      })
      return(res2)
    })
    return(res)
  })

  list_ob_df <- bind_rows(unlist(list_ob_df, recursive = F))

  return(list_ob_df)
}

#' plotS_forest
#' @param S_forest Forest sequestration output
#' @return plot
#' @export
plotS_forest <- function(S_forest) {

  # First convert C sequestration into units CO2
  CO2_C <- 3.667 # Molecular weight ratio C to CO2
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

  S_forest_df <- getDf1(S_forest_CO2)

  p <- ggplot(S_forest_df, aes(x=t, y=value)) +
    geom_line(col="green3") +
    scale_x_continuous(limits = c(min(S_forest_df$t), 200)) +
    facet_grid(Est ~ Area, scales="free_y") +
    theme_bw() +
    labs(x="Time since harvesting [y]", y="Forestry sequestration [tCO2 eq.]", linetype="", title="Forestry sequestration (3PG)")

  return(p)
}

#' plotL_forest
#' @param L_forest Harvested forest losses output
#' @return plot
#' @export
plotL_forest <- function(L_forest) {

  L_forest_df <- getDf3(L_forest)

  L_forest_df$sink <- -1
  L_forest_df$sink[grep("S_", L_forest_df$source)] <- 1
  L_forest_df$discrete <- 0
  L_forest_df$discrete[L_forest_df$source %in% c("L_harv", "L_mulch", "L_dam", "L_bund", "L_turf_import", "L_turf_local", "L_fert", "L_T_biofuel", "L_T_wpF", "L_T_wpM", "L_T_wpS", "S_biofuel")] <- 1
  L_forest_df <- L_forest_df %>%
    mutate(value = ifelse(value==0, NA, value))

  p1 <- ggplot(L_forest_df %>% filter(discrete == 0), aes(x=t, y=value*sink, linetype=factor(source))) +
    geom_line(col="red3") +
    scale_x_continuous(limits = c(0, 50)) +
    facet_grid(Est ~ Area, scales="free_y") +
    theme_bw() +
    labs(x="Time since harvesting [y]", y="Wood product decay emissions [tCO2]", linetype="", title="Forestry product emissions")

  p2 <- ggplot(L_forest_df %>% filter(discrete == 1), aes(x=t, y=value*sink, col=factor(source))) +
    geom_point() +
    scale_x_continuous(limits = c(0, 30)) +
    facet_grid(Est ~ Area, scales="free_y") +
    theme_bw() +
    labs(x="Time since harvesting [y]", y="Discrete harvesting/management emissions [tCO2]", col="", title="Discrete management emissions")

  return(list(p1, p2))
}

#' plotS_bog_plants
#' @param S_bog_plants BOg plant sequestration output
#' @return plot
#' @export
plotS_bog_plants <- function(S_bog_plants) {

  # First convert C sequestration into units CO2
  CO2_C <- 3.667 # Molecular weight ratio C to CO2
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

  S_bog_plants_df <- getDf1(S_bog_plants_CO2)

  p <- ggplot(S_bog_plants_df, aes(x=t, y=value)) +
    geom_line(col="red3") +
    scale_x_continuous(limits = c(min(S_bog_plants_df$t), 200)) +
    facet_grid(Est ~ Area, scales="free_y") +
    theme_bw() +
    labs(x="Time since harvesting [y]", y="Bog plant sequestration [tCO2 eq.]", linetype="", title="Bog plant sequestration")

  return(p)
}

#' plotL_microbes
#' @param L_microbes Microbial emissions output
#' @return plot
#' @export
plotL_microbes <- function(L_microbes) {


  L_microbes_df <- getDf2(L_microbes)

  p <- ggplot(L_microbes_df %>%
                mutate(source = factor(source, levels = c("L_CO2", "L_CH4"))),
              aes(x=t, y=value, linetype=factor(source))) +
    geom_line(col="red3") +
    scale_x_continuous(limits = c(min(L_microbes_df$t), 200)) +
    facet_grid(Est ~ Area, scales="free_y") +
    theme_bw() +
    labs(x="Time since harvesting [y]", y="Peatland microbial emissions [tCO2 eq.]", linetype="", title="Peatland emissions (microbial)")

  return(p)
}

#' plotL_DPOC
#' @param L_DPOC D/POC losses output
#' @return plot
#' @export
plotL_DPOC <- function(L_DPOC) {


  L_DPOC_df <- getDf2(L_DPOC)

  p <- ggplot(L_DPOC_df %>%
                mutate(source = factor(source, levels = c("L_DOC", "L_POC"))),
              aes(x=t, y=value, linetype=factor(source))) +
    geom_line(col="red3") +
    scale_x_continuous(limits = c(min(L_DPOC_df$t), 200)) +
    facet_grid(Est ~ Area, scales="free_y") +
    theme_bw() +
    labs(x="Time since harvesting [y]", y="D/POC losses [tCO2 eq.]", linetype="", title="DOC and POC losses")

  return(p)
}

#' plotR_tot_forestry
#' @param R_tot_forestry Emissions rates from soil under trees
#' @return plot
#' @export
plotR_tot_forestry <- function(R_tot_forestry) {

  coeff <- 10

  R_tot_forestry_df <- bind_rows(lapply(R_tot_forestry, FUN = bind_rows)) %>%
    mutate(source = factor(source, levels=c("R_tot", "R_CO2", "R_CH4")))

  # ggplot(R_tot_forestry_df %>% filter(Est == "Exp", Area == "Area.1"), aes(x=t + 50)) +
  #   geom_line(aes(y=value, col=source)) +
  #   geom_line(aes(y=d_wt*coeff), col="grey") +
  #   scale_y_continuous(name = "Emissions (t CO2 eq. ha-1 yr-1)",
  #                      sec.axis = sec_axis(~./coeff, name = "Root / Water table depth (m)")) +
  #   scale_x_continuous(limits=c(NA, 100)) +
  #   # geom_vline(xintercept = 0, linetype=3) +
  #   facet_grid(Est ~ Area) +#, scales="free_y") +
  #   theme_bw() +
  #   theme(axis.title.y.right = element_text(color = "grey")) +
  #   labs(x = "Time (yr)", col="")

  p <- ggplot(R_tot_forestry_df, aes(x=t)) +
    geom_line(aes(y=value, col=source)) +
    geom_line(aes(y=d_wt*coeff), col="grey") +
    scale_y_continuous(name = "Emissions (t CO2 eq. ha-1 yr-1)",
                       sec.axis = sec_axis(~./coeff, name = "Water table depth (m)")) +
    scale_x_continuous(limits=c(NA, 100)) +
    geom_vline(xintercept = 0, linetype=3) +
    facet_grid(Est ~ Area) +#, scales="free_y") +
    theme_bw() +
    theme(axis.title.y.right = element_text(color = "grey")) +
    labs(x = "Time (yr)", col="")
  return(p)
}


