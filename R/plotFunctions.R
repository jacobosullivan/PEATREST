#' plotS_forest
#' @param res LCA output
#' @return plot
#' @export
plotS_forest <- function(res) {

  # THIS FUNCTION...

  df <- res %>%
    filter(model == "Tree_growth")

  p <- ggplot(df, aes(x=t, y=value)) +
    geom_line(col="green3") +
    scale_x_continuous(limits = c(min(df$t), 200)) +
    facet_grid(Est ~ Area, scales="free_y") +
    theme_bw() +
    labs(x="Time since harvesting [y]", y="Forestry sequestration [tCO2 eq.]", linetype="", title="Forestry sequestration (3PG)")

  return(p)
}

#' plotR_tot_forestry
#' @param res LCA output
#' @return plot
#' @export
plotL_forest_soils <- function(res) {

  # THIS FUNCTION...

  df <- res %>%
    filter(model == "Forest_soils")

  coeff <- 20

  df <- bind_rows(lapply(df, FUN = bind_rows)) %>%
    mutate(source = factor(source, levels=c("L_tot", "L_CO2", "L_CH4")))

  p <- ggplot(df, aes(x=t)) +
    geom_line(aes(y=value, col=source)) +
    geom_line(aes(y=d_wt*coeff), col="grey") +
    scale_y_continuous(name = "Emissions (t CO2 eq. ha-1 yr-1)",
                       sec.axis = sec_axis(~./coeff, name = "Water table depth (m)")) +
    scale_x_continuous(limits=c(NA, 100)) +
    geom_vline(xintercept = 0, linetype=3) +
    facet_grid(Est ~ Area) +#, scales="free_y") +
    theme_bw() +
    theme(axis.title.y.right = element_text(color = "grey")) +
    labs(x = "Time (yr)", col="", title="Emissions from soils under trees")
  return(p)
}

#' plotL_forest
#' @param res LCA output
#' @return plot
#' @export
plotL_forest <- function(res) {

  # THIS FUNCTION ...

  df <- res %>%
    filter(model == "Management")

  df$discrete <- 0
  df$discrete[df$source %in% c("L_harv", "L_mulch", "L_dam", "L_bund", "L_smooth", "L_turf_import", "L_turf_local", "L_fert", "L_T_Biofuel", "L_T_wpF", "L_T_wpM", "L_T_wpS", "L_Biofuel")] <- 1
  df <- df %>%
    mutate(value = ifelse(value==0, NA, value))

  p1 <- ggplot(df %>% filter(discrete == 0), aes(x=t, y=value, col=factor(source))) +
    geom_line() +
    scale_x_continuous(limits = c(0, 50)) +
    facet_grid(Est ~ Area, scales="free_y") +
    theme_bw() +
    labs(x="Time since harvesting [y]", y="Decay emissions [tCO2]", col="", title="Forestry biomass decay emissions")

  source_l <- sort(unique((df %>% filter(discrete == 1))$source))
  source_l <- c("L_Biofuel", source_l[source_l != "L_Biofuel"])
  p2 <- ggplot(df %>%
                 filter(discrete == 1) %>%
                 mutate(source = factor(source, levels = source_l)),
               aes(x=factor(source), y=value, col=factor(source))) +
    geom_point() +
    guides(col="none") +
    scale_y_continuous(transform = "log10") +
    facet_grid(Est ~ Area) +
    theme_bw() +
    labs(x="Time since harvesting [y]", y="Harvesting / restoration emissions [tCO2]", col="", title="Discrete management emissions") +
    theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1))

  return(list(p1, p2))
}

#' plotL_peatland
#' @param res LCA output
#' @return plot
#' @export
plotL_peatland <- function(res) {


  # THIS FUNCTION ...

  df <- res %>%
    filter(model == "Peatland")

  df <- bind_rows(df,
                  df %>%
                    group_by(t, Area, Est) %>%
                    summarise(value=sum(value)) %>%
                    mutate(source = "L_tot")) %>%
    arrange(t, Area, Est)

  p <- ggplot(df %>%
                mutate(source = factor(source, levels = c("L_tot", "L_CO2", "L_CH4"))),
              aes(x=t, y=value, linetype=factor(source))) +
    geom_line(col="red3") +
    scale_x_continuous(limits = c(min(df$t), 200)) +
    facet_grid(Est ~ Area, scales="free_y") +
    theme_bw() +
    labs(x="Time since harvesting [y]", y="Peatland emissions [tCO2 eq.]", linetype="", title="Peatland emissions")

  return(p)
}

#' plotL_DPOC
#' @param res LCA output
#' @return plot
#' @export
plotL_DPOC <- function(res) {


  # THIS FUNCTION ...

  df <- res %>%
    filter(model == "Aq_carbon")

  p <- ggplot(df %>%
                mutate(source = factor(source, levels = c("L_DOC", "L_POC"))),
              aes(x=t, y=value, linetype=factor(source))) +
    geom_line(col="red3") +
    scale_x_continuous(limits = c(min(df$t), 200)) +
    facet_grid(Est ~ Area, scales="free_y") +
    theme_bw() +
    labs(x="Time since harvesting [y]", y="D/POC losses [tCO2 eq.]", linetype="", title="DOC and POC losses")

  return(p)
}

#' plotLCA
#' @param res LCA output
#' @return plot
#' @export
plotLCA <- function(res, sum_areas=T) {

  # THIS FUNCTION..

  if (sum_areas) {
    res_sum <- res %>%
      group_by(treatment, t, Est) %>%
      summarise(value = sum(value)) %>%
      mutate(Area = "All.areas")
  } else {
    res_sum <- res %>%
      group_by(treatment, Area, t, Est) %>%
      summarise(value = sum(value))
  }

  t_flux <- left_join(res_sum %>%
                        ungroup() %>%
                        filter(treatment=="CF") %>%
                        select(-c(treatment)) %>%
                        rename(CF = value),
                      res_sum %>%
                        ungroup() %>%
                        filter(treatment=="PR") %>%
                        select(-c(treatment)) %>%
                        rename(PR = value),
                      by=c("Area", "t", "Est")) %>%
    mutate(dif = PR-CF) %>%
    filter(dif >= 0 & !is.na(dif)) %>%
    group_by(Area, Est) %>%
    summarise(t = min(t))

  p <- ggplot(res_sum, aes(x=t, y=value, col=treatment)) +
    geom_line() +
    scale_x_continuous(limits = c(NA, 200)) +
    geom_vline(data=t_flux, aes(xintercept = t)) +
    facet_grid(Est ~ Area, scales="free_y") +
    theme_bw() +
    labs(x="Time since harvesting [y]", y="Carbon sequestration [tCO2 eq.]", col="", title="LCA summary")

  return(p)
}

#' plotLCA
#' @param res LCA output
#' @return plot
#' @export
plotLCA_cs <- function(res, sum_areas=T) {

  # THIS FUNCTION..

  if (sum_areas) {
    res_sum <- res %>%
      group_by(treatment, t, Est) %>%
      summarise(value = sum(value)) %>%
      mutate(Area = "All.areas")
  } else {
    res_sum <- res %>%
      group_by(treatment, Area, t, Est) %>%
      summarise(value = sum(value))
  }

  res_cs <- res_sum %>%
    filter(t >= 0) %>%
    group_by(treatment, Est, Area) %>%
    mutate(value_cs = cumsum(value))

  t_payback <- left_join(res_cs %>%
                           ungroup() %>%
                           filter(treatment=="CF") %>%
                           select(-c(treatment,value)) %>%
                           rename(CF_cs = value_cs),
                         res_cs %>%
                           ungroup() %>%
                           filter(treatment=="PR") %>%
                           select(-c(treatment,value)) %>%
                           rename(PR_cs = value_cs),
                         by=c("Area", "t", "Est")) %>%
    mutate(dif_cs = PR_cs-CF_cs) %>%
    filter(dif_cs >= 0) %>%
    group_by(Area, Est) %>%
    summarise(t = min(t))


  p <- ggplot(res_cs %>% filter(t <= 100), aes(x=t, y=value_cs, col=treatment)) +
      geom_line() +
      geom_vline(data=t_payback, aes(xintercept = t)) +
      facet_grid(Est ~ Area, scales="free_y") +
      theme_bw() +
      labs(x="Time since harvesting [y]", y="Cummulative carbon sequestration [tCO2 eq.]", col="", title="LCA summary")

  return(p)
}

