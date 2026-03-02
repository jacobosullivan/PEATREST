#' plotS_forest
#' @param res LCA output
#' @return plot
#' @export
plotS_forest <- function(res) {

  # THIS FUNCTION...

  df <- res %>%
    filter(model == "Tree_growth") %>%
      mutate(Est = factor(Est, levels = c("Min", "Exp", "Max")))


  p <- ggplot(df, aes(x=t, y=value)) +
    geom_line(col=hue_pal()(1)) +
    scale_x_continuous(limits = c(min(df$t), 200)) +
    facet_grid(Area ~ Est, scales="free_y") +
    theme_bw() +
    labs(x="Time since harvesting (yr)", y="Sequestration (t CO2 eq.)", linetype="")#, title="Forestry sequestration (3PG)")

  return(p)
}

#' plotL_forest_soils
#' @param res LCA output
#' @return plot
#' @export
plotL_forest_soils <- function(res) {

  # THIS FUNCTION...

  df <- res %>%
    filter(model == "Forest_soils") %>%
    mutate(value = -value)

  df <- bind_rows(df,
                  df %>%
                    group_by(model, treatment, t, Area, Est) %>%
                    summarise(value=sum(value)) %>%
                    mutate(source="L_tot")) %>%
    mutate(source = factor(source, levels=c("L_CO2", "L_CH4", "L_tot"))) %>%
    mutate(Est = factor(Est, levels = c("Min", "Exp", "Max")))

  axis_labels <- parse(text = paste0(stringr::str_replace((levels(df$source)), "\\_", "\\["),"]"))

  p <- ggplot(df, aes(x=t, y=value, col=source, linetype=source)) +
    geom_line() +
    scale_x_continuous(limits=c(NA, 100)) +
    scale_colour_manual(values = c(hue_pal()(2), "black"),
                        labels = axis_labels) +
    scale_linetype_manual(values = c(1,1,2), guide = "none") +
    # scale_y_log10() +
    geom_vline(xintercept = 0, linetype=3) +
    facet_grid(Area ~ Est) +#, scales="free_y") +
    theme_bw() +
    labs(x = "Time (yr)", y="Emissions (t CO2 eq.)", col="")#, title="Emissions from soils under trees")
  return(p)
}

#' plotCounterfactual
#' @param res LCA output
#' @return plot
#' @export
plotCounterfactual <- function(res) {

  # THIS FUNCTION...

  df0 <- res %>%
    filter(model == "Tree_growth") %>%
    mutate(Est = factor(Est, levels = c("Min", "Exp", "Max")))

  df1 <- res %>%
    filter(model == "Forest_soils")

  df1 <- df1 %>%
    group_by(model, treatment, t, Area, Est) %>%
    summarise(value=sum(value)) %>%
    mutate(source="L_forest") %>%
    mutate(Est = factor(Est, levels = c("Min", "Exp", "Max")))

  df2 <- res %>%
    filter(model == "Forest_AqC_loss") %>%
    mutate(Est = factor(Est, levels = c("Min", "Exp", "Max")))

  df <- bind_rows(df0, df1, df2)

  df <- bind_rows(df,
                  df %>%
                    group_by(t,Area,Est) %>%
                    summarise(value=sum(value)) %>%
                    mutate(source = "S_tot")) %>%
    mutate(source = factor(source, levels = c("S_tot", "S_forest", "L_forest", "L_AqC")))

  axis_labels <- parse(text = paste0(stringr::str_replace((levels(df$source)), "\\_", "\\["),"]"))

  p <- ggplot(df, aes(x=t, y=value, col=factor(source))) +
    geom_line() +
    scale_x_continuous(limits = c(min(df$t), 200)) +
    scale_color_manual(values = c( "black", hue_pal()(3)),
                       labels = axis_labels) +
    facet_grid(Area ~ Est, scales="free_y") +
    theme_bw() +
    labs(x="Time since harvesting (yr)", y="Sequestration (t CO2 eq.)", col="")#, title="Forestry sequestration (3PG)")

  return(p)
}

#' plotL_forest
#' @param res LCA output
#' @return plot
#' @export
plotL_forest <- function(res) {

  # THIS FUNCTION ...

  df <- res %>%
    filter(model == "Management") %>%
    mutate(Est = factor(Est, levels = c("Min", "Exp", "Max")))

  df$discrete <- 0
  df$discrete[df$source %in% c("L_harv", "L_mulch", "L_dam", "L_bund", "L_smooth", "L_turf_import", "L_turf_local", "L_fert", "L_TBiofuel", "L_TwpF", "L_TwpM", "L_TwpS", "L_Biofuel")] <- 1
  df <- df %>%
    mutate(value = ifelse(value==0, NA, value))

  df_cont <- df %>%
    filter(discrete == 0) %>%
    select(t, Area, Est, source, value)

  df_cont <- bind_rows(df_cont %>%
                         group_by(t, Area, Est) %>%
                         summarise(value = sum(value)) %>%
                         mutate(source = "L_tot"),
                       df_cont) %>%
    mutate(source = factor(source, levels = unique(source))) %>%
    arrange(t, Area, Est)

  axis_labels1 <- parse(text = paste0(stringr::str_replace((levels(df_cont$source)), "\\_", "\\["),"]"))

  p1 <- ggplot(df_cont %>%
                 mutate(value = -value),
               aes(x=t, y=value, col=factor(source))) +
    geom_line() +
    scale_x_continuous(limits = c(0, 50)) +
    scale_color_manual(values = c( "black", hue_pal()(length(axis_labels1)-1)),
                         labels = axis_labels1) +
    facet_grid(Area ~ Est, scales="free_y") +
    theme_bw() +
    labs(x="Time since harvesting (yr)", y="Emissions (t CO2 eq.)", col="")#, title="Forestry biomass decay emissions")

  df_disc <- df %>%
    filter(discrete == 1) %>%
    select(t, Area, Est, source, value)

  df_disc <- bind_rows(df_disc %>%
                         group_by(Area, Est) %>%
                         summarise(value = sum(value)) %>%
                         mutate(source = "L_tot"),
                       df_disc) %>%
    arrange(Area, Est, value) %>%
    mutate(source = factor(source, levels = unique(source))) %>%
    arrange(t, Area, Est)

  axis_labels2 <- parse(text = paste0(stringr::str_replace((levels(df_disc$source)), "\\_", "\\["),"]"))

  p2 <- ggplot(df_disc %>%
                 mutate(value = -value),
               aes(x=factor(source), y=value, col=factor(source))) +
    geom_point() +
    guides(col="none") +
    scale_y_continuous(transform = "log10") +
    scale_x_discrete(labels = axis_labels2) +
    scale_color_manual(values = c("black", hue_pal()(length(axis_labels2)-1))) +
    facet_grid(Area ~ Est) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.4, hjust = 1)) +
    labs(x="", y="Emissions (t CO2)", col="")#, title="Discrete management emissions") +

  return(list(p1, p2))
}

#' plotL_peatland
#' @param res LCA output
#' @return plot
#' @export
plotL_peatland <- function(res) {


  # THIS FUNCTION ...

  df0 <- res %>%
    filter(model == "Peatland") %>%
    mutate(Est = factor(Est, levels = c("Min", "Exp", "Max")))

  df1 <- res %>%
    filter(model == "Peatland_AqC_loss") %>%
    mutate(Est = factor(Est, levels = c("Min", "Exp", "Max")))

  df <- bind_rows(df0, df1)

  df <- bind_rows(df,
                  df %>%
                    group_by(t, Area, Est) %>%
                    summarise(value=sum(value)) %>%
                    mutate(source = "S_tot")) %>%
    arrange(t, Area, Est) %>%
    mutate(source = factor(source, levels = c("S_tot", "L_CO2", "L_CH4", "L_AqC")))

  axis_labels <- parse(text = paste0(stringr::str_replace((levels(df$source)), "\\_", "\\["),"]"))

  p <- ggplot(df,
              aes(x=t, y=value, col=factor(source))) +
    geom_line() +
    scale_color_manual(values = c("black", hue_pal()(length(axis_labels)-1)),
                       labels = axis_labels) +
    scale_x_continuous(limits = c(min(df$t), 120)) +
    facet_grid(Area ~ Est, scales="free_y") +
    theme_bw() +
    labs(x="Time since harvesting (yr)", y="Sequestration (t CO2 eq.)", col="")#, title="Peatland emissions")

  return(p)
}

#' plotL_AqC_forest_soils
#' @param res LCA output
#' @return plot
#' @export
plotL_AqC_forest_soils <- function(res) {

  # THIS FUNCTION ...

  df <- res %>%
    filter(model == "Forest_AqC_loss") %>%
    mutate(Est = factor(Est, levels = c("Min", "Exp", "Max")),
           source = factor(source))

  axis_labels <- parse(text = paste0(stringr::str_replace((levels(df$source)), "\\_", "\\["),"]"))

  p <- ggplot(df,
              aes(x=t, y=value, col=factor(source))) +
    geom_line() +
    scale_color_manual(values = hue_pal()(1),
                       labels = axis_labels) +
    scale_x_continuous(limits = c(min(df$t), 200)) +
    facet_grid(Area ~ Est, scales="free_y") +
    theme_bw() +
    labs(x="Time since harvesting (yr)", y="Aquatic carbon loss (t CO2 eq.)", col="")#, title="DOC and POC losses")

  return(p)
}

#' plotL_AqC_peatland
#' @param res LCA output
#' @return plot
#' @export
plotL_AqC_peatland <- function(res) {

  # THIS FUNCTION ...

  df <- res %>%
    filter(model == "Peatland_AqC_loss") %>%
    mutate(Est = factor(Est, levels = c("Min", "Exp", "Max")),
           source = factor(source))

  axis_labels <- parse(text = paste0(stringr::str_replace((levels(df$source)), "\\_", "\\["),"]"))

  p <- ggplot(df,
              aes(x=t, y=value, col=factor(source))) +
    geom_line() +
    scale_color_manual(values = hue_pal()(1),
                       labels = axis_labels) +
    scale_x_continuous(limits = c(min(df$t), 200)) +
    facet_grid(Area ~ Est, scales="free_y") +
    theme_bw() +
    labs(x="Time since harvesting (yr)", y="Aquatic carbon loss (t CO2 eq.)", col="")#, title="DOC and POC losses")

  return(p)
}

#' plotLCA
#' @param res LCA output
#' @param t_payback_res carbon flux intercept estimate
#' @return plot
#' @export
plotLCA <- function(res, t_payback_res, sum_areas=T) {

  # THIS FUNCTION..

  if (sum_areas) {
    res_sum <- res %>%
      group_by(treatment, t, Est) %>%
      summarise(value = sum(value)) %>%
      mutate(Area = "All.areas")

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

  } else {
    res_sum <- res %>%
      group_by(treatment, Area, t, Est) %>%
      summarise(value = sum(value))

    t_flux <- t_payback_res %>%
      filter(metric == "t_flux")
  }

  res_sum <- res_sum %>%
    mutate(Est = factor(Est, levels = c("Min", "Exp", "Max")))

  t_flux <- t_flux %>%
    mutate(Est = factor(Est, levels = c("Min", "Exp", "Max")))

  p <- ggplot(res_sum %>% filter(t <= max(t_payback_res$t + 50)), aes(x=t, y=value, col=treatment)) +
    geom_line() +
    geom_vline(data=t_flux, aes(xintercept = t), linetype=2) +
    facet_grid(Area ~ Est, scales="free_y") +
    theme_bw() +
    labs(x="Time since harvesting (yr)", y="Sequestration (t CO2 eq. / yr)", col="")#, title="LCA summary")

  return(p)
}

#' plotLCA
#' @param res LCA output
#' @param t_payback_res carbon payback estimate
#' @return plot
#' @export
plotLCA_cs <- function(res, t_payback_res, sum_areas=T) {

  # THIS FUNCTION..

  if (sum_areas) {
    res_sum <- res %>%
      group_by(treatment, t, Est) %>%
      summarise(value = sum(value)) %>%
      mutate(Area = "All.areas")

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
  } else {
    res_sum <- res %>%
      group_by(treatment, Area, t, Est) %>%
      summarise(value = sum(value))

    res_cs <- res_sum %>%
      filter(t >= 0) %>%
      group_by(treatment, Est, Area) %>%
      mutate(value_cs = cumsum(value))

    t_payback <- t_payback_res %>%
      filter(metric == "t_payback")
  }

  res_cs <- res_cs %>%
    mutate(Est = factor(Est, levels = c("Min", "Exp", "Max")))

  t_payback <- t_payback %>%
    mutate(Est = factor(Est, levels = c("Min", "Exp", "Max")))

  p <- ggplot(res_cs %>% filter(t <= max(t_payback_res$t) + 50), aes(x=t, y=value_cs, col=treatment)) +
      geom_line() +
      scale_x_continuous(limits = c(min(res_sum$t), NA)) +
      geom_vline(data=t_payback , aes(xintercept = t), linetype=2) +
      facet_grid(Area ~ Est, scales="free_y") +
      theme_bw() +
      labs(x="Time since harvesting (yr)", y="Cummulative sequestration (t CO2 eq.)", col="")#, title="LCA summary")

  return(p)
}

#' plotES
#' @param res LCA output
#' @param t_payback_res carbon payback estimate
#' @return plot
#' @export
plotES <- function(res, t_payback_res) {

  # THIS FUNCTION..

  res_sum <- res %>%
    group_by(treatment, Area, t, Est) %>%
    summarise(value = sum(value))

  res_cs <- res_sum %>%
    filter(t >= 0) %>%
    group_by(treatment, Est, Area) %>%
    mutate(value = cumsum(value))

  t_payback <- t_payback_res %>%
    filter(metric == "t_payback")

  t_flux <- t_payback_res %>%
    filter(metric == "t_flux")

  res_sum$model <- "Sequestration (t CO2 eq. / (ha yr))"
  res_cs$model <- "Storage (t CO2 eq. / ha)"
  t_flux$model <- "Sequestration (t CO2 eq. / (ha yr))"
  t_payback$model <- "Storage (t CO2 eq. / ha)"

  res0 <- bind_rows(res_sum, res_cs)
  t_payback0 <- bind_rows(t_flux, t_payback)

  p <- ggplot(res0 %>% filter(t <= max(t_payback_res$t) + 50),
               aes(x=t, y=value, col=treatment)) +
    geom_line() +
    scale_x_continuous(limits = c(min(res_sum$t), NA)) +
    geom_vline(data=t_payback0 , aes(xintercept = t), linetype=2) +
    facet_wrap(~ model, scales="free_y") +
    theme_bw() +
    labs(x="Time since harvesting (yr)", y="Value", col="")

  return(p)
}

