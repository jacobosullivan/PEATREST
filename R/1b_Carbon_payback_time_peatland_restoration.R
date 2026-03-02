## 1b. Carbon payback time peatland restoration

#' Carbon_payback_time
#' @param res LCA output
#' @return Carbon payback time/flux intercept
#' @export
Carbon_payback_time <- function(res, sum_areas=T) {

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

  # THIS FAILS IF PEATLAND NEVER IMPROVES ON FLUXES FROM FORESTRY! FIX
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

  bind_rows(t_flux %>% mutate(metric="t_flux"),
            t_payback %>% mutate(metric="t_payback"))

  return(bind_rows(t_flux %>% mutate(metric="t_flux"),
                   t_payback %>% mutate(metric="t_payback")))
}
