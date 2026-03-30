## Peatland emissions regression modelling. METAR21_XXX (Evans et al. 2021) used in LCA

#' IPCC_CO2
#' @return CO2 emissions rate IPCC Acid bog AND Fen
#' @export
IPCC_CO2 <- function() {
  R_CO2 <- 9.6 * (12 + 16 + 16) / 12
  return(list(R_CO2 = c(Exp = R_CO2, Min = R_CO2, Max = R_CO2))) # Identical for both peat types
}

#' IPCC_CH4_AB
#' @param CH4_CO2 CH4 to CO2 conversion factor
#' @return CH4 emissions rate IPCC Acid bog
#' @export
IPCC_CH4_AB <- function(CH4_CO2) {
  R_CH4 <- CH4_CO2 * (11 / 1000000000) * 10000 * 365
  return(list(R_CH4 = c(Exp = R_CH4, Min = R_CH4, Max = R_CH4)))
}

#' IPCC_CH4_F
#' @param CH4_CO2 CH4 to CO2 conversion factor
#' @return CH4 emissions rate IPCC Fen
#' @export
IPCC_CH4_F <- function(CH4_CO2) {
  R_CH4 = CH4_CO2 * (60 / 1000000000) * 10000 * 365
  return(list(R_CH4 = c(Exp = R_CH4, Min = R_CH4, Max = R_CH4)))
}

#' METAR21_CO2
#' @param d_wt Average water table depth (undrained)
#' @param CO2_C Molecular weight ratio C to CO2
#' @return CO2 emissions rate meta-analytic regression Acid bog
#' @export
METAR21_CO2 <- function(CO2_C, d_wt) {
  # Evans et al. 2021 https://doi.org/10.1038/s41586-021-03523-1
  return(list(R_CO2 = CO2_C * (0.1341 * (d_wt*100) - 1.73)))
}

#' METAR21_CH4
#' @param d_wt Average water table depth (undrained)
#' @param CH4_CO2 CH4 to CO2 conversion factor
#' @return CH4 emissions rate meta-analytic regression Acid bog
#' @export
METAR21_CH4 <- function(CO2_C, d_wt) {
  # Evans et al. 2021 https://doi.org/10.1038/s41586-021-03523-1
  return(list(R_CH4 = CO2_C * (0.334 * 0.5^(((d_wt*100) + 5) / 6.31))))
}

#' METAR97_CO2_AB
#' @param T_air Average air temperature
#' @param d_wt Average water table depth (undrained)
#' @param CO2_C Molecular weight ratio C to CO2
#' @return CO2 emissions rate meta-analytic regression Acid bog
#' @export
METAR97_CO2_AB <- function(CO2_C, d_wt, T_air) {
  return(list(R_CO2 = (CO2_C/1000) * ((6700 * exp(-0.26 * exp(-0.05153 * ((100*d_wt)-50)))) + ((72.54 * T_air) - 800))))
}

#' METAR97_CH4_AB
#' @param T_air Average air temperature
#' @param d_wt Average water table depth (undrained)
#' @param CH4_CO2 CH4 to CO2 conversion factor
#' @return CH4 emissions rate meta-analytic regression Acid bog
#' @export
METAR97_CH4_AB <- function(CH4_CO2, d_wt, T_air) { # converts into CO2 eq units
  return(list(R_CH4 = (CH4_CO2/1000) * (500 * exp(-0.1234 * (100*d_wt)) + ((3.529*T_air) - 36.67))))
}

#' METAR97_CO2_F
#' @param T_air Average air temperature
#' @param d_wt Average water table depth (undrained)
#' @param CO2_C Molecular weight ratio C to CO2
#' @return CO2 emissions rate meta-analytic regression Fen
#' @export
METAR97_CO2_F <- function(CO2_C, d_wt, T_air) {
  return(list(R_CO2 = (CO2_C/1000) * (16244 * exp(-0.17594 * exp(-0.07346 * ((d_wt*100)-50))) + (153.234*T_air))))
}

#' METAR97_CH4_F
#' @param T_air Average air temperature
#' @param d_wt Average water table depth (undrained)
#' @param CH4_CO2 CH4 to CO2 conversion factor
#' @return CO2 emissions rate meta-analytic regression Fen
#' @export
METAR97_CH4_F <- function(CH4_CO2, d_wt, T_air) { # converts into CO2 eq units
  return(list(R_CH4 = (CH4_CO2/1000) * (-10 + 563.6253 * exp(-0.09702 * (100*d_wt)) + (0.662183*T_air))))
}

#' ForestSoilsEmissionsMod
#' @param input.dat UI forestry data
#' @return Emissions rates from drained soils under trees
#' @export
ForestSoilsEmissionsMod <- function(input.dat) {

  # This function models the water table below the tree and thereby the emissions
  # from the drained peats

  CO2_C <- 3.667 # Molecular weight ratio C to CO2
  CH4_CO2 <- 30.66667 # CH4 to CO2 conversion factor

  # Extract inputs for easy access
  d_peat <- map(input.dat[grep("Area", names(input.dat))], .f = "d_peat")
  d_drain <- map(input.dat[grep("Area", names(input.dat))], .f = "d_drain")
  Spp <- map(input.dat[grep("Area", names(input.dat))], .f = "species")
  t_harv <- map(input.dat[grep("Area", names(input.dat))], .f = "t_harv")
  A_harv <- map(input.dat[grep("Area", names(input.dat))], .f = "A_harv")
  YC <- map(input.dat[grep("Area", names(input.dat))], .f = "YC") # if not passed by user, already computed elsewhere from Growth and yield tables
  species <- c("Scots_pine", "Sitka_spruce")
  soil_type <- map(input.dat[grep("Area", names(input.dat))], .f = "soil_type")
  d_root_max <- input.dat$Root.depth$d_root_max
  rho_r_soil <- input.dat$Root.depth$rho_r_soil
  sigma_zR <- 1 / rho_r_soil
  sigma_zR <- sigma_zR[c(1,3,2)] # low density, high volume, high impact on hydrology
  names(sigma_zR) <- c("Exp", "Min", "Max")
  r_CBiomass <- map(input.dat[grep("Area", names(input.dat))], .f = "r_CBiomass")

  # Get rooting depths for water table estimates
  d_wt <- lapply(seq_along(YC), FUN = function(x) {
    d_wt_a <- lapply(seq_along(YC[[x]]), FUN = function(y) {
      Spp_a <- species[Spp[[x]][1]]
      YC_a0 <- YC[[x]][y]
      t_harv_a <- t_harv[[x]][y]

      ## Deal with missing YC values from GY table
      YC_avail <- unlist(input.dat$growthYield %>% filter(Spp == Spp_a) %>% select(YC) %>% unique())

      ### If YC_a is not available, set to closest value.
      ### If equidistant from multiple available values, maximum is used (conservative estimate of payback time)
      YC_a <- max(YC_avail[which(abs(YC_a0 - YC_avail) == min(abs(YC_a0 - unlist(YC_avail))))])

      res <- input.dat$growthYield %>%
        filter(Spp == Spp_a,
               YC == YC_a) %>%
        select(Age, B_r) %>%
        mutate(B_r = (r_CBiomass[[x]][y])^-1 * B_r) %>% # multiplication by 1/r_CBiomass due to the fact that B_r is in dry biomass, while root zone density was estimated using wet biomass
        mutate(#V_r = B_r * sigma_zR[Spp_a], # 3PG version
               V_r = B_r * sigma_zR[y],
               d_wt = V_r / 10000,
               YC = YC_a0,
               YC_avail = YC_a)

      res$Area <- names(YC)[x]

      return(res)
    })

    d_wt_a <- bind_rows(d_wt_a)
    return(d_wt_a)
  })

  names(d_wt) <- names(YC)

  d_wt_pred <- lapply(seq_along(YC), FUN = function(x) {
    d_wt_pred_a <- lapply(seq_along(unique(d_wt[[x]]$YC)), FUN = function(y) {

      YC_a <- unique(d_wt[[x]]$YC)[y]

      fit <- lm(d_wt ~ Age, data = d_wt[[x]] %>% filter(YC==YC_a))

      Age_pred <- 0:(500+t_harv[[x]][y])
      pred <- data.frame(Age = Age_pred)
      pred$Area = names(YC)[x]
      pred$YC = YC[[x]][y]
      pred$YC_avail = unique(d_wt[[x]]$YC)[y]
      pred$d_wt <- predict(fit, newdata = pred)

      pred <- pred %>%
        mutate(d_wt = ifelse(d_wt > min(d_peat[[x]][y], d_root_max[y]), min(d_peat[[x]][y], d_root_max[y]), d_wt)) %>%
        mutate(d_wt = ifelse(d_wt < d_drain[[x]][y], d_drain[[x]][y], d_wt))

      return(pred)
    })
    names(d_wt_pred_a) <- names(YC[[x]])
    return(d_wt_pred_a)
  })
  names(d_wt_pred) <- names(YC)

  if (0) {
    p <- ggplot(bind_rows(d_wt) %>% filter(Area == "Area.1", Age %% 3 == 0),
           aes(x=Age, y=d_wt, col=factor(YC))) +
      geom_point() +
      scale_x_continuous(limits=c(NA, 200)) +
      geom_line(data=bind_rows(lapply(d_wt_pred, FUN=bind_rows)) %>% filter(Area == "Area.1")) +
      # facet_wrap(~ Area) +
      theme_bw() +
      labs(x = "Stand age (yr)", y = "[Root / Water table] depth (m)", col="YC")
    p

    ww <- 16
    hh <- 12

    png("../Figures/root_depth_bounded_linear.png",
        width=ww/1.58, height=hh/1.6, units="cm", res=300)
    p
    dev.off()
  }

  R_CO2_dry <- lapply(seq_along(d_wt_pred), FUN = function(x) {
    R_CO2_dry_a <- lapply(seq_along(d_wt_pred[[x]]), FUN = function(y) {
      res <- METAR21_CO2(CO2_C, d_wt_pred[[x]][[y]]$d_wt)
      df <- data.frame(t = d_wt_pred[[x]][[y]]$Age-t_harv[[x]][y],
                       d_wt = d_wt_pred[[x]][[y]]$d_wt,
                       R_CO2 = unname(res$R_CO2))
      df$Est <- names(d_wt_pred[[x]])[y]
      df$Area <- names(d_wt_pred)[x]
      return(df)
    })

    names(R_CO2_dry_a) <- names(d_wt_pred[[x]])
    return(R_CO2_dry_a)
  })
  names(R_CO2_dry) <- names(d_wt_pred)

  R_CH4_dry <- lapply(seq_along(d_wt_pred), FUN = function(x) {
    R_CH4_dry_a <- lapply(seq_along(d_wt_pred[[x]]), FUN = function(y) {
      res <- METAR21_CH4(CO2_C, d_wt_pred[[x]][[y]]$d_wt)
      df <- data.frame(t = d_wt_pred[[x]][[y]]$Age-t_harv[[x]][y],
                       d_wt = d_wt_pred[[x]][[y]]$d_wt,
                       R_CH4 = unname(res$R_CH4))
      df$Est <- names(d_wt_pred[[x]])[y]
      df$Area <- names(d_wt_pred)[x]
      return(df)
    })

    names(R_CH4_dry_a) <- names(d_wt_pred[[x]])
    return(R_CH4_dry_a)
  })
  names(R_CH4_dry) <- names(d_wt_pred)

  # Invert data structure
  L_tot <- vector(mode = "list", length = length(d_wt))
  names(L_tot) <- names(YC)

  for (i in 1:length(L_tot)) {
    L_tot[[i]] <- list(Exp = left_join(R_CO2_dry[[i]]$Exp,
                                       R_CH4_dry[[i]]$Exp,
                                       by = c("t", "d_wt", "Est", "Area")) %>%
                         mutate(L_CO2 = R_CO2 * A_harv[[i]][1],
                                L_CH4 = R_CH4 * A_harv[[i]][1]) %>%
                         select(-c(R_CO2, R_CH4)) %>%
                         pivot_longer(cols = c(L_CO2, L_CH4), names_to = "source", values_to = "value"),
                       Min = left_join(R_CO2_dry[[i]]$Min,
                                       R_CH4_dry[[i]]$Min,
                                       by = c("t", "d_wt", "Est", "Area")) %>%
                           mutate(L_CO2 = R_CO2 * A_harv[[i]][2],
                                  L_CH4 = R_CH4 * A_harv[[i]][2]) %>%
                         select(-c(R_CO2, R_CH4)) %>%
                         pivot_longer(cols = c(L_CO2, L_CH4), names_to = "source", values_to = "value"),
                       Max = left_join(R_CO2_dry[[i]]$Max,
                                       R_CH4_dry[[i]]$Max,
                                       by = c("t", "d_wt", "Est", "Area")) %>%
                           mutate(L_CO2 = R_CO2 * A_harv[[i]][3],
                                  L_CH4 = R_CH4 * A_harv[[i]][3]) %>%
                          select(-c(R_CO2, R_CH4)) %>%
                         pivot_longer(cols = c(L_CO2, L_CH4), names_to = "source", values_to = "value"))

  }
  return(L_tot)
}

#' PeatlandSoilsEmissionsMod
#' @param input.dat UI forestry data
#' @param R_tot estimated emissions rates
#' @return L_indirect
#' @export
PeatlandSoilsEmissionsMod <- function(input.dat) {

  ## This function will estimate the emissions from the site following harvesting and restoration interventions
  ## assuming a non-linear restoration of ecosystem function parameterised by the user

  CO2_C <- 3.667 # Molecular weight ratio C to CO2
  CH4_CO2 <- 30.66667 # CH4 to CO2 conversion factor

  # Extract UI forestry WTD and modify to represent hydrological impact of harvesting
  # 45% reduction in WTD following removal of trees: crude estimate from Gaffney et al. 2018
  d_wt_drained <- map(input.dat[grep("Area", names(input.dat))], .f = "d_wt_drained") # User estimated average water table depth pre-restoration
  d_wt_drained <- lapply(d_wt_drained, FUN = function(x) {
    x <- x * (1 - 0.45)
    names(x) <- c("Exp", "Min", "Max")
    return(x)
  })

  # User estimated average water table depth post-restoration - default values from pristine bog controls set to 5 (1, 10) cm, Gaffney et al. (2018)
  d_wt_restored <- map(input.dat[grep("Area", names(input.dat))], .f = "d_wt_restored")

  R_CO2_dry <- lapply(d_wt_drained, FUN = function(x) {
    METAR21_CO2(CO2_C, x)
  })

  R_CO2_wet <- lapply(d_wt_restored, FUN = function(x) {
    METAR21_CO2(CO2_C, x)
  })

  R_CH4_dry <- lapply(d_wt_drained, FUN = function(x) {
    METAR21_CH4(CO2_C, x)
  })

  R_CH4_wet <- lapply(d_wt_restored, FUN = function(x) {
    METAR21_CH4(CO2_C, x)
  })

  # Invert data structure
  R_tot <- vector(mode = "list", length = length(d_wt_drained))
  names(R_tot) <- names(d_wt_drained)
  for (i in 1:length(R_tot)) {
    R_tot[[i]]$R_CO2_dry <- R_CO2_dry[[i]]$R_CO2
    R_tot[[i]]$R_CO2_wet <- R_CO2_wet[[i]]$R_CO2
    R_tot[[i]]$R_CH4_dry <- R_CH4_dry[[i]]$R_CH4
    R_tot[[i]]$R_CH4_wet <- R_CH4_wet[[i]]$R_CH4
  }

  conv_val <- 0.999 # convergence value required by arbitrary convergent function

  AVG_WTD <- T # if TRUE remove partitioning into flooded and unflooded days and assume that annual average WTD can be used
  # Averages will work so long as R_CX ~ WTD is approximately linear. This is true for CO2 in the range WTD = [0,1m] but NOT for CH4
  # Thus, average WTD will systematically under estimate methane emissions

  # Extract input variables for easy access
  A_harv <- map(input.dat[grep("Area", names(input.dat))], .f = "A_harv") # in units ha
  t_fallow <- map(input.dat[grep("Area", names(input.dat))], .f = "t_fallow") # time between felling and restoration
  t_restore <- map(input.dat[grep("Area", names(input.dat))], .f = "t_restore_peatland") # time to restoration of microbial function
  n_restore <- map(input.dat[grep("Area", names(input.dat))], .f = "n_restore_peatland") # shape parameter for restoration of microbial function

  ## Compute emissions rates from deforested, unrestored peatland

  if (!AVG_WTD) { # partition into flooded and unflooded days. 2 values of WTD needed in principle

    D_f <- 0 # Assume flooded days per year D_f = 0 for drained, unrestored peats
    pD_f <- D_f / 365

    CO2_dry <- lapply(seq_along(A_harv), FUN = function(x) {
      res <- A_harv[[x]] * ((R_tot[[x]]$R_CO2_wet * pD_f) + (R_tot[[x]]$R_CO2_dry * (1 - pD_f)))
      return(res)
    })
    names(CO2_dry) <- names(A_harv)

    CH4_dry <- lapply(seq_along(A_harv), FUN = function(x) {
      res <- A_harv[[x]] * ((R_tot[[x]]$R_CH4_wet * pD_f) + (R_tot[[x]]$R_CH4_dry * (1 - pD_f)))
      return(res)
    })
    names(CH4_dry) <- names(A_harv)

  } else { # take the annual average water table depth (robust in linear region of emissions function)

    CO2_dry <- lapply(seq_along(A_harv), FUN = function(x) {
      res <- A_harv[[x]] * R_tot[[x]]$R_CO2_dry
      return(res)
    })
    names(CO2_dry) <- names(A_harv)

    CH4_dry <- lapply(seq_along(A_harv), FUN = function(x) {
      res <- A_harv[[x]] * R_tot[[x]]$R_CH4_dry
      return(res)
    })
    names(CH4_dry) <- names(A_harv)

  }

  ## Compute emissions rates from deforested, restored peatland

  if (!AVG_WTD) { # partition into flooded and unflooded days. 2 values of WTD needed in principle

    if (peat_type[1] == 1) { # Acid bog selected
      D_f <- 178
    } else {
      D_f <- 169
    }
    pD_f <- D_f / 365

    CO2_wet <- lapply(seq_along(A_harv), FUN = function(x) {
      res <- A_harv[[x]] * ((R_tot[[x]]$R_CO2_wet * pD_f) + (R_tot[[x]]$R_CO2_dry * (1 - pD_f)))
      return(res)
    })
    names(CO2_wet) <- names(A_harv)

    CH4_wet <- lapply(seq_along(A_harv), FUN = function(x) {
      res <- A_harv[[x]] * ((R_tot[[x]]$R_CH4_wet * pD_f) + (R_tot[[x]]$R_CH4_dry * (1 - pD_f)))
      return(res)
    })
    names(CH4_wet) <- names(A_harv)

  } else { # take the annual average water table depth (robust in linear region of emissions function)

    CO2_wet <- lapply(seq_along(A_harv), FUN = function(x) {
      res <- A_harv[[x]] * R_tot[[x]]$R_CO2_wet
      return(res)
    })
    names(CO2_wet) <- names(A_harv)

    CH4_wet <- lapply(seq_along(A_harv), FUN = function(x) {
      res <- A_harv[[x]] * R_tot[[x]]$R_CH4_wet
      return(res)
    })
    names(CH4_wet) <- names(A_harv)

  }

  ## Interpolate emissions across restoration phase (arbitrary asymptotic function)
  L_CO2_peatland <- lapply(seq_along(A_harv), FUN = function(x) {
    res <- lapply(seq_along(A_harv[[x]]), FUN = function(y) {
      rest_dyn_mod(t = 1:t_restore[[x]][y],
                   n = n_restore[[x]][y],
                   ymin = CO2_dry[[x]][y], # pre-restoration CO2 emissions rate scaled by area (units CO2)
                   ymax = CO2_wet[[x]][y], # pre-restoration CO2 emissions rate scaled by area (units CO2)
                   convThresh = conv_val)
    })
    names(res) <- names(A_harv[[x]])
    return(res)
  })

  L_CH4_peatland <- lapply(seq_along(A_harv), FUN = function(x) {
    res <- lapply(seq_along(A_harv[[x]]), FUN = function(y) {
      rest_dyn_mod(t = 1:t_restore[[x]][y],
                   n = n_restore[[x]][y],
                   ymin = CH4_dry[[x]][y], # pre-restoration CO2 emissions rate scaled by area (units CO2)
                   ymax = CH4_wet[[x]][y], # pre-restoration CO2 emissions rate scaled by area (units CO2)
                   convThresh = conv_val)
    })
    names(res) <- names(A_harv[[x]])
    return(res)
  })

  # Add fallow period to time series
  L_CO2_peatland <- lapply(seq_along(L_CO2_peatland), FUN = function(x) {
    res <- lapply(seq_along(L_CO2_peatland[[x]]), FUN = function(y) {
      return(c(rep(CO2_dry[[x]][y], t_fallow[[x]][y]), unname(unlist(L_CO2_peatland[[x]][y]))))
    })
    names(res) <- names(L_CO2_peatland[[x]])
    return(res)
  })

  L_CH4_peatland <- lapply(seq_along(L_CH4_peatland), FUN = function(x) {
    res <- lapply(seq_along(L_CH4_peatland[[x]]), FUN = function(y) {
      return(c(rep(CH4_dry[[x]][y], t_fallow[[x]][y]), unname(unlist(L_CH4_peatland[[x]][y]))))
    })
    names(res) <- names(L_CH4_peatland[[x]])
    return(res)
  })

  # Extend time series to 500 years (if t_payback > 500, these will need to be extended again in the run script)
  L_CO2_peatland <- lapply(seq(L_CO2_peatland), FUN = function(x) {
    res <- lapply(seq_along(L_CO2_peatland[[x]]), FUN = function(y) {
      ext <- 501 - length(unlist(L_CO2_peatland[[x]][y]))
      return(unname(c(unlist(L_CO2_peatland[[x]][y]), rep(CO2_wet[[x]][y], ext))))
    })
    names(res) <- names(L_CO2_peatland[[x]])
    return(res)
  })

  L_CH4_peatland <- lapply(seq(L_CH4_peatland), FUN = function(x) {
    res <- lapply(seq_along(L_CH4_peatland[[x]]), FUN = function(y) {
      ext <- 501 - length(unlist(L_CH4_peatland[[x]][y]))
      return(unname(c(unlist(L_CH4_peatland[[x]][y]), rep(CH4_wet[[x]][y], ext))))
    })
    names(res) <- names(L_CH4_peatland[[x]])
    return(res)
  })

  # Update data structure to timeseries/dataframes
  L_CO2_peatland <- lapply(seq_along(L_CO2_peatland), FUN = function (x) {
    L_CO2_peatland_a <- lapply(seq_along(L_CO2_peatland[[x]]), FUN = function(y) {
      L <- data.frame(t = 0:(length(L_CO2_peatland[[x]][[y]])-1),
                      L_CO2 = L_CO2_peatland[[x]][[y]])
      return(L)
    })
    names(L_CO2_peatland_a) <- names(L_CO2_peatland[[x]])
    return(L_CO2_peatland_a)
  })

  L_CH4_peatland <- lapply(seq_along(L_CH4_peatland), FUN = function (x) {
    L_CH4_peatland_a <- lapply(seq_along(L_CH4_peatland[[x]]), FUN = function(y) {
      L <- data.frame(t = 0:(length(L_CH4_peatland[[x]][[y]])-1),
                      L_CH4 = L_CH4_peatland[[x]][[y]])
      return(L)
    })
    names(L_CH4_peatland_a) <- names(L_CH4_peatland[[x]])
    return(L_CH4_peatland_a)
  })

  names(L_CO2_peatland) <- names(A_harv)
  names(L_CH4_peatland) <- names(A_harv)

  L_peatland <- lapply(seq_along(L_CO2_peatland), FUN = function (x) {
    L_peatland_a <- lapply(seq_along(L_CO2_peatland[[x]]), FUN = function(y) {
      L <- L_CO2_peatland[[x]][[y]]
      L$L_CH4 <- L_CH4_peatland[[x]][[y]]$L_CH4
      return(L)
    })
    names(L_peatland_a) <- names(L_CO2_peatland[[x]])
    return(L_peatland_a)
  })

  names(L_peatland) <- names(A_harv)

  return(L_peatland)
}
