#' HarvestingManagementMod
#' @param forestry.dat UI forestry data
#' @param growthYield.dat growth and yield data (estimated from CARBINE runs)
#' @param S_forest 3PG output
#' @param alpha_df dataframe of decay rates / efficiencies
#' @return Estimated lifetime loss of carbon stored in forestry products
#' @export
HarvestingManagementMod <- function(forestry.dat,
                                        growthYield.dat,
                                        S_forest,
                                        alpha_df) {

  # This function models the emissions/losses associated with havesting and management
  # including discrete emissions due to mechanised interventions and time dependent
  # emissions due to the decomposition of forestry biomass

  # Extract input variables for easy access
  CO2_C <- 3.667 # Molecular weight ratio C to CO2
  A_harv <- map(forestry.dat[grep("Area", names(forestry.dat))], "A_harv")
  t_harv <- map(forestry.dat[grep("Area", names(forestry.dat))], "t_harv")
  Spp <- map(forestry.dat[grep("Area", names(forestry.dat))], .f = "species")
  YC <- map(forestry.dat[grep("Area", names(forestry.dat))], .f = "YC") # if not passed by user, already computed elsewhere from Growth and yield tables
  t_fallow <- map(forestry.dat[grep("Area", names(forestry.dat))], .f = "t_fallow")

  # Emissions factors
  E_transport <- rep(list(forestry.dat$Emissions$E_transport / 1e6), length = length(grep("Area", names(forestry.dat)))) # convert from g CO2 km-1 to t CO2 km-3
  E_harv <- rep(list(forestry.dat$Emissions$E_harv / 1e3), length = length(grep("Area", names(forestry.dat)))) # convert from g CO2 m-3 to t CO2 m-3
  E_mulch <- rep(list(forestry.dat$Emissions$E_mulch / 1e3), length = length(grep("Area", names(forestry.dat)))) # convert from kg CO2 ha-1 to t CO2 ha-1
  E_dam <- rep(list(forestry.dat$Emissions$E_dam / 1e3), length = length(grep("Area", names(forestry.dat)))) # convert from kg CO2 ha-1 to t CO2 ha-1
  E_bund <- rep(list(forestry.dat$Emissions$E_bund / 1e3), length = length(grep("Area", names(forestry.dat)))) # convert from kg CO2 ha-1 to t CO2 ha-1
  E_smooth <- rep(list(forestry.dat$Emissions$E_smooth / 1e3), length = length(grep("Area", names(forestry.dat)))) # convert from kg CO2 ha-1 to t CO2 ha-1
  E_turf_import <- rep(list(forestry.dat$Emissions$E_turf_import / 1e3), length = length(grep("Area", names(forestry.dat)))) # convert from kg CO2 ha-1 to t CO2 ha-1
  E_turf_local <- rep(list(forestry.dat$Emissions$E_turf_local / 1e3), length = length(grep("Area", names(forestry.dat)))) # convert from kg CO2 ha-1 to t CO2 ha-1
  E_fert <- rep(list(forestry.dat$Emissions$E_fert / 1e3), length = length(grep("Area", names(forestry.dat)))) # convert from kg CO2 ha-1 to t CO2 ha-1

  # Management strategy
  timber_removed <- map(forestry.dat[grep("Area", names(forestry.dat))], "timber_removed")
  mulch <- map(forestry.dat[grep("Area", names(forestry.dat))], "mulch")
  mulch <- lapply(seq_along(mulch), FUN = function(x) {
    if (is.null(mulch[[x]])) { # mulch drop down depends on whether user has selected to leave timber in situ
      mulch[[x]] <- c("Exp" = 2, "Min" = NA, "Max" = NA)
    }
    return(mulch[[x]])
  })
  names(mulch) <- names(timber_removed)
  dam <- map(forestry.dat[grep("Area", names(forestry.dat))], "dam")
  bund <- map(forestry.dat[grep("Area", names(forestry.dat))], "bund")
  smooth <- map(forestry.dat[grep("Area", names(forestry.dat))], "smooth")
  turf_import <- map(forestry.dat[grep("Area", names(forestry.dat))], "turf_import")
  turf_local <- map(forestry.dat[grep("Area", names(forestry.dat))], "turf_local")
  fert <- map(forestry.dat[grep("Area", names(forestry.dat))], "fert")

  # Set emissions factors to zero for management strategies not selected
  E_transport <- lapply(seq_along(E_transport), FUN = function(x) {
    if (timber_removed[[x]][1] == 2) { # if leaving timber in situ, transport emissions can be omitted
      E_transport[[x]] <- c("Exp" = 0, "Min" = 0, "Max" = 0)
    }
    return(E_transport[[x]])
  })
  names(E_transport) <- names(YC)

  E_harv <- lapply(seq_along(E_harv), FUN = function(x) {
    if (mulch[[x]][1] == 1) { # if mulching, harvesting emissions can be omitted since trees are also felled by mulching tractor
      E_harv[[x]] <- c("Exp" = 0, "Min" = 0, "Max" = 0)
    }
    return(E_harv[[x]])
  })
  names(E_harv) <- names(YC)

  E_mulch <- lapply(seq_along(E_mulch), FUN = function(x) {
    if (mulch[[x]][1] == 2) { # if not mulching emissions can be omitted
      E_mulch[[x]] <- c("Exp" = 0, "Min" = 0, "Max" = 0)
    }
    return(E_mulch[[x]])
  })
  names(E_mulch) <- names(YC)

  E_dam <- lapply(seq_along(E_dam), FUN = function(x) {
    if (dam[[x]][1] == 2) { # if not damming emissions can be omitted
      E_dam[[x]] <- c("Exp" = 0, "Min" = 0, "Max" = 0)
    }
    return(E_dam[[x]])
  })
  names(E_dam) <- names(YC)

  E_bund <- lapply(seq_along(E_bund), FUN = function(x) {
    if (bund[[x]][1] == 2) { # if not bunding emissions can be omitted
      E_bund[[x]] <- c("Exp" = 0, "Min" = 0, "Max" = 0)
    }
    return(E_bund[[x]])
  })
  names(E_bund) <- names(YC)

  E_smooth <- lapply(seq_along(E_smooth), FUN = function(x) {
    if (smooth[[x]][1] == 2) { # if not smoothing emissions can be omitted
      E_smooth[[x]] <- c("Exp" = 0, "Min" = 0, "Max" = 0)
    }
    return(E_smooth[[x]])
  })
  names(E_smooth) <- names(YC)

  E_turf_import <- lapply(seq_along(E_turf_import), FUN = function(x) {
    if (turf_import[[x]][1] == 2) { # if not importing turf emissions can be omitted
      E_turf_import[[x]] <- c("Exp" = 0, "Min" = 0, "Max" = 0)
    }
    return(E_turf_import[[x]])
  })
  names(E_turf_import) <- names(YC)

  E_turf_local <- lapply(seq_along(E_turf_local), FUN = function(x) {
    if (turf_local[[x]][1] == 2) { # if not locally sourcing turf emissions can be omitted
      E_turf_local[[x]] <- c("Exp" = 0, "Min" = 0, "Max" = 0)
    }
    return(E_turf_local[[x]])
  })
  names(E_turf_local) <- names(YC)

  E_fert <- lapply(seq_along(E_fert), FUN = function(x) {
    if (fert[[x]][1] == 2) { # if not locally sourcing turf emissions can be omitted
      E_fert[[x]] <- c("Exp" = 0, "Min" = 0, "Max" = 0)
    }
    return(E_fert[[x]])
  })
  names(E_fert) <- names(YC)

  # Biofuel handling
  r_CBiomass <- map(forestry.dat[grep("Area", names(forestry.dat))], "r_CBiomass")
  e_felled_biofuel <- map(forestry.dat[grep("Area", names(forestry.dat))], "e_felled_biofuel")
  e_biofuel_plant <- map(forestry.dat[grep("Area", names(forestry.dat))], "e_biofuel_plant")
  e_biofuel_plant <- lapply(e_biofuel_plant, FUN = function(x) {
    if (is.null(x)) {
      return(c(Exp = 0, Min = 0, Max = 0)) # add zero vector in case p_biofuel not passed
    } else {
      return(x)
    }
  })

  E_grid_mix <- rep(list(forestry.dat$Emissions$E_grid_mix), length = length(grep("Area", names(forestry.dat))))
  names(E_grid_mix) <- names(r_CBiomass)

  # Wood product handling
  d_biofuel <- map(forestry.dat[grep("Area", names(forestry.dat))], "d_biofuel")
  d_wpF <- map(forestry.dat[grep("Area", names(forestry.dat))], "d_wpF")
  d_wpM <- map(forestry.dat[grep("Area", names(forestry.dat))], "d_wpM")
  d_wpS <- map(forestry.dat[grep("Area", names(forestry.dat))], "d_wpS")

  # Match object structure for downstream manipulations
  d_wp <- lapply(seq_along(d_wpF), FUN = function(x) {
    if (timber_removed[[x]][1]==1) {
      d_wp_a <- list(Exp = c(Biofuel = unname(d_biofuel[[x]][1]),
                             wpF = unname(d_wpF[[x]][1]),
                             wpM = unname(d_wpM[[x]][1]),
                             wpS = unname(d_wpS[[x]][1])),
                     Min = c(Biofuel = unname(d_biofuel[[x]][2]),
                             wpF = unname(d_wpF[[x]][2]),
                             wpM = unname(d_wpM[[x]][2]),
                             wpS = unname(d_wpS[[x]][2])),
                     Max = c(Biofuel = unname(d_biofuel[[x]][3]),
                             wpF = unname(d_wpF[[x]][3]),
                             wpM = unname(d_wpM[[x]][3]),
                             wpS = unname(d_wpS[[x]][3])))
    } else {
      d_wp_a <- NULL
    }

    return(d_wp_a)
  })

  names(d_wp) <- names(r_CBiomass)

  # Get exponential decay rates for wood products - this now set in runscript and passed as argument
  # alpha_df <- read_xlsx("Templates/alpha_wp.xlsx",
  #                        sheet = "Sheet1",
  #                        range = "A1:F10",
  #                        progress = F)

  # Get decay rate parameters
  alpha <- lapply(seq_along(d_wp), FUN = function(x) {
    if (all(unlist(d_wp[[x]]) == 0)) {
      ## if distance to processing sites not passed, assume forestry products left in situ
      alpha_a <- alpha_df %>%
        filter(Type == "Unprocessed")

      if (mulch[[x]][1]==1) {
        alpha_a <- alpha_a %>%
          filter(Compartment %in% c("Roots", "Mulch", "Foliage"))
      } else {
        alpha_a <- alpha_a %>%
          filter(Compartment != "Mulch")
      }
    } else {
      alpha_a <- alpha_df %>%
        filter(Type == "Processed" | Compartment == "Foliage" | Compartment == "Roots")
    }
    alpha_wp <- alpha_a$alpha
    names(alpha_wp) <- stringr::str_replace(alpha_a$Var, "alpha_", "")
    return(alpha_wp)
  })

  names(alpha) <- names(d_wp)

  # Get decay efficiency parameters (set to 1 by default)
  delta <- lapply(seq_along(d_wp), FUN = function(x) {
    if (all(unlist(d_wp[[x]]) == 0)) {
      ## if distance to processing sites not passed, assume forestry products left in situ
      delta_a <- alpha_df %>%
        filter(Type == "Unprocessed")

      if (mulch[[x]][1]==1) {
        delta_a <- delta_a %>%
          filter(Compartment %in% c("Roots", "Mulch", "Foliage"))
      } else {
        delta_a <- delta_a %>%
          filter(Compartment != "Mulch")
      }

      delta_wp <- delta_a$delta
      names(delta_wp) <- stringr::str_replace(delta_a$Var, "alpha_", "")

    } else {
      delta_a <- alpha_df %>%
        filter(Type == "Processed" | Compartment == "Foliage" | Compartment == "Roots")

      delta_wp <- delta_a$delta
      names(delta_wp) <- stringr::str_replace(delta_a$Var, "alpha_", "")

      delta_wp <- c(Biofuel=1, delta_wp) # add biofuel for carbon partitioning (always equal to 1 as all material is burnt)
    }

    return(delta_wp)
  })

  names(delta) <- names(d_wp)

  species <- c("Scots_pine", "Sitka_spruce")

  # Get above ground volume for harvesting/mulching emissions
  V_harv <- lapply(seq_along(A_harv), FUN = function(x) {
    V_harv_a <- lapply(seq_along(A_harv[[x]]), FUN = function(y) {
      Spp_a <- species[Spp[[x]][1]]
      YC_a <- YC[[x]][y]
      t_harv_a <- t_harv[[x]][y]

      ## Deal with missing YC values from GY table
      YC_avail <- unlist(growthYield.dat %>% filter(Spp == Spp_a) %>% select(YC) %>% unique())

      ### If YC_a is not available, set to closest value.
      ### If equidistant from multiple available values, maximum is used (conservative estimate of payback time)
      YC_a <- max(YC_avail[which(abs(YC_a - YC_avail) == min(abs(YC_a - unlist(YC_avail))))])

      ### If t_harv_a is not available, set to min/max as appropriate (unlikely)
      t_harv_min <- min((growthYield.dat %>% filter(Spp == Spp_a, YC == YC_a))$Age)
      t_harv_max <- max((growthYield.dat %>% filter(Spp == Spp_a, YC == YC_a))$Age)

      t_harv_a <- min(max(t_harv_a, t_harv_min), t_harv_max)

      V_a <- growthYield.dat %>%
        filter(Spp == Spp_a,
               YC == floor(YC_a), # floor required since if YC or t_harv implicitly coerced to floats, filter will fail!
               Age == floor(t_harv_a)) %>%
        select(V_a)

      V_harv_a <- V_a * A_harv[[x]][[y]]

      return(V_harv_a)
    })

    V_harv_a <- unlist(V_harv_a)
    names(V_harv_a) <- names(A_harv[[x]])
    return(V_harv_a)
  })

  names(V_harv) <- names(A_harv)

  # Compute harvesting emissions
  L_harv <- lapply(seq_along(E_harv), FUN = function(x) {
    L_harv_a <- lapply(seq_along(E_harv[[x]]), FUN = function(y) {

      L_harv_a <- V_harv[[x]][y] * E_harv[[x]][y]

      L_harv_a <- data.frame(t = 0,
                             L_harv = unname(L_harv_a))
      return(L_harv_a)
    })

    names(L_harv_a) <- names(E_harv[[x]])
    return(L_harv_a)
  })

  names(L_harv) <- names(E_harv)

  # Compute mulching emissions
  L_mulch <- lapply(seq_along(E_mulch), FUN = function(x) {
    L_mulch_a <- lapply(seq_along(E_mulch[[x]]), FUN = function(y) {

      L_mulch_a <- A_harv[[x]][y] * E_mulch[[x]][y]

      L_mulch_a <- data.frame(t = 0,
                              L_mulch = unname(L_mulch_a))
      return(L_mulch_a)
    })

    names(L_mulch_a) <- names(E_mulch[[x]])
    return(L_mulch_a)
  })

  names(L_mulch) <- names(E_mulch)

  # Compute damming emissions
  L_dam <- lapply(seq_along(E_dam), FUN = function(x) {
    L_dam_a <- lapply(seq_along(E_dam[[x]]), FUN = function(y) {

      L_dam_a <- A_harv[[x]][y] * E_dam[[x]][y]

      L_dam_a <- data.frame(t = t_fallow[[x]][y],
                            L_dam = unname(L_dam_a))
      return(L_dam_a)
    })

    names(L_dam_a) <- names(E_dam[[x]])
    return(L_dam_a)
  })

  names(L_dam) <- names(E_dam)

  # Compute bunding emissions
  L_bund <- lapply(seq_along(E_bund), FUN = function(x) {
    L_bund_a <- lapply(seq_along(E_bund[[x]]), FUN = function(y) {

      L_bund_a <- A_harv[[x]][y] * E_bund[[x]][y]

      L_bund_a <- data.frame(t = t_fallow[[x]][y],
                             L_bund = unname(L_bund_a))
      return(L_bund_a)
    })

    names(L_bund_a) <- names(E_bund[[x]])
    return(L_bund_a)
  })

  names(L_bund) <- names(E_bund)

  # Compute smoothing emissions
  L_smooth <- lapply(seq_along(E_smooth), FUN = function(x) {
    L_smooth_a <- lapply(seq_along(E_smooth[[x]]), FUN = function(y) {

      L_smooth_a <- A_harv[[x]][y] * E_smooth[[x]][y]

      L_smooth_a <- data.frame(t = t_fallow[[x]][y],
                               L_smooth = unname(L_smooth_a))
      return(L_smooth_a)
    })

    names(L_smooth_a) <- names(E_smooth[[x]])
    return(L_smooth_a)
  })

  names(L_smooth) <- names(E_smooth)

  # Compute turfing emissions (imported)
  L_turf_import <- lapply(seq_along(E_turf_import), FUN = function(x) {
    L_turf_import_a <- lapply(seq_along(E_turf_import[[x]]), FUN = function(y) {

      L_turf_import_a <- A_harv[[x]][y] * E_turf_import[[x]][y]

      L_turf_import_a <- data.frame(t = t_fallow[[x]][y],
                                    L_turf_import = unname(L_turf_import_a))
      return(L_turf_import_a)
    })

    names(L_turf_import_a) <- names(E_turf_import[[x]])
    return(L_turf_import_a)
  })

  names(L_turf_import) <- names(E_turf_import)

  # Compute turfing emissions (local)
  L_turf_local <- lapply(seq_along(E_turf_local), FUN = function(x) {
    L_turf_local_a <- lapply(seq_along(E_turf_local[[x]]), FUN = function(y) {

      L_turf_local_a <- A_harv[[x]][y] * E_turf_local[[x]][y]

      L_turf_local_a <- data.frame(t = t_fallow[[x]][y],
                                   L_turf_local = unname(L_turf_local_a))
      return(L_turf_local_a)
    })

    names(L_turf_local_a) <- names(E_turf_local[[x]])
    return(L_turf_local_a)
  })

  names(L_turf_local) <- names(E_turf_local)

  # Compute fertilization emissions
  L_fert <- lapply(seq_along(E_fert), FUN = function(x) {
    L_fert_a <- lapply(seq_along(E_fert[[x]]), FUN = function(y) {

      L_fert_a <- A_harv[[x]][y] * E_fert[[x]][y]

      L_fert_a <- data.frame(t = t_fallow[[x]][y],
                             L_fert = unname(L_fert_a))
      return(L_fert_a)
    })

    names(L_fert_a) <- names(E_fert[[x]])
    return(L_fert_a)
  })

  names(L_fert) <- names(E_fert)

  # Extract carbon content of the forestry at harvesting (above + below ground)
  C_forest_tot <- lapply(seq_along(S_forest), FUN = function(x) {
    res <- lapply(seq_along(S_forest[[x]]), FUN = function(y) {
      S_forest[[x]][[y]] %>%
        filter(t<=0) %>%
        summarise(C = sum(NPP))
    })
    res <- unlist(res)
    names(res) <- names(S_forest[[x]])
    return(res)
  })

  names(C_forest_tot) <- grep("Area", names(forestry.dat), value=T)

  # Estimate above ground Carbon from CARBINE ratios
  C_forest <- lapply(seq_along(C_forest_tot), FUN = function(x) {
    res <- lapply(seq_along(C_forest_tot[[x]]), FUN = function(y) {

      Spp_a <- species[Spp[[x]][1]]
      YC_a <- YC[[x]][y]
      t_harv_a <- t_harv[[x]][y]

      ## Deal with missing YC values from GY table
      YC_avail <- unlist(growthYield.dat %>% filter(Spp == Spp_a) %>% select(YC) %>% unique())

      ### If YC_a is not available, set to closest value.
      ### If equidistant from multiple available values, maximum is used (conservative estimate of payback time)
      YC_a <- max(YC_avail[which(abs(YC_a - YC_avail) == min(abs(YC_a - unlist(YC_avail))))])

      ### If t_harv_a is not available, set to min/max as appropriate (unlikely)
      t_harv_min <- min((growthYield.dat %>% filter(Spp == Spp_a, YC == YC_a))$Age)
      t_harv_max <- max((growthYield.dat %>% filter(Spp == Spp_a, YC == YC_a))$Age)

      t_harv_a <- min(max(t_harv_a, t_harv_min), t_harv_max)

      rho_ag <- growthYield.dat %>%
        filter(Spp == Spp_a,
               YC == floor(YC_a),
               Age == floor(t_harv_a)) %>%
        select(rho_ag)

      C_forest_ag <- rho_ag * C_forest_tot[[x]][y]

      return(C_forest_ag)
    })
    res <- unlist(res)
    names(res) <- names(S_forest[[x]])
    return(res)
  })

  names(C_forest) <- grep("Area", names(forestry.dat), value=T)

  # Proportion of biomass in different decay compartments
  rho_wp <- lapply(seq_along(C_forest), FUN = function(x) {
    rho_wp_a <- lapply(seq_along(C_forest[[x]]), FUN = function(y) {
      Spp_a <- species[Spp[[x]][1]]
      YC_a <- YC[[x]][y]
      t_harv_a <- t_harv[[x]][y]

      ## Deal with missing YC values from GY table
      YC_avail <- unlist(growthYield.dat %>% filter(Spp == Spp_a) %>% select(YC) %>% unique())

      ### If YC_a is not available, set to closest value.
      ### If equidistant from multiple available values, maximum is used (conservative estimate of payback time)
      YC_a <- max(YC_avail[which(abs(YC_a - YC_avail) == min(abs(YC_a - unlist(YC_avail))))])

      ### If t_harv_a is not available, set to min/max as appropriate (unlikely)
      t_harv_min <- min((growthYield.dat %>% filter(Spp == Spp_a, YC == YC_a))$Age)
      t_harv_max <- max((growthYield.dat %>% filter(Spp == Spp_a, YC == YC_a))$Age)

      t_harv_a <- min(max(t_harv_a, t_harv_min), t_harv_max)

      if (timber_removed[[x]][1]==1) {
        rho <- growthYield.dat %>%
          filter(Spp == Spp_a,
                 YC == YC_a,
                 Age == t_harv_a) %>%
          select(rho_Biofuel, rho_wpF, rho_wpM, rho_wpS, rho_wpO, rho_f, rho_r)
      } else { # timber left in situ

        if (mulch[[x]][1] == 1) { # mulched
          rho <- growthYield.dat %>%
            filter(Spp == Spp_a,
                   YC == YC_a,
                   Age == t_harv_a) %>%
            select(rho_r, rho_m, rho_f)
        } else { # not mulched
          rho <- growthYield.dat %>%
            filter(Spp == Spp_a,
                   YC == YC_a,
                   Age == t_harv_a) %>%
            select(rho_r, rho_s, rho_b, rho_f)
        }
      }

      return(rho)
    })

    names(rho_wp_a) <- names(C_forest[[x]])
    return(rho_wp_a)
  })

  names(rho_wp) <- names(C_forest)

  # Compute carbon content of the decomposable proportion of the material (i.e. accounting for the decay efficiency)
  C_wp <- lapply(seq_along(C_forest), FUN = function(x) {
    C_wp_a <- lapply(seq_along(C_forest[[x]]), FUN = function(y) {
      C <- unlist(rho_wp[[x]][[y]]) * delta[[x]] * C_forest[[x]][[y]]
      names(C) <- stringr::str_replace(names(C), "rho_", "")
      return(C)
    })
    names(C_wp_a) <- names(C_forest[[x]])
    return(C_wp_a)
  })

  names(C_wp) <- names(C_forest)

  # Convert from units C to units CO2
  CO2_wp <- lapply(seq_along(C_wp), FUN = function (x) {
    CO2_wp_a <- lapply(seq_along(C_wp[[x]]), FUN = function(y) {
      CO2 <- unlist(C_wp[[x]][[y]]) * CO2_C
      return(CO2)
    })
    names(CO2_wp_a) <- names(C_wp[[x]])
    return(CO2_wp_a)
  })

  names(CO2_wp) <- names(C_wp)

  # Convert from units C to units Biomass (select only wood products as these are used for transport/biofuels)
  B_wp <- lapply(seq_along(C_wp), FUN = function (x) {
    B_wp_a <- lapply(seq_along(C_wp[[x]]), FUN = function(y) {
      B <- unlist(C_wp[[x]][[y]]) / r_CBiomass[[x]][[y]]
      wps <- c("Biofuel", "wpF", "wpM", "wpS")
      if (!sum(names(B) %in% wps)) { # no wps detected
        B <- NULL
      } else {
        B <- B[names(B) %in% wps]
      }
      return(B)
    })
    names(B_wp_a) <- names(C_wp[[x]])
    return(B_wp_a)
  })

  names(B_wp) <- names(C_wp)

  # Model exponential decay in wood product stored CO2
  if (0) {
    ## Simulate for long enough that the slowest decaying product reaches a proportion C_min of initial value
    C_min <- 0.01
    tMax <- -max(unlist(alpha)) * log(C_min)
  } else {
    ## Set max simulation time to 500
    tMax <- 500
  }

  tt <- 0:signif(tMax, digits = 1) # round to nearest 100
  CO2_wp_decay <- lapply(seq_along(CO2_wp), FUN = function (x) {
    CO2_wp_decay_a <- lapply(seq_along(CO2_wp[[x]]), FUN = function(y) {

      if (timber_removed[[x]][1] == 1) { # timber removed from site
        res <- data.frame(t=tt,
                          S_wpF=CO2_wp[[x]][[y]]["wpF"] * exp(-tt*alpha[[x]]["wpF"]),
                          S_wpM=CO2_wp[[x]][[y]]["wpM"] * exp(-tt*alpha[[x]]["wpM"]),
                          S_wpS=CO2_wp[[x]][[y]]["wpS"] * exp(-tt*alpha[[x]]["wpS"]),
                          S_wpO=CO2_wp[[x]][[y]]["wpO"] * exp(-tt*alpha[[x]]["wpO"]),
                          S_f=CO2_wp[[x]][[y]]["f"] * exp(-tt*alpha[[x]]["f"]),
                          S_r=CO2_wp[[x]][[y]]["r"] * exp(-tt*alpha[[x]]["r"]))

        res <- res %>%
          mutate(L_wpF = replace_na(lag(S_wpF, n = 1) - S_wpF, 0),
                 L_wpM = replace_na(lag(S_wpM, n = 1) - S_wpM, 0),
                 L_wpS = replace_na(lag(S_wpS, n = 1) - S_wpS, 0),
                 L_wpO = replace_na(lag(S_wpO, n = 1) - S_wpO, 0),
                 L_f = replace_na(lag(S_f, n = 1) - S_f, 0),
                 L_r = replace_na(lag(S_r, n = 1) - S_r, 0)) %>%
          select(t, L_wpF, L_wpM, L_wpS, L_wpO, L_f, L_r)
      } else { # timber left in situ

        if (mulch[[x]][1] == 1) { # mulched
          res <- data.frame(t=tt,
                            S_r=CO2_wp[[x]][[y]]["r"] * exp(-tt*alpha[[x]]["r"]),
                            S_m=CO2_wp[[x]][[y]]["m"] * exp(-tt*alpha[[x]]["m"]),
                            S_f=CO2_wp[[x]][[y]]["f"] * exp(-tt*alpha[[x]]["f"]))

          res <- res %>%
            mutate(L_r = replace_na(lag(S_r, n = 1) - S_r, 0),
                   L_m = replace_na(lag(S_m, n = 1) - S_m, 0),
                   L_f = replace_na(lag(S_f, n = 1) - S_f, 0)) %>%
            select(t, L_r, L_m, L_f)
        } else { # not mulched
          res <- data.frame(t=tt,
                            S_r=CO2_wp[[x]][[y]]["r"] * exp(-tt*alpha[[x]]["r"]),
                            S_s=CO2_wp[[x]][[y]]["s"] * exp(-tt*alpha[[x]]["s"]),
                            S_b=CO2_wp[[x]][[y]]["b"] * exp(-tt*alpha[[x]]["b"]),
                            S_f=CO2_wp[[x]][[y]]["f"] * exp(-tt*alpha[[x]]["f"]))

          res <- res %>%
            mutate(L_r = replace_na(lag(S_r, n = 1) - S_r, 0),
                   L_s = replace_na(lag(S_s, n = 1) - S_s, 0),
                   L_b = replace_na(lag(S_b, n = 1) - S_b, 0),
                   L_f = replace_na(lag(S_f, n = 1) - S_f, 0)) %>%
            select(t, L_r, L_s, L_b, L_s)
        }

      }

      return(res)
    })
    names(CO2_wp_decay_a) <- names(CO2_wp[[x]])
    return(CO2_wp_decay_a)
  })

  names(CO2_wp_decay) <- names(CO2_wp)

  ## Esimate transportion emissions B*D*E
  L_Twp <- lapply(seq_along(B_wp), FUN = function (x) {
    L_Twp_a <- lapply(seq_along(B_wp[[x]]), FUN = function(y) {
      if (timber_removed[[x]][1] == 1) {
        L_T <- B_wp[[x]][[y]] * d_wp[[x]][[y]] * E_transport[[x]][y]
        L_T <- data.frame(t = 0,
                          L_TBiofuel = unname(L_T["Biofuel"]),
                          L_TwpF = unname(L_T["wpF"]),
                          L_TwpM = unname(L_T["wpM"]),
                          L_TwpS = unname(L_T["wpS"]))
      } else {
        L_T <- data.frame(t = 0,
                          L_TBiofuel = 0,
                          L_TwpF = 0,
                          L_TwpM = 0,
                          L_TwpS = 0)
      }

      return(L_T)
    })
    names(L_Twp_a) <- names(B_wp[[x]])
    return(L_Twp_a)
  })

  names(L_Twp) <- names(B_wp)

  # multiply the power value of the biofuel by the efficiency of the biomass power plant for global efficiency value
  e_biofuel <- list_op(l1 = e_felled_biofuel,
                       l2 = e_biofuel_plant,
                       func = "*")

  L_biofuel <- list_op(l1 = lapply(seq_along(CO2_wp), FUN = function(x) { # Extract biofuel from CO2 content object
                                     unlist(lapply(CO2_wp[[x]], FUN=function(y) unname(y["Biofuel"])))
                                   }),
                       l2 = list_op(l1 = e_biofuel, # power value of forestry biomass X efficiency of biomass power plant
                                    l2 = lapply(seq_along(B_wp), FUN = function(x) { # Extract biofuel from biomass object
                                      unlist(lapply(B_wp[[x]], FUN=function(y) unname(y["Biofuel"])))
                                    }),
                                    l3 = E_grid_mix, # emissions factor of counterfactual
                                    func = "*"),
                       func = "-")

  L_biofuel <- lapply(seq_along(L_biofuel), FUN = function (x) {
    L_biofuel_a <- lapply(seq_along(L_biofuel[[x]]), FUN = function(y) {
      L_b <- data.frame(t = 0,
                        L_Biofuel = unname(L_biofuel[[x]][y]))
      return(L_b)
    })
    names(L_biofuel_a) <- names(L_biofuel[[x]])
    return(L_biofuel_a)
  })

  names(L_biofuel) <- names(B_wp)

  # Invert data structure
  L_forest <- vector(mode = "list", length=length(grep("Area", names(forestry.dat))))
  names(L_forest) <- grep("Area", names(forestry.dat), value=T)
  for (i in 1:length(L_forest)) {

    L_forest[[i]]$L_harv <- L_harv[[i]]
    L_forest[[i]]$L_mulch <- L_mulch[[i]]
    L_forest[[i]]$L_terraform <- NULL
    L_forest[[i]]$L_dam <- L_dam[[i]]
    L_forest[[i]]$L_bund <- L_bund[[i]]
    L_forest[[i]]$L_smooth <- L_smooth[[i]]
    L_forest[[i]]$L_turf_import <- L_turf_import[[i]]
    L_forest[[i]]$L_turf_local <- L_turf_local[[i]]
    L_forest[[i]]$L_fert <- L_fert[[i]]
    L_forest[[i]]$L_wp <- CO2_wp_decay[[i]]
    L_forest[[i]]$L_Twp <- L_Twp[[i]]
    L_forest[[i]]$L_biofuel <- L_biofuel[[i]]
  }

  return(L_forest)
}

