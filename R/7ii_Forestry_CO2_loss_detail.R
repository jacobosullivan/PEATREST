## 7ii. Forestry CO2 loss - detail
#' Forestry_CO2_loss_detail
#' @param core.dat UI data
#' @param forestry.dat UI forestry data
#' @return Estimated lifetime loss of carbon sequestration
#' @export
Forestry_CO2_loss_detail <- function(core.dat,
                                     forestry.dat) {

  # THIS FUNCTION...
  CO2_C <- 3.667 # Molecular weight ratio C to CO2

  # JDEBUG: REMOVE THIS FROM HERE OR LEAVE IN WINDFARM LCA?

  # Proportion of biomass used in biofuel and long, medium and short lived wood products
  # These may need to be area specific requiring different downstream implementation (i.e. using list_op)
  rho_felled <- list(biofuel = 1, wp_long = 0, wp_med = 0, wp_short = 0) # THIS WILL NEED TO BE A USER INPUT
  rho_replant <- list(biofuel = 1, wp_long = 0, wp_med = 0, wp_short = 0) # THIS WILL NEED TO BE A USER INPUT

  # exponential decay rate of long, medium and short lived wood products
  alpha_wp <- list(wp_long = c(Exp = 0.01, Min = 0.01, Max = 0.01),
                   wp_med = c(Exp = 0.1, Min = 0.1, Max = 0.1),
                   wp_short = c(Exp = 1, Min = 1, Max = 1)) # THIS COULD BE A USER INPUT OR COULD BE ESTIMATED FROM LITERATURE

  # average transportation distances of long, medium and short lived wood products
  D_wp <- list(wp_long = c(Exp = 100, Min = 100, Max = 100),
               wp_med = c(Exp = 100, Min = 100, Max = 100),
               wp_short = c(Exp = 100, Min = 100, Max = 100)) # THIS SHOULD BE A USER INPUT

  # average transportation emissions factors of long, medium and short lived wood products
  E_wp <- list(wp_long = c(Exp = 0.1, Min = 0.1, Max = 0.1),
               wp_med = c(Exp = 0.1, Min = 0.1, Max = 0.1),
               wp_short = c(Exp = 0.1, Min = 0.1, Max = 0.1)) # THIS COULD BE A USER INPUT OR COULD BE ESTIMATED FROM LITERATURE

  ## Loss of carbon sequestration due to felling of forestry for wind farm
  C_wpry <- C_sequest_in_trees(core.dat,
                                   forestry.dat)

  A_felled <- list_op(l1 = map(forestry.dat[grep("Area", names(forestry.dat))], "n_turb"),
                      l2 = map(forestry.dat[grep("Area", names(forestry.dat))], "A_harv_turb"),
                      func = "*")

  A_replant <- list_op(l1 = map(forestry.dat[grep("Area", names(forestry.dat))], "n_turb"),
                       l2 = map(forestry.dat[grep("Area", names(forestry.dat))], "A_replant_turb"),
                       func = "*")

  C_seq_loss_felled <- lapply(list_op(l1 = A_felled,
                                      l2 = C_wpry$seq_pot,
                                      func = "*"),
                              FUN = function (x) x * CO2_C)

  # SPREADSHEET ERROR
  # Replanted sequestration should consider a different per area total due to e.g. the age modifier
  C_seq_gain_replant <- lapply(list_op(l1 = A_replant,
                                       l2 = C_wpry$seq_pot_replant,
                                       func = "*"),
                               FUN = function (x) x * CO2_C)

  C_seq_loss_net <- list_op(l1 = C_seq_loss_felled,
                            l2 = C_seq_gain_replant,
                            func = "-")


  ## Cleared forest floor emissions

  # Extract and restructure data for easy access
  # JDebug: replace the following with this:
  # e.g. E_fossil_mix <- rep(list(core.dat$Counterfactual$E_fossil_mix), length = length(grep("Area", names(forestry.dat))))

  C_seq_soil_df <- C_seq_soil()
  soil_seq_rate <- list()
  t_wf_by_area <- list()
  emissions_from_felling <- list()
  emissions_from_transport <- list()
  fossil_fuel_emissions_factor <- list()
  dist_fuel_plant <- list()

  ii <- 1
  for (i in grep("Area", names(forestry.dat))) {
    soil_seq_rate[[ii]] <- C_seq_soil_df$seq_rate[which(C_seq_soil_df$Soil_type == forestry.dat[[i]]$soil_type[1])]
    t_wf_by_area[[ii]] <- core.dat$Windfarm$t_wf
    emissions_from_felling[[ii]] <- forestry.dat$Emissions$E_harv / 1e6 # unit conversion
    emissions_from_transport[[ii]] <- forestry.dat$Emissions$E_transport / 1e6 # unit conversion
    fossil_fuel_emissions_factor[[ii]] <- core.dat$Counterfactual$E_fossil_mix
    dist_fuel_plant[[ii]] <- forestry.dat$Windfarm$dist_biofuel_plant
    ii <- ii + 1
  }
  names(soil_seq_rate) <- grep("Area", names(forestry.dat), value = T)
  names(t_wf_by_area) <- grep("Area", names(forestry.dat), value = T)
  names(emissions_from_felling) <- grep("Area", names(forestry.dat), value = T)
  names(emissions_from_transport) <- grep("Area", names(forestry.dat), value = T)
  names(fossil_fuel_emissions_factor) <- grep("Area", names(forestry.dat), value = T)
  names(dist_fuel_plant) <- grep("Area", names(forestry.dat), value = T)

  L_floor_bfr_replant <- list_op(l1 = A_felled,
                                 l2 = map(forestry.dat[grep("Area", names(forestry.dat))], "t_replant"),
                                 l3 = soil_seq_rate,
                                 func = "*")

  L_floor_aft_replant <- list_op(l1 = list_op(l1 = A_felled,
                                              l2 = A_replant,
                                              func = "-"),
                                 l2 = list_op(l1 = t_wf_by_area,
                                              l2 = map(forestry.dat[grep("Area", names(forestry.dat))], "t_replant"),
                                              func = "-"),
                                 l3 = soil_seq_rate,
                                 func = "*")

  L_floor <- lapply(list_op(l1 = L_floor_bfr_replant,
                            l2 = L_floor_aft_replant,
                            func = "+"),
                    FUN = function(x) x * CO2_C)

  ## Emissions from harvesting operations (from growth yield table)
  vol_harv <- lapply(list_op(l1 = map(forestry.dat[grep("Area", names(forestry.dat))], "t_harv"),
                             l2 = map(forestry.dat[grep("Area", names(forestry.dat))], "soil_type"),
                             func = "c"),
                     FUN = function(x) growth_yield_tab(t = x[1:3], soil_type = x[4], species = 2)$volume)

  L_harv <- list_op(l1 = vol_harv,
                    l2 = A_felled,
                    l3 = emissions_from_felling,
                    func = "*")

  ## Savings from use of felled forestry as biofuel
  W_felled <- list_op(l1 = list_op(l1 = A_felled,
                                   l2 = C_wpry$C_tot,
                                   func = "*"),
                      l2 = lapply(map(forestry.dat[grep("Area", names(forestry.dat))], "r_CBiomass"),
                                  FUN = function(x) {
                                    x <- x[c(1,3,2)] # re-arrange Min, Max
                                    names(x) <- c("Exp", "Min", "Max")
                                    return(x)
                                  }),
                      func = "/")

  W_felled_biofuel <- lapply(W_felled,
                             FUN = function(x) x * rho_felled$biofuel)

  W_pow_val_felled <- list_op(l1 = W_felled_biofuel,
                              l2 = map(forestry.dat[grep("Area", names(forestry.dat))], "e_felled_biofuel"),
                              func = "*")

  # Get 1/0 for Yes/No converting felled forestry to biofuel
  # JDebug: this can be handled instead via rho_felled
  # JDebug: THIS DOESN'T WORK ANYMORE AS I HAVE REMOVED felled_biofuel FROM THE INPUT LIST!!!
  felled_biofuel <- lapply(map(forestry.dat[grep("Area", names(forestry.dat))], "felled_biofuel"),
                           FUN = function(x) {
                             if (x[1]==2) { # not used as biofuel
                               return(c(Exp = 0, Min = 0, Max = 0))
                             } else {
                               return(c(Exp = 1, Min = 1, Max = 1))
                             }
                           })

  S_biofuel_felled <- list_op(l1 = W_pow_val_felled,
                              l2 = fossil_fuel_emissions_factor,
                              l3 = felled_biofuel,
                              func = "*")

  L_transp_felled <- list_op(l1 = dist_fuel_plant,
                             l2 = emissions_from_transport,
                             l3 = W_felled_biofuel,
                             func = "*")

  S_biofuel_felled <- list_op(l1 = S_biofuel_felled,
                              l2 = L_transp_felled,
                              func = "-")

  ## Savings from use of replanted forestry as biofuel
  W_replant <- list_op(l1 = list_op(l1 = A_replant,
                                    l2 = C_wpry$seq_pot_replant,
                                    func = "*"),
                       l2 = lapply(map(forestry.dat[grep("Area", names(forestry.dat))], "r_CBiomass"),
                                   FUN = function(x) {
                                     x <- x[c(1,3,2)] # re-arrange Min, Max
                                     names(x) <- c("Exp", "Min", "Max")
                                     return(x)
                                   }),
                       func = "/")

  W_replant_biofuel <- lapply(W_replant,
                              FUN = function(x) x * rho_replant$biofuel)

  W_pow_val_replant <- list_op(l1 = W_replant_biofuel,
                               l2 = map(forestry.dat[grep("Area", names(forestry.dat))], "e_felled_biofuel"),
                               func = "*")

  # Assume that if harvested biomass converted to biofuel, so is replanted biomass...
  # This could instead be handled using rho_replant
  replant_biofuel <- felled_biofuel

  S_biofuel_replant <- list_op(l1 = W_pow_val_replant,
                               l2 = fossil_fuel_emissions_factor,
                               l3 = replant_biofuel,
                               func = "*")

  L_transp_replant <- list_op(l1 = dist_fuel_plant,
                              l2 = emissions_from_transport,
                              l3 = W_replant_biofuel,
                              func = "*")

  S_biofuel_replant <- list_op(l1 = S_biofuel_replant,
                               l2 = L_transp_replant,
                               func = "-")

  ## Wood product decay functions
  ### Felled forestry
  W_wp_long_felled <- lapply(W_felled,
                             FUN = function(x) x * rho_replant$wp_long)

  W_wp_med_felled <- lapply(W_felled,
                             FUN = function(x) x * rho_replant$wp_med)

  W_wp_short_felled <- lapply(W_felled,
                             FUN = function(x) x * rho_replant$wp_short)

  L_wp_long_felled <- lapply(W_wp_long_felled,
                             FUN = function(x) x * (1 - exp(-alpha_wp$wp_long * core.dat$Windfarm$t_wf) + D_wp$wp_long * E_wp$wp_long))

  L_wp_med_felled <- lapply(W_wp_med_felled,
                             FUN = function(x) x * (1 - exp(-alpha_wp$wp_med * core.dat$Windfarm$t_wf) + D_wp$wp_med * E_wp$wp_med))

  L_wp_short_felled <- lapply(W_wp_short_felled,
                             FUN = function(x) x * (1 - exp(-alpha_wp$wp_short * core.dat$Windfarm$t_wf) + D_wp$wp_short * E_wp$wp_short))

  L_wp_felled <- list_op(l1 = L_wp_long_felled,
                         l2 = L_wp_med_felled,
                         l3 = L_wp_short_felled,
                         func = "+")

  ### Replanted forestry
  W_wp_long_replant <- lapply(W_replant,
                              FUN = function(x) x * rho_replant$wp_long)

  W_wp_med_replant <- lapply(W_replant,
                             FUN = function(x) x * rho_replant$wp_med)

  W_wp_short_replant <- lapply(W_replant,
                               FUN = function(x) x * rho_replant$wp_short)

  L_wp_long_replant <- lapply(W_wp_long_replant,
                              FUN = function(x) x * (1 - exp(-alpha_wp$wp_long * core.dat$Windfarm$t_wf) + D_wp$wp_long * E_wp$wp_long))

  L_wp_med_replant <- lapply(W_wp_med_replant,
                             FUN = function(x) x * (1 - exp(-alpha_wp$wp_med * core.dat$Windfarm$t_wf) + D_wp$wp_med * E_wp$wp_med))

  L_wp_short_replant <- lapply(W_wp_short_replant,
                               FUN = function(x) x * (1 - exp(-alpha_wp$wp_short * core.dat$Windfarm$t_wf) + D_wp$wp_short * E_wp$wp_short))

  L_wp_replant <- list_op(l1 = L_wp_long_replant,
                          l2 = L_wp_med_replant,
                          l3 = L_wp_short_replant,
                          func = "+")
  ## Totals
  ### Emissions
  L_tot <- list_op(l1 = list_op(l1 = C_seq_loss_felled,
                                l2 = L_floor,
                                l3 = L_harv,
                                func = "+"),
                   l2 = list_op(l1 = L_wp_felled,
                                l2 = L_wp_replant,
                                func = "+"),
                   func = "+")

  ### Savings (biofuel)
  S_tot <- list_op(l1 = lapply(S_biofuel_felled,
                               FUN = function(x) {
                                 x <- x[c(1,3,2)] # re-arrange Min, Max
                                 names(x) <- c("Exp", "Min", "Max")
                                 return(x)
                               }),
                   l2 = lapply(S_biofuel_replant,
                               FUN = function(x) {
                                 x <- x[c(1,3,2)] # re-arrange Min, Max
                                 names(x) <- c("Exp", "Min", "Max")
                                 return(x)
                               }),
                   func = "+")

  L_forestry <- list_op(l1 = L_tot,
                        l2 = S_tot,
                        func = "-")

  L_forestry_tot <- colSums(bind_rows(L_forestry))

  return(L_forestry_tot)
}

# 7ii. Forestry CO2 loss - detail restoration model
#' Forestry_CO2_loss_detail
#' @param core.dat UI data
#' @param forestry.dat UI forestry data
#' @param growthYield.dat growth and yield data (estimated from CARBINE runs)
#' @param C_wpry carbon content of the removed forestry estimated by 3PG
#' @return Estimated lifetime loss of carbon stored in forestry products
#' @export
Forestry_CO2_loss_detail_RM <- function(core.dat,
                                        forestry.dat,
                                        growthYield.dat,
                                        S_forest) {

  # THIS FUNCTION...

  # Extract input variables for easy access
  CO2_C <- 3.667 # Molecular weight ratio C to CO2
  A_harv <- map(forestry.dat[grep("Area", names(forestry.dat))], "A_harv")
  t_harv <- map(forestry.dat[grep("Area", names(forestry.dat))], "t_harv")
  Spp <- map(forestry.dat[grep("Area", names(forestry.dat))], .f = "species")
  YC <- map(forestry.dat[grep("Area", names(forestry.dat))], .f = "YC") # if not passed by user, already computed elsewhere from Growth and yield tables

  E_harv <- rep(list(forestry.dat$Emissions$E_harv), length = length(grep("Area", names(forestry.dat))))
  names(E_harv) <- names(YC)

  p_biofuel <- map(forestry.dat[grep("Area", names(forestry.dat))], "p_biofuel")
  p_biofuel <- lapply(p_biofuel, FUN = function(x) {
    if (is.null(x)) {
      return(c(Exp = 0, Min = 0, Max = 0)) # add zero vector in case p_biofuel not passed
    } else {
      p_biofuel_a <- x[c(1,3,2)] # re-order min/max
      names(p_biofuel_a) <- c("Exp", "Min", "Max")
      return(p_biofuel_a)
    }
  })
  r_CBiomass <- map(forestry.dat[grep("Area", names(forestry.dat))], "r_CBiomass")
  e_felled_biofuel <- map(forestry.dat[grep("Area", names(forestry.dat))], "e_felled_biofuel")

  E_fossil_mix <- rep(list(core.dat$Counterfactual$E_fossil_mix), length = length(grep("Area", names(forestry.dat))))
  names(E_fossil_mix) <- names(r_CBiomass)


  d_wpF <- map(forestry.dat[grep("Area", names(forestry.dat))], "d_wpF")
  d_wpM <- map(forestry.dat[grep("Area", names(forestry.dat))], "d_wpM")
  d_wpS <- map(forestry.dat[grep("Area", names(forestry.dat))], "d_wpS")

  d_biofuel <- lapply(map(forestry.dat[grep("Area", names(forestry.dat))], "d_biofuel"),
                      FUN = function(x) {
                        if (is.null(x)) { # add zero vector if not passed
                          return(c(Exp = 0, Min = 0, Max = 0))
                        } else {
                          return(x)
                        }
                      })

  # Match object structure for downstream manipulations
  # Set to zero if not passed: max(0,NULL) returns 0
  d_wp <- lapply(seq_along(d_wpF), FUN = function(x) {
    d_wp_a <- list(Exp = c(wpF = max(0,unname(d_wpF[[x]][1])),
                           wpM = max(0,unname(d_wpM[[x]][1])),
                           wpS = max(0,unname(d_wpS[[x]][1]))),
                   Min = c(wpF = max(0,unname(d_wpF[[x]][2])),
                           wpM = max(0,unname(d_wpM[[x]][2])),
                           wpS = max(0,unname(d_wpS[[x]][2]))),
                   Max = c(wpF = max(0,unname(d_wpF[[x]][3])),
                           wpM = max(0,unname(d_wpM[[x]][3])),
                           wpS = max(0,unname(d_wpS[[x]][3]))))
    return(d_wp_a)
  })

  names(d_wp) <- names(r_CBiomass)

  # Get exponential decay rates for wood products
  alpha_dat <- read_xlsx("Templates/alpha_wp.xlsx",
                         sheet = "Sheet1",
                         range = "A1:D7",
                         progress = F)

  alpha <- lapply(seq_along(d_wp), FUN = function(x) {
    if (all(unlist(d_wp[[x]]) == 0)) {
      ## if distance to processing sites not passed, assume forestry products left in situ
      alpha_a <- alpha_dat %>%
        filter(Type == "Unprocessed")
    } else {
      alpha_a <- alpha_dat %>%
        filter(Type == "Processed")
    }
    alpha_wp <- alpha_a$alpha
    names(alpha_wp) <- stringr::str_replace(alpha_a$Var, "alpha_", "")
    return(alpha_wp)
  })

  names(alpha) <- names(d_wp)

  species <- c("Scots_pine", "Sitka_spruce")

  # Compute harvesting emissions
  L_harv <- lapply(seq_along(E_harv), FUN = function(x) {
    L_harv_a <- lapply(seq_along(E_harv[[x]]), FUN = function(y) {
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
               YC == YC_a,
               Age == t_harv_a) %>%
        select(V_a)

      L_harv_a <- V_a * A_harv[[x]][[y]] * E_harv[[x]][[y]]

      return(unname(L_harv_a))
    })

    L_harv_a <- unlist(L_harv_a)
    names(L_harv_a) <- names(E_harv[[x]])
    return(L_harv_a)
  })

  names(L_harv) <- names(C_forest)

  # Extract carbon content of the forestry at harvesting
  C_forest <- lapply(seq_along(S_forest), FUN = function(x) {
    res <- lapply(seq_along(S_forest[[x]]), FUN = function(y) {
      S_forest[[x]][[y]] %>%
        filter(t<=0) %>%
        summarise(C = sum(NPP))
    })
    res <- unlist(res)
    names(res) <- names(out[[x]])
    return(res)
  })

  names(C_forest) <- grep("Area", names(forestry.dat), value=T)

  # Compute carbon content of forestry products converted to biofuel
  C_biofuel <- list_op(l1 = C_forest,
                       l2 = p_biofuel,
                       func = "*")

  # Subtract biofuel carbon from wood products
  C_forest <- list_op(l1 = C_forest,
                      l2 = C_biofuel,
                      func = "-")

  # Proportion of biomass used in long, medium and short lived wood products
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

      rho <- growthYield.dat %>%
        filter(Spp == Spp_a,
               YC == YC_a,
               Age == t_harv_a) %>%
        select(rho_wpF, rho_wpM, rho_wpS)

      return(rho)
    })

    names(rho_wp_a) <- names(C_forest[[x]])
    return(rho_wp_a)
  })

  names(rho_wp) <- names(C_forest)

  C_wp <- lapply(seq_along(C_forest), FUN = function(x) {
    C_wp_a <- lapply(seq_along(C_forest[[x]]), FUN = function(y) {
      C <- unlist(rho_wp[[x]][[y]]) * C_forest[[x]][[y]]
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

  # Convert from units C to units Biomass
  B_wp <- lapply(seq_along(C_wp), FUN = function (x) {
    B_wp_a <- lapply(seq_along(C_wp[[x]]), FUN = function(y) {
      B <- unlist(C_wp[[x]][[y]]) * r_CBiomass[[x]][[y]]
      return(B)
    })
    names(B_wp_a) <- names(C_wp[[x]])
    return(B_wp_a)
  })

  names(B_wp) <- names(C_wp)

  # Model exponential decay in wood product stored CO2
  ## Simulate for long enough that the slowest decaying product reaches a proportion C_min of initial value
  C_min <- 0.01
  tMax <- -max(unlist(alpha)) * log(C_min)
  tt <- 0:signif(tMax, digits = 1) # round to nearest 100
  CO2_wp_decay <- lapply(seq_along(CO2_wp), FUN = function (x) {
    CO2_wp_decay_a <- lapply(seq_along(CO2_wp[[x]]), FUN = function(y) {
      res <- data.frame(t=tt,
                        S_wpF=CO2_wp[[x]][[y]]["wpF"] * exp(-tt/alpha_wp["wpF"]),
                        S_wpM=CO2_wp[[x]][[y]]["wpM"] * exp(-tt/alpha_wp["wpM"]),
                        S_wpS=CO2_wp[[x]][[y]]["wpS"] * exp(-tt/alpha_wp["wpS"]))

      res <- res %>%
        mutate(L_wpF = replace_na(lag(S_wpF, n = 1) - S_wpF, 0),
               L_wpM = replace_na(lag(S_wpM, n = 1) - S_wpM, 0),
               L_wpS = replace_na(lag(S_wpS, n = 1) - S_wpS, 0)) %>%
        select(t, L_wpF, L_wpM, L_wpS)

      if (0) {
        plot(L_wpF ~ t, res, type='l')
        plot(L_wpM ~ t, res, type='l')
        plot(L_wpS ~ t, res, type='l')
      }

      return(res)
    })
    names(CO2_wp_decay_a) <- names(CO2_wp[[x]])
    return(CO2_wp_decay_a)
  })

  names(CO2_wp_decay) <- names(CO2_wp)

  if (0) {
    plot(L_wpF ~ t, CO2_wp_decay$Area.1$Exp, type='l', xlim=c(0,50))
    plot(L_wpM ~ t, CO2_wp_decay$Area.1$Exp, type='l', xlim=c(0,50))
    plot(L_wpS ~ t, CO2_wp_decay$Area.1$Exp, type='l', xlim=c(0,50))
  }

  ## Esimate transportion emissions B*D*E
  L_T_wp <- lapply(seq_along(B_wp), FUN = function (x) {
    L_T_wp_a <- lapply(seq_along(B_wp[[x]]), FUN = function(y) {
      L_T <- B_wp[[x]][[y]] * d_wp[[x]][[y]] * forestry.dat$Emissions$E_transport[y] / 1e6 # unit conversion g CO2 to t CO2
      return(L_T)
    })
    names(L_T_wp_a) <- names(B_wp[[x]])
    return(L_T_wp_a)
  })

  names(L_T_wp) <- names(B_wp)

  # Compute biomass converted to biofuel
  # JDebug: I am going to use tree biomass only here as I think it is wrong to
  # estimate standing biomass of understory using the cumulative sum of understory
  # NPP. This is likely to include perennial species whose biomass is not retained year on year

  B_biofuel <- list_op(l1 = C_biofuel,
                       l2 = lapply(r_CBiomass,
                                   FUN = function(x) {
                                     x <- x[c(1,3,2)] # re-arrange Min, Max
                                     names(x) <- c("Exp", "Min", "Max")
                                     return(x)
                                   }),
                       func = "/")

  S_biofuel <- list_op(l1 = B_biofuel,
                       l2 = e_felled_biofuel, # power value of forestry biomass
                       l3 = E_fossil_mix, # emissions factor of counterfactual
                       func = "*")

  L_T_biofuel <- list_op(l1 = B_biofuel,
                         l2 = d_biofuel,
                         l3 = rep(list(forestry.dat$Emissions$E_transport), length = length(grep("Area", names(forestry.dat)))),
                         func = "*")

  return(list(L_harv = L_harv,
              L_mulch = NULL,
              L_wp = CO2_wp_decay,
              L_T_wp = L_T_wp,
              S_biofuel = S_biofuel,
              L_T_biofuel = L_T_biofuel))
}
