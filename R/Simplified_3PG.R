#' ForestSequestrationMod
#' @param forestry.dat UI forestry data
#' @return 3PG output
#' @export
ForestSequestrationMod <- function(forestry.dat) {

  # THIS FUNCTION...

  # Get 3PG templates
  parms_3PG <- read_excel("Templates/3PG_parms.xlsx",
                          sheet = "3PG_parms",
                          range = "A1:E24")

  species <- c("Scots_Pine", "Sitka_Spruce")

  parms_3PG <- as.data.frame(parms_3PG %>%
                               filter(complete.cases(.)) %>%
                               select(Var_name, all_of(species)))

  f_E_coef <-  read_excel("Templates/f_E_coef.xlsx",
                          range = "A1:H7")

  # Extract input variables for easy access
  YC <- map(forestry.dat[grep("Area", names(forestry.dat))], .f = "YC") # if not passed by user, already computed elsewhere from Growth and yield tables
  Spp <- map(forestry.dat[grep("Area", names(forestry.dat))], .f = "species")

  ## Estimate NPP_max for full 3PG simulation based on YC
  NPP_Max <- lapply(1:length(grep("Area", names(forestry.dat))),
                    FUN = function(x) {
                      Spp_a <- species[Spp[[x]][1]]
                      YC_a <- YC[[x]]

                      aa <- f_E_coef %>%
                        filter(spp == Spp_a) %>%
                        filter(y=="NPP_max", x=="YC") %>%
                        select(-c(y, x, spp)) %>%
                        unlist()

                      NPP_max <- aa[1] + aa[2]*YC_a + aa[3]*YC_a^2 + aa[4]*YC_a^3 + aa[5]*YC_a^4

                      return(NPP_max)
                    })
  names(NPP_Max) <- grep("Area", names(forestry.dat), value=T)

  ## Estimate f_E for simplified 3PG simulation based on NPP_max
  f_E <- lapply(1:length(grep("Area", names(forestry.dat))),
                FUN = function(x) {
                  Spp_a <- species[Spp[[x]][1]]
                  NPP_Max_a <- NPP_Max[[x]]

                  aa <- f_E_coef %>%
                    filter(spp == Spp_a) %>%
                    filter(y=="f_E", x=="NPP_max_LCA") %>%
                    select(-c(y, x, spp)) %>%
                    unlist()

                  f_E <- aa[1] + aa[2]*NPP_Max_a + aa[3]*NPP_Max_a^2 + aa[4]*NPP_Max_a^3 + aa[5]*NPP_Max_a^4
                  return(f_E)
                })
  names(f_E) <- grep("Area", names(forestry.dat), value=T)

  # Get full parameter lists for each area
  parms_3PG_by_area <- array(0,
                             dim = c(nrow(parms_3PG), # f_E and t_seedling_replant not included in template
                                     3, # Exp, Min, Max if defined
                                     length(grep("Area", names(forestry.dat)))), # number of areas considered
                             dimnames = list(c(parms_3PG$Var_name),
                                             c("Exp", "Min", "Max"),
                                             grep("Area", names(forestry.dat), value=T)))

  ii <- 1
  for (i in grep("Area", names(forestry.dat))) {

    # Species parameters (no range given)
    parms_3PG_by_area[parms_3PG$Var_name,,ii] <- parms_3PG[,which(colnames(parms_3PG) == species[forestry.dat[[i]]$species[1]])]
    parms_3PG_by_area["f_E",,ii] <- f_E[[ii]]

    # Max simulation length set to 500 + harvesting age
    parms_3PG_by_area["t_wf",,ii] <- 500 + map(forestry.dat[grep("Area", names(forestry.dat))], .f = "t_harv")[[ii]]

    # If user selects rotation, set rotation length to 50 by default.
    if (map(forestry.dat[grep("Area", names(forestry.dat))], .f = "rotation")[[ii]][1] == 1) { # user selects rotation
      if (is.null(map(forestry.dat[grep("Area", names(forestry.dat))], .f = "t_rotation")[[ii]])) { # user has NOT input rotation length
        parms_3PG_by_area["t_rotation",,ii] <- rep(50, 3)
      } else {
        parms_3PG_by_area["t_rotation",,ii] <- map(forestry.dat[grep("Area", names(forestry.dat))], .f = "t_rotation")[[ii]]
      }
    } else { # user selects NO rotation
      parms_3PG_by_area["t_rotation",,ii] <- parms_3PG_by_area["t_wf",,ii]
    }

    ii <- ii + 1
  }

  # 3PG is run using a nested, vectorised approach for efficiency
  # Nesting applies run_3PG to a) Each area, b) Each of Exp, Min, Max
  # run_3PG will internally estimate the various total sequestration values
  # These are stored in the output as NPP_0, NPP_01
  # Full trajectories are also returned

  out <- apply(parms_3PG_by_area,
               MAR = 3,
               FUN = function(y) {
                 apply(y,
                       MAR=2,
                       function(x) do.call(run_3PG, as.list(c(x, c(replant=0), c(thin=NA)))))
               })

  # Remove summary data used in wind farm LCA
  out <- lapply(out, FUN = function(x) map(x, .f = "res"))

  # Translate time axis in accordance with harvesting age and scale NPP by area
  out <- lapply(seq_along(out), FUN = function(x) {
    res <- lapply(seq_along(out[[x]]), FUN = function(y) {
      out[[x]][[y]] %>%
        mutate(t = t - unlist(map(forestry.dat[grep("Area", names(forestry.dat))[x]], .f = "t_harv"))[y]) %>%
        rename(NPP_pa = NPP, # trees only
               NPP_tot_pa = NPP_tot) %>% # trees + understory
        mutate(NPP = NPP_pa * unlist(map(forestry.dat[grep("Area", names(forestry.dat))[x]], .f = "A_harv"))[y], # trees only
               NPP_tot = NPP_tot_pa * unlist(map(forestry.dat[grep("Area", names(forestry.dat))[x]], .f = "A_harv"))[y]) # trees + understory
    })
    names(res) <- names(out[[x]])
    return(res)
  })

  names(out) <- grep("Area", names(forestry.dat), value=T)

  return(Forestry.seq = out)
}

run_3PG <- function(t_rotation = 50, # rotation length
                    rotation = 3, # no. rotations
                    t_step = 1, # time step
                    L_long = 4, # leaf longevity
                    pF = 0.25, # proportion carbon allocated to foliage
                    N0 = 0, # coefficient for allocation response to nitrogen (NOT USED)
                    R_long = 0, # fine root longevity (NOT USED)
                    Wl_init = 0.01, # initial foliage biomass # was 0.005; set to 0.01 after age at which leaf longevity impacts dynamics set to zero for simplicity
                    t_seedling_replant = 0, # seedling age at planting
                    SLA = 6, # specific leaf area
                    phi_p = 2880, # average annual photosynthetically active radiation
                    kP = 0.5, # extinction coefficient
                    acc_temp = 1, # accumulated temperature (not used)
                    f_E = 0.3176845825, # environmental modifier
                    A0.5 = 100, # age at 50% reduction in NPP
                    LAI_u = 3, # LAI understory
                    e_max = 0.00152, # maximum light use efficiency
                    YPP = 0.47, # NPP:GPP ratio
                    t_harv = 25, # age of forestry when felled for wind farm
                    t_wf = 25, # lifetime of wind farm
                    t_replant = 0, # years after felling when replanting occurs
                    t_replant_guar = 20, # years for which replanted forestry will be grown on the site
                    t1 = 25, # either t_wf or t_replant_guar
                    replant = 0, # boolean to select pre-wind farm or replanted forest for output
                    thin = data.frame(t = c(20, 25, 30, 35, 40, 45), # thinning regime (years/proportion harvested)
                                      p = c(0.189655172413793, 0.180555555555556, 0.166666666666667, 0.141538461538462, 0.050561797752809, 0.0537897310513447))) {

  if (replant == 0) {
    t_seedling_replant <- 0
  }

  if (is.na(thin)) { # required since setting a variable to NULL in a list will simply remove that variable!
    thin <- NULL
  }

  t <- seq(0,t_rotation,by=t_step)
  res <- data.frame(matrix(0, nrow=length(t), ncol=10))
  colnames(res) <- c("t",
                     "thin",
                     "Wl",
                     "LAI",
                     "phi_pau",
                     "phi_pa_u",
                     "phi_pau_u",
                     "NPP",
                     "NPP_tot",
                     "NPP_cum")

  res$t <- t

  leaf_long_onset <- 0 # 6 as in spreadsheet: suggest setting to zero!

  # Initialise 3PG
  res$Wl[1] <- Wl_init
  res$LAI[1] <- res$Wl[1] * SLA

  # SPREADSHEET ERROR
  # res$phi_pau[1] <- phi_p * f_E * (1 - exp(-kP * res$LAI[1])) * (1 / (1 + (res$t[1] / A0.5)^4)) # SPREADSHEET ERROR
  res$phi_pau[1] <- phi_p * f_E * (1 - exp(-kP * res$LAI[1])) * (1 / (1 + ((res$t[1] + t_seedling_replant) / A0.5)^4))

  res$phi_pa_u[1] <- phi_p * (1 - (1 - exp(-kP * res$LAI[1]))) * (1 - exp(-kP * LAI_u))
  res$phi_pau_u[1] <- res$phi_pa_u[1] * f_E
  res$NPP[1] <- YPP * e_max * res$phi_pau[1]
  res$NPP_tot[1] <- YPP * e_max * (res$phi_pau[1] +  res$phi_pau_u[1])
  res$NPP_cum[1] <- res$NPP_tot[1]

  # Iterate 3PG
  for (i in 2:nrow(res)) {
    if (!is.null(thin)) {
      # Add thinning (accounting for possible non-integer time step)
      if (nrow(thin) > 0 & res$t[i] >= thin$t[1]) {
        res$thin[i] <- thin$p[1]
        thin <- thin[-1,]
      }
    }

    # SPREADSHEET ERROR
    # if (res$t[i] < (t_seedling_replant + leaf_long_onset)) { # Leaf longevity not yet relevant SPREADSHEET ERROR
    if ((res$t[i] + t_seedling_replant) < leaf_long_onset) { # Leaf longevity not yet relevant
      res$Wl[i] <- res$Wl[i-1] * (1 - res$thin[i]) + t_step * res$NPP[i-1] * pF
    } else { # Accounting for leaf longevity
      res$Wl[i] <- res$Wl[i-1] * (1 - res$thin[i]) + t_step * (res$NPP[i-1] * pF - res$Wl[i-1] / L_long)
    }
    res$LAI[i] <- res$Wl[i] * SLA

    # SPREADSHEET ERROR
    # res$phi_pau[i] <- phi_p * f_E * (1 - exp(-kP * res$LAI[i])) * (1 / (1 + (res$t[i] / A0.5)^4)) # SPREADSHEET ERROR
    res$phi_pau[i] <- phi_p * f_E * (1 - exp(-kP * res$LAI[i])) * (1 / (1 + ((res$t[i] + t_seedling_replant) / A0.5)^4))

    res$phi_pa_u[i] <- phi_p * (1 - (1 - exp(-kP * res$LAI[i]))) * (1 - exp(-kP * LAI_u))
    res$phi_pau_u[i] <- res$phi_pa_u[i] * f_E
    res$NPP[i] <- YPP * e_max * res$phi_pau[i]
    res$NPP_tot[i] <- YPP * e_max * (res$phi_pau[i] +  res$phi_pau_u[i])
    res$NPP_cum[i] <- res$NPP_cum[i-1] + res$NPP_tot[i]
  }

  if (replant == 0) {
    t0 <- t_harv
    t1 <- t0 + t_wf
  } else {
    t0 <- t_seedling_replant
    t1 <- t0 + t_replant_guar
  }

  if (t_rotation < t1) {
    rotation <- floor(t1 / t_rotation) + 1
    res <- rbind(res, do.call(rbind, replicate(rotation-1, res[-nrow(res),], simplify=F)))
    res$t <- seq(0,(nrow(res)-1)*t_step,by=t_step)
  }

  res <- res %>%
    mutate(NPP = 10 * NPP, # convert from kg C m-2 yr-1 to t C ha-1 hr-1 (10000/1000 = 10)
           NPP_tot = 10 * NPP_tot,
           NPP_cum = 10 * NPP_cum)

  NPP_0 <- res %>%
    filter(abs(t - t0) == min(abs(t - t0))) %>% # this in case of non-integer t/t0
    slice(1) %>%
    dplyr::select(NPP_cum) %>%
    unname()

  NPP_01 <- res %>%
    filter(t >= t0 & t < t1) %>%
    dplyr::select(NPP_tot) %>%
    sum()

  return(list(res = res,
              NPP_0 = NPP_0,
              NPP_01 = NPP_01))
}

if(0) {

  # Run 3PG for variety of YCs for illustrative plotting

  # Get 3PG templates
  parms_3PG <- read_excel("Templates/3PG_parms.xlsx",
                          sheet = "3PG_parms",
                          range = "A1:E24")

  species <- c("Scots_Pine", "Sitka_Spruce")

  parms_3PG <- as.data.frame(parms_3PG %>%
                               filter(complete.cases(.)) %>%
                               select(Var_name, all_of(species)))

  f_E_coef <-  read_excel("Templates/f_E_coef.xlsx",
                          range = "A1:H7")

  # Extract input variables for easy access
  # YC <- map(forestry.dat[grep("Area", names(forestry.dat))], .f = "YC") # if not passed by user, already computed elsewhere from Growth and yield tables
  # Spp <- map(forestry.dat[grep("Area", names(forestry.dat))], .f = "species")

  YC <- c(6,8,10,12)
  Spp <- 2

  ## Estimate NPP_max for full 3PG simulation based on YC
  NPP_Max <- sapply(seq_along(YC),
                    FUN = function(x) {
                      Spp_a <- species[Spp]
                      YC_a <- YC[x]

                      aa <- f_E_coef %>%
                        filter(spp == Spp_a) %>%
                        filter(y=="NPP_max", x=="YC") %>%
                        select(-c(y, x, spp)) %>%
                        unlist()

                      NPP_max <- aa[1] + aa[2]*YC_a + aa[3]*YC_a^2 + aa[4]*YC_a^3 + aa[5]*YC_a^4

                      return(NPP_max)
                    })

  ## Estimate f_E for simplified 3PG simulation based on NPP_max
  f_E <- sapply(seq_along(YC),
                FUN = function(x) {
                  Spp_a <- species[Spp]
                  NPP_Max_a <- NPP_Max[x]

                  aa <- f_E_coef %>%
                    filter(spp == Spp_a) %>%
                    filter(y=="f_E", x=="NPP_max_LCA") %>%
                    select(-c(y, x, spp)) %>%
                    unlist()

                  f_E <- aa[1] + aa[2]*NPP_Max_a + aa[3]*NPP_Max_a^2 + aa[4]*NPP_Max_a^3 + aa[5]*NPP_Max_a^4
                  return(f_E)
                })

  # Get full parameter lists for each area
  parms_3PG_by_area <- matrix(0, nrow = nrow(parms_3PG), ncol = length(YC),
                              dimnames = list(c(parms_3PG$Var_name),
                                              paste0("YC", 1:length(YC))))

  for (i in 1:length(YC)) {
    parms_3PG_by_area[parms_3PG$Var_name,i] <- parms_3PG[,which(colnames(parms_3PG) == species[Spp])]
    parms_3PG_by_area["f_E",i] <- unname(f_E[i])
    parms_3PG_by_area["t_wf",i] <- 250
    parms_3PG_by_area["t_rotation",i] <- parms_3PG_by_area["t_wf",i]
  }

  # 3PG is run using a nested, vectorised approach for efficiency
  # Nesting applies run_3PG to a) Each area, b) Each of Exp, Min, Max
  # run_3PG will internally estimate the various total sequestration values
  # These are stored in the output as NPP_0, NPP_01
  # Full trajectories are also returned

  out <- apply(parms_3PG_by_area,
               MAR = 2,
               FUN = function(x) {
                 do.call(run_3PG, as.list(c(x, c(replant=0), c(thin=NA))))
               })

  res <- c()

  for (i in 1:length(out)) {
    res_yc <- out[[i]]$res %>%
      select(t, NPP) %>%
      mutate(YC = YC[i])

    res <- rbind(res,
                 res_yc)
  }

  p3PG <- ggplot(res, aes(x=t, y=3.67*NPP, col=factor(YC))) +
    geom_line() +
    labs(x="Stand age (yr)", y="Sequestration (t CO2 ha-1 yr-1)", col="YC") +
    scale_y_continuous(limits = c(NA, 30)) +
    theme_bw()

  ww <- 16
  hh <- 12

  png("../Figures/LCA implementation/V2/3PG_seq.png",
      width=ww/1.5, height=hh/1.6, units="cm", res=300)
  p3PG
  dev.off()

}
