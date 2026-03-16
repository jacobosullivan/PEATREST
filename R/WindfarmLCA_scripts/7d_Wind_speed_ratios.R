## 7d. Wind speed ratios

#' Wind_speed_ratios
#' @param core.dat UI data
#' @param input.dat UI forestry data
#' @return windspeed ratios
#' @export
Wind_speed_ratios <- function(core.dat,
                              input.dat) {

  # THIS FUNCTION...

  # Extract and restructure data for easy access
  t_wf <- list()
  h_turb <- list()

  ii <- 1
  for (i in grep("Area", names(input.dat))) {
    t_wf[[ii]] <- core.dat$Windfarm$t_wf
    h_turb[[ii]] <- input.dat$Windfarm$H_turb
    ii <- ii + 1
  }
  names(t_wf) <- grep("Area", names(input.dat), value = T)
  names(h_turb) <- grep("Area", names(input.dat), value = T)

  # Width of felled forestry around turbine
  w_felled <- lapply(map(input.dat[grep("Area", names(input.dat))], .f = "A_harv_turb"),
                     FUN = function(x) sqrt(x*10000))

  # Height of original forestry at harvesting
  # Age at harvesting and soil type passed to growth and yield table function
  avg_height_felled <- lapply(list_op(l1 = map(input.dat[grep("Area", names(input.dat))], "t_harv"),
                                      l2 = map(input.dat[grep("Area", names(input.dat))], "soil_type"),
                                      func = "c"),
                              FUN = function(x) growth_yield_tab(t = x[1:3], soil_type = x[4], species = 2)$avg_height)

  # Height of replanted forestry at decomissioning
  # Age given by windfarm liftime + age of seedling at plantation - length of fallow period
  # This and soil type are passed to the growth and yield table function
  avg_height_replant <- lapply(list_op(l1 = list_op(l1 = t_wf,
                                                    l2 = map(input.dat[grep("Area", names(input.dat))], "t_seedling_replant"),
                                                    l3 = lapply(map(input.dat[grep("Area", names(input.dat))], "t_replant"), FUN = function(x) -x),
                                                    func = "+"),
                                       l2 = map(input.dat[grep("Area", names(input.dat))], "soil_type"),
                                       func = "c"),
                                 FUN = function(x) growth_yield_tab(t = x[1:3], soil_type = x[4], species = 2)$avg_height)

  # Height of original forestry at decomissioning IF NOT FELLED (accounting for 50 year rotation)
  t_rotation <- 50
  t_harv_not_felled <- list_op(l1 = map(input.dat[grep("Area", names(input.dat))], "t_harv"),
                               l2 = t_wf,
                               func = "+")

  # If t > 50 dial back by 50, repeat until (while loop) in case t > 100 (unlikely)
  t_harv_not_felled <- lapply(t_harv_not_felled,
                              FUN = function(x) {
                                while (any(x > t_rotation)) {
                                  x[x > t_rotation] <- x[x > t_rotation] - t_rotation
                                }
                                return(x)
                              })

  # Age at decomissioning if not felled and soil type passed to growth and yield table function
  avg_height_not_felled <- lapply(list_op(l1 = t_harv_not_felled,
                                          l2 = map(input.dat[grep("Area", names(input.dat))], "soil_type"),
                                          func = "c"),
                                  FUN = function(x) growth_yield_tab(t = x[1:3], soil_type = x[4], species = 2)$avg_height)

  # Wind speeds
  ## Define roughness and zero plane displacement
  r_upstream <- lapply(avg_height_not_felled,
                       FUN = function(x) c(Exp = 0.03, Min = 0.03, Max = 0.03))

  r_forest <- lapply(seq(length(r_upstream)),
                     FUN = function(x){
                       res <- r_upstream[[x]]
                       res[avg_height_not_felled[[x]] > 0.1] <- 0.075 * avg_height_not_felled[[x]][avg_height_not_felled[[x]] > 0.1]
                     })
  names(r_forest) <- names(r_upstream)

  r_SRF <- lapply(seq(length(r_upstream)),
                  FUN = function(x){
                    res <- r_upstream[[x]]
                    res[avg_height_replant[[x]] > 0.1] <- 0.075 * avg_height_replant[[x]][avg_height_replant[[x]] > 0.1]
                  })
  names(r_SRF) <- names(r_upstream)

  zpd_forest <- lapply(seq(length(r_upstream)),
                       FUN = function(x){
                         res <- r_upstream[[x]]
                         res[avg_height_not_felled[[x]] > 0.1] <- 0.65 * avg_height_not_felled[[x]][avg_height_not_felled[[x]] > 0.1]
                       })
  names(zpd_forest) <- names(r_upstream)

  zpd_SRF <- lapply(seq(length(r_upstream)),
                    FUN = function(x){
                      res <- r_upstream[[x]]
                      res[avg_height_replant[[x]] > 0.1] <- 0.65 * avg_height_replant[[x]][avg_height_replant[[x]] > 0.1]
                    })
  names(zpd_SRF) <- names(r_upstream)


  # Intermediate calculations
  # These are set as function defaults
  # max_HBL <- 5000
  # von_karman <- 0.4
  # alpha <- 4

  ## IBL forest
  h_IBL_forest <- getIBL(la = r_upstream,
                         lb = r_forest,
                         lc = map(input.dat[grep("Area", names(input.dat))], .f = "D_width"),
                         ld = zpd_forest)

  ## IBL felled forest
  h_IBL_felled <- getIBL(la = r_forest,
                         lb = r_upstream,
                         lc = w_felled,
                         ld = r_upstream)

  ## IBL replanted forest
  h_IBL_replant <- getIBL(la = r_forest,
                          lb = r_SRF,
                          lc = w_felled,
                          ld = zpd_SRF)

  # Windspeed at IBL forest
  ws_IBL_forest <- getWS_upwind(h1 = h_IBL_forest,
                                r_upstream = r_upstream)

  # Windspeed at IBL felled forest
  ws_IBL_felled <- getWS_forest(h1 = h_IBL_felled,
                                h_IBL_forest = h_IBL_forest,
                                avg_height_not_felled = avg_height_not_felled,
                                r_upstream = r_upstream,
                                r_forest = r_forest,
                                ws_IBL_forest = ws_IBL_forest,
                                zpd_forest = zpd_forest)

  # Windspeed at IBL replanted forest
  ws_IBL_replant <- getWS_felled(h1 = h_IBL_replant,
                                 h_IBL_forest = h_IBL_forest,
                                 h_IBL_felled = h_IBL_felled,
                                 avg_height_not_felled = avg_height_not_felled,
                                 r_upstream = r_upstream,
                                 r_forest = r_forest,
                                 ws_IBL_forest = ws_IBL_forest,
                                 ws_IBL_felled = ws_IBL_felled,
                                 zpd_forest = zpd_forest)

  # Windspeed at hub height upwind
  ws_hub_upwind <- getWS_upwind(h1 = h_turb,
                                r_upstream = r_upstream)

  # Windspeed at hub height forest
  ws_hub_forest <- getWS_forest(h1 = h_turb,
                                h_IBL_forest = remMinMax(h_IBL_forest), #
                                avg_height_not_felled = remMinMax(avg_height_not_felled),
                                r_upstream = remMinMax(r_upstream),
                                r_forest = remMinMax(r_forest),
                                ws_IBL_forest = remMinMax(ws_IBL_forest), #
                                zpd_forest = remMinMax(zpd_forest))

  # Windspeed at hub height felled
  ws_hub_felled <- getWS_felled(h1 = h_turb,
                                h_IBL_forest = remMinMax(h_IBL_forest),
                                h_IBL_felled = remMinMax(h_IBL_felled),
                                avg_height_not_felled = remMinMax(avg_height_not_felled),
                                r_upstream = remMinMax(r_upstream),
                                r_forest = remMinMax(r_forest),
                                ws_IBL_forest = remMinMax(ws_IBL_forest),
                                ws_IBL_felled = remMinMax(ws_IBL_felled),
                                zpd_forest = remMinMax(zpd_forest))

  # Windspeed at hub height replanted
  ws_hub_replant <- getWS_replant(h1 = h_turb,
                                  h_IBL_forest = remMinMax(h_IBL_forest),
                                  h_IBL_felled = remMinMax(h_IBL_felled),
                                  h_IBL_replant = remMinMax(h_IBL_replant),
                                  avg_height_not_felled = remMinMax(avg_height_not_felled),
                                  r_upstream = remMinMax(r_upstream),
                                  r_forest = remMinMax(r_forest),
                                  r_SRF = remMinMax(r_SRF),
                                  ws_IBL_forest = remMinMax(ws_IBL_forest),
                                  ws_IBL_felled = remMinMax(ws_IBL_felled),
                                  ws_IBL_replant = remMinMax(ws_IBL_replant),
                                  zpd_forest = remMinMax(zpd_forest))

  # wind speed ratio without felling
  ratio_no_fell <- list_op(l1 = ws_hub_forest,
                           l2 = ws_hub_upwind,
                           func = "/")

  # wind speed ratio with felling
  ratio_felled <- list_op(l1 = ws_hub_felled,
                          l2 = ws_hub_upwind,
                          func = "/")

  # wind speed ratio with replanting
  ratio_felled_replant <- list_op(l1 = ws_hub_replant,
                                  l2 = ws_hub_upwind,
                                  func = "/")

  return(list(ratio_no_fell = ratio_no_fell,
              ratio_felled = ratio_felled,
              ratio_felled_replant = ratio_felled_replant))
}

remMinMax <- function(y) {

  # Some calculations use only expection values for each area. This function removes Min and Max values

  y <- lapply(y, FUN = function(x) {
    x[2:3] <- x[1]
    return(x)
  })

  return(y)
}

getIBL <- function(la, lb, lc, ld, max_HBL = 5000) {

  # THIS FUNCTION...

  # H_IBL = ((0.75 + 0.03 * log(R_upwind / R_surface)) * R_surface * (D_width / R_surface)^0.8) + DeltaH_zero
  log_r_ratio <- lapply(list_op(l1 = la,
                                l2 = lb,
                                func = "/"),
                        FUN = function(x) 0.75 + 0.03 * log(x))

  w_r_ratio_p0.8 <- lapply(list_op(l1 = lc,
                                   l2 = lb,
                                   func = "/"),
                           FUN = function(x) x^0.8)

  IBL <- lapply(list_op(l1 = list_op(l1 = log_r_ratio,
                                     l2 = lb,
                                     l3 = w_r_ratio_p0.8,
                                     func = "*"),
                        l2 = ld,
                        func = "+"),
                FUN = function(x) {
                  x[x > max_HBL] <- max_HBL # IBL bounded at maximum
                  return(x)
                })
  return(IBL)
}

getWS_upwind <- function(h1, r_upstream, von_karman = 0.4) {

  # THIS FUNCTION...

  # V_upwind(rel) = log(H / R_upwind) / k_vonKarmen
  ws <- lapply(seq(length(h1)),
         FUN = function(x) {
           res <- log(round(h1[[x]]) / r_upstream[[x]]) / von_karman
           return(res)
         })
  names(ws) <- names(h1)

  return(ws)
}

getWS_forest <- function(h1, h_IBL_forest, avg_height_not_felled, r_upstream, r_forest, ws_IBL_forest, zpd_forest, von_karman = 0.4, alpha = 4) {

  # THIS FUNCTION...
  # Remove rounding of heights. This was required to reproduce spreadsheet result

  suppressWarnings({ # NaNs can be produces, however these should be filtered by conditionals
    ws <- getWS_upwind(lapply(h1,
                              FUN = round),
                       r_upstream) # this is true if h1 > h_IBL_forest

    # V_downwind(rel) = V_HIBL * log((H - DeltaH_zero)/R_forest) / log((H_IBL - DeltaH_zero)/R_forest)
    ws2 <- list_op(l1 = list_op(l1 = ws_IBL_forest,
                                l2 = lapply(list_op(l1 = list_op(l1 = lapply(h1,
                                                                             FUN = round),
                                                                 l2 = zpd_forest,
                                                                 func = "-"),
                                                    l2 = r_forest,
                                                    func = "/"),
                                            FUN = log),
                                func = "*"),
                   l2 = lapply(list_op(l1 = list_op(l1 = h_IBL_forest,
                                                    l2 = zpd_forest,
                                                    func = "-"),
                                       l2 = r_forest,
                                       func = "/"),
                               FUN = log),
                   func = "/") # this is true if h1 > avg_height_not_felled

    # JDEBUG: This doesn't seem to match theory, might be a spreadsheet error...
    # In the paper, this is V_HIBL + exp()..., here it is  V_HIBL * exp()
    ws3 <- list_op(l1 = list_op(l1 = ws_IBL_forest,
                                l2 = lapply(list_op(l1 = list_op(l1 = avg_height_not_felled,
                                                                 l2 = zpd_forest,
                                                                 func = "-"),
                                                    l2 = r_forest,
                                                    func = "/"),
                                            FUN = log),
                                func = "*"),
                   l2 = list_op(l1 = lapply(list_op(l1 = lapply(h1,
                                                                FUN = round),
                                                    l2 = avg_height_not_felled,
                                                    func = "/"),
                                            FUN = function(x) exp(alpha * (x - 1))),
                                l2 = lapply(list_op(l1 = list_op(l1 = h_IBL_forest,
                                                                 l2 = zpd_forest,
                                                                 func = "-"),
                                                    l2 = r_forest,
                                                    func = "/"),
                                            log),
                                func = "/"),
                   func = "*") # this is true if h1 < avg_height_not_felled
  })
  ws <- lapply(seq(length(ws)),
               FUN = function(x) {
                 res <- ws[[x]]
                 sel_v2 <- h1[[x]] < h_IBL_forest[[x]] & h1[[x]] > avg_height_not_felled[[x]]
                 res[sel_v2] <- ws2[[x]][sel_v2]

                 sel_v3 <- h1[[x]] < h_IBL_forest[[x]] & h1[[x]] < avg_height_not_felled[[x]]
                 res[sel_v3] <- ws3[[x]][sel_v3]

                 return(res)
               })

  if (any(is.na(unlist(ws)))) {
    stop("getWS_forest: NaNs produced, not filtered by conditionals")
  }

  # ws <- lapply(ws,
  #              FUN = function(x) {
  #                x[x<0] <- 0
  #                return(x)
  #              })

  names(ws) <- names(h1)

  return(ws)
}

getWS_felled <- function(h1, h_IBL_forest, h_IBL_felled, avg_height_not_felled, r_upstream, r_forest, ws_IBL_forest, ws_IBL_felled, zpd_forest, von_karman = 0.4, alpha = 4) {

  suppressWarnings({ # NaNs can be produces, however these should be filtered by conditionals
    ws <- getWS_forest(lapply(h1,
                              FUN = round),
                       h_IBL_forest,
                       avg_height_not_felled,
                       r_upstream,
                       r_forest,
                       ws_IBL_forest,
                       zpd_forest) # true if h1 > h_IBL_felled

    ws2 <- list_op(l1 = ws_IBL_felled,
                   l2 = list_op(l1 = lapply(list_op(l1 = lapply(h1,
                                                                FUN = round),
                                                    l2 = r_upstream,
                                                    func = "/"),
                                            FUN = log),
                                l2 = lapply(list_op(l1 = h_IBL_felled,
                                                    l2 = r_upstream,
                                                    func = "/"),
                                            FUN = log),
                                func = "/"),
                   func = "*")

  })
  ws <- lapply(seq(length(ws)),
               FUN = function(x) {
                 res <- ws[[x]]
                 sel_v2 <- h_IBL_felled[[x]] > h1[[x]]
                 res[sel_v2] <- ws2[[x]][sel_v2]

                 return(res)
               })

  if (any(is.na(unlist(ws)))) {
    stop("getWS_felled: NaNs produced, not filtered by conditionals")
  }

  ws <- lapply(ws,
               FUN = function(x) {
                 x[x<0] <- 0
                 return(x)
               })

  names(ws) <- names(h1)

  return(ws)
}

getWS_replant <- function(h1, h_IBL_forest, h_IBL_felled, h_IBL_replant, avg_height_not_felled, r_upstream, r_forest, r_SRF, ws_IBL_forest, ws_IBL_felled, ws_IBL_replant, zpd_forest, von_karman = 0.4, alpha = 4) {

  suppressWarnings({ # NaNs can be produces, however these should be filtered by conditionals
    ws <- getWS_felled(lapply(h1,
                              FUN = round),
                       h_IBL_forest,
                       h_IBL_felled,
                       avg_height_not_felled,
                       r_upstream,
                       r_forest,
                       ws_IBL_forest,
                       ws_IBL_felled,
                       zpd_forest) # true if h1 > h_IBL_replant

    ws2 <- list_op(l1 = ws_IBL_replant,
                   l2 = list_op(l1 = lapply(list_op(l1 = lapply(h1,
                                                                FUN = round),
                                                    l2 = r_SRF,
                                                    func = "/"),
                                            FUN = log),
                                l2 = lapply(list_op(l1 = h_IBL_replant,
                                                    l2 = r_SRF,
                                                    func = "/"),
                                            FUN = log),
                                func = "/"),
                   func = "*")
  })

  ws <- lapply(seq(length(ws)),
               FUN = function(x) {
                 res <- ws[[x]]
                 sel_v2 <- h_IBL_replant[[x]] > lapply(h1,
                                                       FUN = round)[[x]]
                 res[sel_v2] <- ws2[[x]][sel_v2]

                 return(res)
               })

  if (any(is.na(unlist(ws)))) {
    stop("getWS_replant: NaNs produced, not filtered by conditionals")
  }

  ws <- lapply(ws,
               FUN = function(x) {
                 x[x<0] <- 0
                 return(x)
               })

  names(ws) <- names(h1)

  return(ws)
}
