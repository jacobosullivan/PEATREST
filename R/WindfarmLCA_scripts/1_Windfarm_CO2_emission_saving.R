## 1. Windfarm CO2 emission saving

#' Windfarm_output
#' @param p_cap capcity factor
#' @param n_turb number of turbines
#' @param c_turb maximum tubine power capacity
#' @return Total Windfarm energy output
#' @export
Windfarm_output <- function(p_cap,
                            n_turb,
                            c_turb,
                            input.dat = NULL) {

  ## This function will estimate annual windfarm output [MW] as a function
  ## of the number of turbines, maximum turbine capacity [MW] and the power
  ## capacity, either user input, or estimated from relative windspeeds

  # Calculate annual energy output
  if (class(p_cap) == "list") { # power capacity computed for each forestry area

    c_turb_area <- list()

    for (i in 1:length(p_cap)) {
      c_turb_area[[i]] <- c_turb
    }
    names(c_turb_area) <- names(p_cap)

    # e_out = (24 * 365) * (p_cap/100) * n_turb * c_turb
    e_out <- colSums(bind_rows(lapply(list_op(l1 = n_turb,
                                              l2 = c_turb_area,
                                              l3 = p_cap,
                                              func = "*"),
                                      FUN = function(x) 24 * 365 * x / 100)))
  } else {
    e_out <- (365*24) * (p_cap/100) * n_turb * c_turb
  }

  return(e_out)
}

#' Windfarm_emissions_saving
#' @param energy_output output of Windfarm_output
#' @param E_mat counterfactuals in matrix form with IDs as rownames
#' @return Total Windfarm CO2 emissions savings
#' @export
Windfarm_emissions_saving <- function(e_out,
                                      E_mat) {
  
  ## This function computes the saved emissions as a function of the windfarm power output [MW]
  ## and the user input counterfactuals (emissions factors) [t CO2 MW]

  # Calculate emissions savings
  e_out_mat <- matrix(e_out,
                      nrow = nrow(E_mat),
                      ncol = length(e_out),
                      byrow = T,
                      dimnames = dimnames(E_mat))
  S_fuel <- data.frame(Fuel=rownames(e_out_mat), e_out_mat * E_mat, row.names = NULL)

  return(S_fuel)
}

#' p_cap_windspeed
#' @param core.dat UI data
#' @param input.dat UI forestry data
#' @param R_windspeed_all estimated windspeed ratios
#' @return power capacity
#' @export
p_cap_windspeed <- function(core.dat, input.dat, R_windspeed_all) {

  ## This function computes the wind turbine power capacity as a function of 
  ## relative windspeeds [ms-1/ms-1]

  # Extract and restructure data for easy access
  windspeed <- list()
  P_max <- list()
  t_down <- list()
  R_windspeed <- R_windspeed_all$ratio_no_fell

  ii <- 1
  for (i in grep("Area", names(input.dat))) {
    windspeed[[ii]] <- input.dat$Windfarm$Vwind_site
    P_max[[ii]] <- 24 * 365 * core.dat$Windfarm$c_turb
    t_down[[ii]] <- input.dat$Windfarm$t_down

    sel_felled <- map(input.dat[grep("Area", names(input.dat))], .f = "A_harv_turb")[[ii]] > 0 & map(input.dat[grep("Area", names(input.dat))], .f = "D_width")[[ii]]
    sel_replant <-  map(input.dat[grep("Area", names(input.dat))], .f = "A_replant_turb")[[ii]]
    if (any(sel_felled & sel_replant)) {
      R_windspeed[[ii]][sel_felled & sel_replant] <- R_windspeed_all$ratio_felled_replant[[ii]][sel_felled & sel_replant]
    } else if (any(sel_felled)) {
      R_windspeed[[ii]][sel_felled] <- R_windspeed_all$ratio_felled[[ii]][sel_felled]
    }
    ii <- ii + 1
  }
  names(windspeed) <- grep("Area", names(input.dat), value = T)
  names(P_max) <- grep("Area", names(input.dat), value = T)
  names(t_down) <- grep("Area", names(input.dat), value = T)

  slope_pow_curve <- map(input.dat[grep("Area", names(input.dat))], .f = "slope_pow_curve")
  int_pow_curve <- map(input.dat[grep("Area", names(input.dat))], .f = "int_pow_curve")

  # Inset Vestas standard values if not passed by user
  if (any(unlist(lapply(slope_pow_curve, FUN=is.null)))) {
    slope_pow_curve[[which(unlist(lapply(slope_pow_curve, FUN=is.null)))]] <- c(Exp = 1392.5,
                                                                                Min = 1392.5,
                                                                                Max = 1392.5)

    int_pow_curve[[which(unlist(lapply(int_pow_curve, FUN=is.null)))]] <- c(Exp = -4291.9,
                                                                            Min = -4291.9,
                                                                            Max = -4291.9)
  }

  # P_act = a * V_upwind * r_wind + b
  P_act <- list_op(l1 = list_op(l1 = slope_pow_curve,
                                l2 = windspeed,
                                l3 = R_windspeed,
                                func = "*"),
                   l2 = int_pow_curve,
                   func = "+")

  # p_cap = (100 - t_down) * (P_act / P_max)
  p_cap <- list_op(l1 = list_op(l1 = lapply(t_down, FUN = function(x) 100 - x),
                                l2 = P_act,
                                func = "*"),
                   l2 = P_max,
                   func = "/")

  return(p_cap)
}
