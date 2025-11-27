## 5c. Volume of peat drained

#' AV_peat_drained
#' @param core.dat UI data
#' @param construct.dat UI construction data
#' @return AV_indirect
#' @export
AV_peat_drained <- function(core.dat, construct.dat) {

  # Wrapper function for the AV_peat_drained0() module
  # THIS FUNCTION...

  ## Compute foundation and hardstanding dimensions
  if (core.dat$Foundations$found_in[1] == 1) { # pool all foundation/hardstanding
    l_f <- core.dat$Foundations$l_found
    w_f <- core.dat$Foundations$w_found

    l_h <- core.dat$Foundations$l_hardstand
    w_h <- core.dat$Foundations$w_hardstand

    # Get total no turbines
    n_turb <- core.dat$Windfarm$n_turb

    # Sum total length/width of foundations + hardstandings
    l_fh <- l_f + l_h
    w_fh <- w_f + w_h

    # Get max depths
    d_f <- core.dat$Foundations$d_peat_rem_found
    d_h <- core.dat$Foundations$d_peat_rem_hardstand

    d_fh <- apply(cbind(d_f,d_h), MAR=1, FUN=max)
  } else { # each area considered individually (produces more drainage due to increased edge effects)
    l_f <- map(construct.dat, "l_found_bott") # 'bottom' width used in this case
    w_f <- map(construct.dat, "w_found_bott")

    l_h <- map(construct.dat, "l_hardstand_bott") # 'bottom' width used in this case
    w_h <- map(construct.dat, "w_hardstand_bott")

    n_turb <- map(construct.dat, "n_turb")

    # Sum total length/width of foundations + hardstandings by area
    l_fh <- list_op(l1 = l_f, 
                    l2 = l_h, 
                    func = "+")
    w_fh <- list_op(l1 = w_f, 
                    l2 = w_h, 
                    func = "+")

    d_f <- map(construct.dat, "d_peat_rem_found")
    d_h <- map(construct.dat, "d_peat_rem_hardstand")

    # Extract maximum depth (foundations or hardstandings)
    d_fh <- list_op(l1 = d_f, 
                    l2 = d_h, 
                    func="max")
  }

  AV_indirect <- AV_peat_drained0(drain_ext = core.dat$Peatland$drain_ext,
                                  # borrow pit dimensions
                                  pit_dims = list(n_pit = core.dat$Borrow.pits$n_pit,
                                                  l_pit = core.dat$Borrow.pits$l_pit,
                                                  w_pit = core.dat$Borrow.pits$w_pit,
                                                  d_pit = core.dat$Borrow.pits$d_pit),
                                  # foundation/hardstand dimensions
                                  fh_dims = list(n = n_turb,
                                                 l = l_fh,
                                                 w = w_fh,
                                                 d = d_fh),
                                  # access track dimensions
                                  at_dims = list(float = list(l = core.dat$Access.tracks$l_float,
                                                              w = core.dat$Access.tracks$w_float,
                                                              d = core.dat$Access.tracks$d_float_drain),
                                                 track = list(l = core.dat$Access.tracks$l_track,
                                                              w = core.dat$Access.tracks$w_track,
                                                              d = core.dat$Access.tracks$d_track),
                                                 rock = list(l = core.dat$Access.tracks$l_rock_drain,
                                                             w = core.dat$Access.tracks$w_rock,
                                                             d = core.dat$Access.tracks$d_rock_drain)),
                                  # cable trench dimensions
                                  ct_dims = list(l = core.dat$Cable.trenches$l_cab_trench,
                                                 d = core.dat$Cable.trenches$d_cab_trench),
                                  # additional excavation dimensions
                                  add_dims = list(v = core.dat$Add.excavation$V_add,
                                                  a = core.dat$Add.excavation$A_add))

  return(AV_indirect)
}

#' area_drained
#' @param drain_ext average extent of drainage around drainage features
#' @param l length of drainage feature
#' @param w width of drainage feature
#' @return Area of drained peat (assuming rectangular drainage profile)
#' @export
area_drained <- function(drain_ext, l, w) {
  return((2 * drain_ext + l) * (2 * drain_ext + w) - (l * w))
}

#' drainage_bp
#' @param drain_ext average extent of drainage around drainage features
#' @param pit_dims number, length, width and depth of borrow pits (list)
#' @return Area/Volume of peat drained due to borrow pits
#' @export
drainage_bp <- function(drain_ext,
                        pit_dims) {

  # THIS FUNCTION...
  area_drained_bp <- pit_dims$n_pit * area_drained(drain_ext, pit_dims$l_pit, pit_dims$w_pit)
  vol_drained_bp <- pit_dims$d_pit * area_drained_bp * 0.5 # not clear why the factor 2 here

  return(list(a = area_drained_bp,
              v = vol_drained_bp))
}

#' drainage_fh
#' @param drain_ext average extent of drainage around drainage features
#' @param fh_dims foundations + hardstanding dimensions
#' @return Area/Volume of peat drained due to foundations + hardstanding
#' @export
drainage_fh <- function(drain_ext,
                        fh_dims) {

  # THIS FUNCTION...

  if (class(fh_dims$l)=="list") { # dimensions passed as list: handle each area individually

    # drainage
    drainage_per_turb <- lapply(Map(list, fh_dims$l, fh_dims$w), FUN=function(x) area_drained(drain_ext, unlist(x[[1]]), unlist(x[[2]])))

    # multiply by n_turb
    drainage_wf <- list_op(l1 = drainage_per_turb, 
                           l2 = fh_dims$n, 
                           func = "*")

    # sum across areas
    area_drained_fh <- Reduce("+", drainage_wf)

    # compute volume (factor 2 due to assumption of a triangular depth profile) and sum
    vol_drained_fh <- Reduce("+", list_op(l1 = drainage_wf, 
                                          l2 = fh_dims$d, 
                                          func = "*0.5"))

  } else { # dimensions passed as vector: single area/areas pooled
    area_drained_fh <- area_drained(drain_ext, fh_dims$l, fh_dims$w) * fh_dims$n
    
    # compute volume (factor 2 due to assumption of a triangular depth profile)
    vol_drained_fh <- 0.5 * area_drained_fh * fh_dims$d
  }

  return(list(a = area_drained_fh,
              v = vol_drained_fh))
}

#' drainage_at
#' @param drain_ext average extent of drainage around drainage features
#' @param at_dims access track dimensions (list)
#' @return Area/Volume of peat drained due to access tracks
#' @export
drainage_at <- function(drain_ext,
                        at_dims) {

  # THIS FUNCTION...

  # Floating roads
  area_drained_float <- at_dims$float$l * (2*drain_ext + at_dims$float$w)
  vol_drained_float <- 0.5 * area_drained_float * at_dims$float$d

  # Excavated tracks
  area_drained_track <- at_dims$track$l * (2*drain_ext)
  vol_drained_track <- 0.5 * area_drained_track * at_dims$track$d

  # Rock-filled roads
  area_drained_rock <- at_dims$rock$l * (2*drain_ext)
  vol_drained_rock <- 0.5 * area_drained_track * at_dims$rock$d

  # Total drainage due to access tracks
  area_drained_at <- area_drained_float + area_drained_track + area_drained_rock

  vol_drained_at <- vol_drained_float + vol_drained_track + vol_drained_rock

  return(list(a = area_drained_at,
              v = vol_drained_at))
}

#' drainage_ct
#' @param drain_ext average extent of drainage around drainage features
#' @param ct_dims cable trench dimensions (list)
#' @return Area/Volume of peat drained due to cable trenches
#' @export
drainage_ct <- function(drain_ext,
                        ct_dims) {

  #THIS FUNCTION...

  area_drained_ct <- ct_dims$l * (2*drain_ext)
  vol_drained_ct <- 0.5 * area_drained_ct * ct_dims$d

  return(list(a = area_drained_ct,
              v = vol_drained_ct))
}

#' drainage_add
#' @param drain_ext average extent of drainage around drainage features
#' @param add_dims additional excavation dimensions (list)
#' @return Area/Volume of peat drained due to additional excavations
#' @export
drainage_add <- function(drain_ext,
                         add_dims) {

  #THIS FUNCTION...
  if (any(add_dims$v > 0)) {
    d_add <- add_dims$v / add_dims$a[c(1,3,2)] # re-order Min/Max inputs
    r_add <- sqrt(add_dims$a/pi)
    r_add_drained <- r_add + drain_ext
    area_drained_add <- pi * r_add_drained * r_add_drained - add_dims$a
    vol_drained_add <- area_drained_add * d_add
  } else {
    area_drained_add <- c(Exp=0, Min=0, Max=0)
    vol_drained_add <- c(Exp=0, Min=0, Max=0)
  }

  return(list(a = area_drained_add,
              v = vol_drained_add))
}

#' AV_peat_drained0
#' @param drain_ext average extent of drainage around drainage features
#' @param pit_dims borrow pit dimensions (list)
#' @param fh_dims foundations + hardstanding dimensions
#' @param at_dims access track dimensions (list)
#' @param ct_dims cable trench  dimensions (list)
#' @param add_dims additional excavation dimensions (list)
#' @return Area/Volume of peat drained
#' @export
AV_peat_drained0 <- function(drain_ext,
                             pit_dims,
                             fh_dims,
                             at_dims,
                             ct_dims,
                             add_dims) {

  # THIS FUNCTION...

  ## Drainage due to borrow pits
  drained_bp <- drainage_bp(drain_ext = drain_ext,
                            pit_dims = pit_dims)

  area_drained_bp <- drained_bp$a
  vol_drained_bp <- drained_bp$v

  ## Drainage due to foundations + hardstanding
  drained_fh <- drainage_fh(drain_ext = drain_ext,
                            fh_dims = fh_dims)

  area_drained_fh <- drained_fh$a
  vol_drained_fh <- drained_fh$v

  ## Drainage due to access tracks
  drained_at <- drainage_at(drain_ext = drain_ext,
                            at_dims = at_dims)

  area_drained_at <- drained_at$a
  vol_drained_at <- drained_at$v

  ## Drainage due to cable trenches
  drained_ct <- drainage_ct(drain_ext = drain_ext,
                            ct_dims = ct_dims)

  area_drained_ct <- drained_ct$a
  vol_drained_ct <- drained_ct$v

  ## Drainage due to additional excavation
  drained_add <- drainage_add(drain_ext = drain_ext,
                              add_dims = add_dims)

  area_drained_add <- drained_add$a
  vol_drained_add <- drained_add$v

  ## Total drainage
  area_drained <- area_drained_bp + area_drained_fh + area_drained_at + area_drained_ct + area_drained_add
  vol_drained <- vol_drained_bp + vol_drained_fh + vol_drained_at + vol_drained_ct + vol_drained_add

  return(list(Borrow.pits = list(a = area_drained_bp,
                                 v = vol_drained_bp),
              Foundations = list(a = area_drained_fh,
                                 v = vol_drained_fh),
              Access.tracks = list(a = area_drained_at,
                                   v = vol_drained_at),
              Cable.trenches = list(a = area_drained_ct,
                                    v = vol_drained_ct),
              Add.excavation = list(a = area_drained_add,
                                    v = vol_drained_add),
              Total = list(a = area_drained,
                           v = vol_drained)))
}
