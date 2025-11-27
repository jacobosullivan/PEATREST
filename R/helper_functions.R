## Helper functions

#' list_op
#' @param l1 list object 1
#' @param l2 list_object 2
#' @param l3 list_object 3
#' @param func string describing elementwise operation
#' @return outcome of elementwise operation
#' @export
list_op <- function(l1, l2, l3 = NULL, func) {

  # THIS FUNCTION...

  if (func == "+") {
    res <- lapply(seq_along(l1), FUN=function(x) unlist(unname(l1[x])) + unlist(unname(l2[x])))
  } else if (func == "-") {
    res <- lapply(seq_along(l1), FUN=function(x) unlist(unname(l1[x])) - unlist(unname(l2[x])))
  } else if (func == "*") {
    res <- lapply(seq_along(l1), FUN=function(x) unlist(unname(l1[x])) * unlist(unname(l2[x])))
  } else if (func == "*0.5") {
    res <- lapply(seq_along(l1), FUN=function(x) 0.5 * unlist(unname(l1[x])) * unlist(unname(l2[x])))
  } else if (func == "max") {
    res <- lapply(seq_along(l1), FUN=function(x) apply(cbind(unlist(unname(l1[x])), unlist(unname(l2[x]))), MAR=1, FUN=max))
  } else if (func == "c") {
    res <- lapply(seq_along(l1), FUN=function(x) c(unlist(unname(l1[x])), unlist(unname(l2[x]))))
  } else if (func == "/") {
    res <- lapply(seq_along(l1), FUN=function(x) unlist(unname(l1[x])) / unlist(unname(l2[x])))
  }

  if (!is.null(l3)) {
    if (func == "+") {
      res <- lapply(seq_along(res), FUN=function(x) unlist(unname(res[x])) + unlist(unname(l3[x])))
    } else if (func == "-") {
      res <- lapply(seq_along(res), FUN=function(x) unlist(unname(res[x])) - unlist(unname(l3[x])))
    } else if (func == "*") {
      res <- lapply(seq_along(res), FUN=function(x) unlist(unname(res[x])) * unlist(unname(l3[x])))
    } else if (func == "*0.5") {
      res <- lapply(seq_along(res), FUN=function(x) unlist(unname(res[x])) * unlist(unname(l3[x])))
    } else if (func == "max") {
      res <- lapply(seq_along(res), FUN=function(x) apply(cbind(unlist(unname(res[x])), unlist(unname(l3[x]))), MAR=1, FUN=max))
    } else if (func == "c") {
      res <- lapply(seq_along(res), FUN=function(x) c(unlist(unname(res[x])), unlist(unname(l3[x]))))
    }
  }

  names(res) <- names(l1)

  return(res)
}

#' rest_dyn_mod flexible convergent function for modelling restoration dynmaics
#' @param t sequence of years for restoration phase
#' @param n shape parameter (user input)
#' @param ymin value of y at t=0 (drained rate)
#' @param ymax value of y at t=max(t) (restored rate)
#' @param convThresh proportion of ymax at which convergence assumed to occur (since y attains ymax at t=Inf)
#' @return rate sequence
#' @export
rest_dyn_mod <- function(t,n,ymin,ymax,convThresh=0.99) {

  # Bounded flexible function implemented in absence of mechanistic model for peatland restoration

  aa <- -log(1-convThresh)/(max(t)^n) # allows control over value of x at convergence point
  rate <- ymax + (ymin - ymax) * exp(-aa*(t^n))

  return(rate)
}
