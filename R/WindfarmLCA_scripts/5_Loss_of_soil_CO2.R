## 5. Loss of soil CO2

#' CO2_loss_from_soil
#' @param L_direct Carbon content of dry peat
#' @param L_indirect Dry soil bulk density
#' @return Total CO2 loss from removed and drained
#' @export
CO2_loss_from_soil <- function(L_direct, L_indirect) {

  return(L_direct + L_indirect$L_indirect)

}
