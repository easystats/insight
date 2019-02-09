#' find_parameters
#' @importFrom stats coef
#' @param model model
#' @export
find_parameters <- function(model) {
  # TODO ENHANCE
  return(names(coef(model)))
}
