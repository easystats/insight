#' Get model parameters
#' @param x x.
#' @export
get_parameters <- function(x, ...) {
  # TODO ENHANCE
  return(as.data.frame(x)[find_parameters(x)$conditional])
}