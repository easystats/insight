#' get_parameters
#' @importFrom stats coef
#' @param model model
#' @export
get_parameters <- function(model) {
  # TODO ENHANCE
  return(as.data.frame(model)[find_parameters(model)])
}