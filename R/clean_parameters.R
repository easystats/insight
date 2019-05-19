#' @title Get clean names of model paramters
#' @name clean_parameters
#'
#' @description This function "cleans" names of model parameters by removing
#' patterns like \code{"r_"} or \code{"b[]"} (mostly applicable to Stan models)
#' and adding columns with information to which group or component parameters
#' belong (i.e. fixed or random, count or zero-inflated...)
#'
#' @param x A fitted model.
#'
#' @return A data frame with "cleaned" parameter names and information on
#'   group or component where parameters belong to.
#'
#' @examples
#' model <- download_model("brms_zi_2")
#' clean_parameters(model)
#' @export
clean_parameters <- function(x, ...) {
  UseMethod("clean_parameters")
}


#' @export
clean_parameters.brmsfit <- function(x, ...) {
  pars <- find_parameters(x, effects = "all", component = "all", flatten = FALSE)

}