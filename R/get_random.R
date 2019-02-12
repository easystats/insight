#' @title Get the data from a model's random effects terms
#' @name get_random
#'
#' @description Returns the data of all random effects terms.
#'
#' @inheritParams find_random
#'
#' @return The data of all random effects terms, as data frame.
#'
#' @examples
#' library(lme4)
#' data(sleepstudy)
#' # prepare some data...
#' sleepstudy$mygrp <- sample(1:5, size = 180, replace = TRUE)
#' sleepstudy$mysubgrp <- NA
#' for (i in 1:5) {
#'   filter_group <- sleepstudy$mygrp == i
#'   sleepstudy$mysubgrp[filter_group] <-
#'     sample(1:30, size = sum(filter_group), replace = TRUE)
#' }
#'
#' m <- lmer(
#'   Reaction ~ Days + (1 | mygrp / mysubgrp) + (1 | Subject),
#'   data = sleepstudy
#' )
#'
#' head(get_random(m))
#' @export
get_random <- function(x) {
  if (is_empty_object(find_random(x))) {
    warning("No random effects found in model.")
    return(NULL)
  }


  get_data(x, effects = "random")
}
