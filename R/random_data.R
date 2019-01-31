#' @title Get the data from a model's random effects terms
#' @name random_data
#'
#' @description Returns the data of all random effect terms.
#'
#' @inheritParams model_random
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
#'   sample(1:30, size = sum(filter_group), replace = TRUE)
#' }
#'
#' m <- lmer(
#'   Reaction ~ Days + (1 | mygrp / mysubgrp) + (1 | Subject),
#'   data = sleepstudy
#' )
#'
#' random_data(m)
#'
#' @export
random_data <- function(x, ...) {
  model_data(x, effects = "random")
}
