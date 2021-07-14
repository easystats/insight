#' @title Get the data from random effects
#' @name get_random
#'
#' @description Returns the data from all random effects terms.
#'
#' @inheritParams find_random
#'
#' @return The data from all random effects terms, as data frame. Or \code{NULL}
#'    if model has no random effects.
#'
#' @examples
#' if (require("lme4")) {
#'   data(sleepstudy)
#'   # prepare some data...
#'   sleepstudy$mygrp <- sample(1:5, size = 180, replace = TRUE)
#'   sleepstudy$mysubgrp <- NA
#'   for (i in 1:5) {
#'     filter_group <- sleepstudy$mygrp == i
#'     sleepstudy$mysubgrp[filter_group] <-
#'       sample(1:30, size = sum(filter_group), replace = TRUE)
#'   }
#'
#'   m <- lmer(
#'     Reaction ~ Days + (1 | mygrp / mysubgrp) + (1 | Subject),
#'     data = sleepstudy
#'   )
#'
#'   head(get_random(m))
#' }
#' @export
get_random <- function(x) {
  UseMethod("get_random")
}

#' @export
get_random.default <- function(x) {
    if (.is_empty_object(find_random(x))) {
    warning("No random effects found in model.")
    return(NULL)
  }

  get_data(x, effects = "random")
}


#' @export
get_random.afex_aov <- function(x) {
  get_data(x, shape = "long")[find_random(x, flatten = TRUE)]
}