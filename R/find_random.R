#' @title Find names of random effect terms
#' @name find_random
#'
#' @description to do...
#'
#' @param x A fitted mixed model.
#' @param split_nested Logical, if \code{TRUE}, terms from nested random
#'   effects will be returned as separeted elements, not as single string
#'   with colon. See 'Examples'.
#'
#' @inheritParams find_predictors
#' @inheritParams find_terms
#'
#' @return A list of character vectors that represent the name(s) of the
#'    random effects (grouping factors). Depending on the model, the
#'    returned list has following elements:
#'    \itemize{
#'      \item \code{random}, the "random effects" terms from the conditional part of model
#'      \item \code{zero_inflated_random}, the "random effects" terms from the zero-inflation component of the model
#'    }
#'
#' @examples
#' library(lme4)
#' data(sleepstudy)
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
#' find_random(m)
#' find_random(m, split_nested = TRUE)
#'
#' @export
find_random <- function(x, split_nested = FALSE, flatten = FALSE) {
  f <- find_formula(x)

  r1 <- unique(unlist(lapply(f$random, function(.x) get_model_random(.x, split_nested, inherits(x, "MCMCglmm")))))
  r2 <- unique(unlist(lapply(f$zero_inflated_random, function(.x) get_model_random(.x, split_nested, inherits(x, "MCMCglmm")))))

  l <- compact_list(list(random = r1, zero_inflated_random = r2))

  if (flatten)
    unique(unlist(l))
  else
    l
}
