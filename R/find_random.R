#' @title Find names of random effects
#' @name find_random
#'
#' @description Return the name of the grouping factors from mixed effects models.
#'
#' @param x A fitted mixed model.
#' @param split_nested Logical, if \code{TRUE}, terms from nested random
#'   effects will be returned as separated elements, not as single string
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
#'     sample(1:30, size = sum(filter_group), replace = TRUE)
#' }
#'
#' m <- lmer(
#'   Reaction ~ Days + (1 | mygrp / mysubgrp) + (1 | Subject),
#'   data = sleepstudy
#' )
#'
#' find_random(m)
#' find_random(m, split_nested = TRUE)
#' @export
find_random <- function(x, split_nested = FALSE, flatten = FALSE) {
  f <- find_formula(x)

  if (is_multivariate(x)) {
    rn <- names(find_response(x))
    l <- lapply(rn, function(i) .find_random_effects(x, f[[i]], split_nested))
    names(l) <- rn
    l <- .compact_list(l)
  } else {
    l <- .find_random_effects(x, f, split_nested)
  }


  if (is_empty_object(l)) {
    return(NULL)
  }

  if (flatten) {
    unique(unlist(l))
  } else {
    l
  }
}


.find_random_effects <- function(x, f, split_nested) {
  if (!obj_has_name(f, "random") && !obj_has_name(f, "zero_inflated_random")) {
    return(NULL)
  }

  if (obj_has_name(f, "random")) {
    if (is.list(f$random)) {
      r1 <- unique(unlist(lapply(f$random, function(.x) get_model_random(.x, split_nested, x))))
    } else {
      r1 <- unique(unlist(get_model_random(f$random, split_nested, x)))
    }
  } else {
    r1 <- NULL
  }


  if (obj_has_name(f, "zero_inflated_random")) {
    if (is.list(f$zero_inflated_random)) {
      r2 <- unique(unlist(lapply(f$zero_inflated_random, function(.x) get_model_random(.x, split_nested, x))))
    } else {
      r2 <- unique(get_model_random(f$zero_inflated_random, split_nested, x))
    }
  } else {
    r2 <- NULL
  }


  .compact_list(list(random = r1, zero_inflated_random = r2))
}