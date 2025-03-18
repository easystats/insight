#' @title Find names of random effects
#' @name find_random
#'
#' @description Return the name of the grouping factors from mixed effects models.
#'
#' @param x A fitted mixed model.
#' @param split_nested Logical, if `TRUE`, terms from nested random
#'   effects will be returned as separated elements, not as single string
#'   with colon. See 'Examples'.
#'
#' @inheritParams find_predictors
#' @inheritParams find_variables
#'
#' @return A list of character vectors that represent the name(s) of the
#' random effects (grouping factors). Depending on the model, the
#' returned list has following elements:
#'
#' - `random`, the "random effects" terms from the conditional part of model
#' - `zero_inflated_random`, the "random effects" terms from the zero-inflation
#'   component of the model
#'
#' @examplesIf require("lme4", quietly = TRUE)
#' data(sleepstudy, package = "lme4")
#' sleepstudy$mygrp <- sample(1:5, size = 180, replace = TRUE)
#' sleepstudy$mysubgrp <- NA
#' for (i in 1:5) {
#'   filter_group <- sleepstudy$mygrp == i
#'   sleepstudy$mysubgrp[filter_group] <-
#'     sample(1:30, size = sum(filter_group), replace = TRUE)
#' }
#'
#' m <- lme4::lmer(
#'   Reaction ~ Days + (1 | mygrp / mysubgrp) + (1 | Subject),
#'   data = sleepstudy
#' )
#'
#' find_random(m)
#' find_random(m, split_nested = TRUE)
#' @export
find_random <- function(x, split_nested = FALSE, flatten = FALSE) {
  UseMethod("find_random")
}

#' @export
find_random.default <- function(x, split_nested = FALSE, flatten = FALSE) {
  f <- find_formula(x, verbose = FALSE)

  if (is_multivariate(x)) {
    rn <- names(find_response(x))
    l <- lapply(rn, function(i) .find_random_effects(x, f[[i]], split_nested))
    names(l) <- rn
    l <- compact_list(l)
  } else {
    l <- .find_random_effects(x, f, split_nested)
  }

  if (is_empty_object(l)) {
    return(NULL)
  }

  if (flatten) {
    unique(unlist(l, use.names = FALSE))
  } else {
    l
  }
}

#' @export
find_random.afex_aov <- function(x, split_nested = FALSE, flatten = FALSE) {
  if (flatten) {
    attr(x, "id")
  } else {
    list(random = attr(x, "id"))
  }
}


.find_random_effects <- function(x, f, split_nested) {
  # potential components that can have random effects
  components <- c("random", "zero_inflated_random")

  # for brms, we can have random effects for auxilliary elements, too
  if (inherits(x, "brmsfit")) {
    components <- c(components, paste0(.brms_aux_elements(), "_random"))
  }
  # for glmmTMB, we can have random effects for dispersion component, too
  if (inherits(x, "glmmTMB")) {
    components <- c(components, "dispersion_random")
  }

  # check which components we have
  components <- components[vapply(components, function(i) object_has_names(f, i), logical(1))]

  # if nothing, return null
  if (!length(components)) {
    return(NULL)
  }

  out <- lapply(components, function(comp) {
    if (is.list(f[[comp]])) {
      unique(unlist(lapply(
        f[[comp]],
        .get_model_random,
        model = x,
        split_nested = split_nested
      ), use.names = FALSE))
    } else {
      unique(unlist(
        .get_model_random(f[[comp]], model = x, split_nested),
        use.names = FALSE
      ))
    }
  })

  names(out) <- components
  compact_list(out)
}
