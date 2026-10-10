#' @title Find names of random slopes
#' @name find_random_slopes
#'
#' @description Return the name of the random slopes from mixed effects models.
#'
#' @param x A fitted mixed model.
#'
#' @return A list of character vectors with the name(s) of the random slopes, or
#' `NULL` if model has no random slopes. Depending on the model, the returned
#' list has following elements:
#'
#' - `random`, the random slopes from the conditional part of model
#' - `zero_inflated_random`, the random slopes from the zero-inflation component
#'   of the model. For **brms**, this is named `zi_random`.
#' - `dispersion_random`, the random slopes from the dispersion component of the
#'   model
#'
#' Models of class `brmsfit` may also contain elements for auxiliary parameters.
#'
#' @examplesIf require("lme4", quietly = TRUE)
#' data(sleepstudy, package = "lme4")
#' m <- lme4::lmer(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)
#' find_random_slopes(m)
#' @export
find_random_slopes <- function(x) {
  random_slopes <- vector(mode = "list")

  # do we already have a formula?
  if (inherits(x, "insight_formula")) {
    f <- x
    x <- NULL
    # random components in formulas end with "random"
    components <- names(f)[endsWith(names(f), "random")]
  } else {
    f <- find_formula(x, verbose = FALSE)
    # potential components that can have random effects
    components <- c("random", "zero_inflated_random")
  }

  # for brms, we can have random effects for auxilliary elements, too
  if (inherits(x, "brmsfit")) {
    components <- unique(c(components, names(f)[endsWith(names(f), "_random")]))
  }
  # for glmmTMB, we can have random effects for dispersion component, too
  if (inherits(x, "glmmTMB")) {
    components <- c(components, "dispersion_random")
  }

  # check which components we have
  components <- components[vapply(
    components,
    function(i) object_has_names(f, i),
    logical(1)
  )]

  # if nothing, return null
  if (!length(components)) {
    return(NULL)
  }

  # in brms, "Intercept" is the explicit intercept, not a slope
  random_slopes <- lapply(components, function(comp) {
    .extract_random_slopes(f[[comp]], drop_intercept = inherits(x, "brmsfit"))
  })
  names(random_slopes) <- components
  random_slopes <- compact_list(random_slopes)

  if (is_empty_object(random_slopes)) {
    NULL
  } else {
    random_slopes
  }
}


.extract_random_slopes <- function(fr, drop_intercept = FALSE) {
  if (is.null(fr)) {
    return(NULL)
  }

  if (!is.list(fr)) {
    fr <- list(fr)
  }

  random_slope <- lapply(fr, function(f) {
    slope_terms <- .random_slope_terms(f)
    # no "|", e.g. lme formulas like ~ day
    if (is.null(slope_terms)) {
      slope_terms <- list(f)
    }
    re <- unlist(lapply(slope_terms, all.vars), use.names = FALSE)
    if (drop_intercept) {
      re <- re[re != "Intercept"]
    }
    re
  })

  unique(unlist(compact_list(random_slope)))
}


# the terms left of each "|" or "||". For brms terms like (1 + x | p | g),
# this also skips the correlation id "p".
.random_slope_terms <- function(x) {
  is_bar <- function(i) {
    is.call(i) && (identical(i[[1]], as.name("|")) || identical(i[[1]], as.name("||")))
  }
  if (!is.call(x)) {
    return(NULL)
  }
  if (is_bar(x)) {
    lhs <- x[[2]]
    while (is_bar(lhs)) {
      lhs <- lhs[[2]]
    }
    return(list(lhs))
  }
  out <- lapply(as.list(x)[-1], .random_slope_terms)
  out <- unlist(out, recursive = FALSE)
  if (length(out)) out else NULL
}
