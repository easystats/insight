#' @title Find names of random effect terms
#' @name find_random
#'
#' @description to do...
#'
#' @param x A fitted mixed model.
#' @param split_nested Logical, if \code{TRUE}, terms from nested random
#'   effects will be returned as separeted elements, not as single string
#'   with colon. See 'Examples'.
#' @param ... Currently not used.
#'
#' @inheritParams find_predictors
#' @inheritParams find_terms
#'
#' @return The name(s) of the random effects as character vector; for mixed models
#'    with zero-inflated component, if \code{component = "all"}, a list with
#'    the names of the random effects from the conditional and zero-inflated
#'    model parts.
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
find_random <- function(x, split_nested = FALSE, ...) {
  UseMethod("find_random")
}


#' @importFrom stats formula
#' @export
find_random.default <- function(x, split_nested = FALSE, ...) {
  # make sure we have no invalid component request
  dots <- list(...)
  if (obj_has_name(dots, "component")) {
    if (dots$component %in% c("zero_inflated", "zi", "disp", "dispersion"))
      return(NULL)
  }

  f <- tryCatch(
    {stats::formula(x)},
    error = function(x) { NULL }
  )
  get_model_random(f, split_nested)
}


#' @export
find_random.hurdle <- function(x, ...) {
  NULL
}

#' @export
find_random.zeroinfl <- function(x, ...) {
  NULL
}

#' @export
find_random.zerotrunc <- function(x, ...) {
  NULL
}

#' @importFrom stats formula
#' @export
find_random.lme <- function(x, split_nested = FALSE, ...) {
  # make sure we have no invalid component request
  dots <- list(...)
  if (obj_has_name(dots, "component")) {
    if (dots$component %in% c("zero_inflated", "zi", "disp", "dispersion"))
      return(NULL)
  }

  f <- tryCatch(
    {find_formula(x, effects = "random")},
    error = function(x) { NULL }
  )
  get_model_random(f, split_nested)
}


#' @importFrom stats formula
#' @export
find_random.brmsfit <- function(x, split_nested = FALSE, ...) {
  # make sure we have no invalid component request
  dots <- list(...)
  if (obj_has_name(dots, "component")) {
    if (dots$component %in% c("zero_inflated", "zi", "disp", "dispersion"))
      return(NULL)
  }

  f <- tryCatch(
    {stats::formula(x)[[1]]},
    error = function(x) { NULL }
  )
  get_model_random(f, split_nested)
}


#' @export
find_random.MixMod <- function(x, split_nested = FALSE, ...) {

  dots <- list(...)
  if (obj_has_name(dots, "component")) {
    if (dots$component %in% c("disp", "dispersion")) {
      warning("Dispersion-parameters can not be returned for random effects.")
      return(NULL)
    }
  }

  ## TODO currently, MixMod only supports same random effects structure for both cond. and zi-model
  ## TODO fix this once nested random effects are possible in MixMod
  x$id_name
}


#' @rdname find_random
#' @importFrom stats formula
#' @export
find_random.glmmTMB <- function(x, split_nested = FALSE, component = c("all", "conditional", "zi", "zero_inflated", "dispersion"), flatten = FALSE, ...) {
  component = match.arg(component)

  if (component == "dispersion") {
    warning("Dispersion-parameters can not be returned for random effects.")
    return(NULL)
  }

  f <- tryCatch(
    {
      f.cond = stats::formula(x)
      f.zi = stats::formula(x, component = "zi")

      switch(
        component,
        all = list(conditional = f.cond, zero_inflated = f.zi),
        conditional = list(conditional = f.cond),
        zi = ,
        zero_inflated = list(zero_inflated = f.zi)
      )
    },
    error = function(x) { NULL }
  )

  re <- compact_list(lapply(f, get_model_random, split_nested))

  if (flatten || length(re) == 1)
    unlist(re)
  else
    re
}


get_model_random <- function(f, split_nested = FALSE) {
  if (!requireNamespace("lme4", quietly = TRUE))
    stop("To use this function, please install package 'lme4'.")

  re <- sapply(lme4::findbars(f), deparse)
  re <- trim(substring(re, regexpr(pattern = "\\|", re) + 1))

  if (split_nested)
    unique(unlist(strsplit(re, "\\:")))
  else
    unique(re)
}
