#' @title Get names of random effect terms
#' @name model_random
#'
#' @description to do...
#'
#' @param x A fitted mixed model.
#' @param split_nested Logical, if \code{TRUE}, terms from nested random
#'   effects will be returned as separeted elements, not as single string
#'   with colon. See 'Examples'.
#' @param ... Currently not used.
#'
#' @inheritParams model_predictors
#'
#' @return The name(s) of the random effects as character vector.
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
#' model_random(m)
#' model_random(m, split_nested = TRUE)
#'
#' @export
model_random <- function(x, split_nested = FALSE, ...) {
  UseMethod("model_random")
}


#' @importFrom stats formula
#' @export
model_random.default <- function(x, split_nested = FALSE, ...) {
  f <- tryCatch(
    {stats::formula(x)},
    error = function(x) { NULL }
  )
  get_model_random(f, split_nested)
}


#' @importFrom stats formula
#' @export
model_random.brmsfit <- function(x, split_nested = FALSE, ...) {
  f <- tryCatch(
    {stats::formula(x)[[1]]},
    error = function(x) { NULL }
  )
  get_model_random(f, split_nested)
}


#' @export
model_random.MixMod <- function(x, split_nested = FALSE, ...) {
  ## TODO fix this once nested random effects are possible in MixMod
  x$id_name
}


#' @rdname model_random
#' @importFrom stats formula
#' @export
model_random.glmmTMB <- function(x, split_nested = FALSE, zi = FALSE, ...) {
  f <- tryCatch(
    {
      cond_formula <- stats::formula(x)
      if (zi)
        zi_formula <- stats::formula(x, component = "zi")
      else
        zi_formula <- NULL
      list(cond_formula, zi_formula)
    },
    error = function(x) { NULL }
  )

  unique(c(
    get_model_random(f[[1]], split_nested),
    get_model_random(f[[2]], split_nested)
  ))
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
