#' @title Get names of random effect terms
#' @name model_random
#'
#' @description to do...
#'
#' @param x A fitted mixed model.
#' @param ... Currently not used.
#'
#' @return The name(s) of the random effects as character vector.
#'
#' @examples
#' ## TODO
#'
#' @export
model_random <- function(x, ...) {
  UseMethod("model_random")
}


#' @importFrom stats formula
#' @export
model_random.brmsfit <- function(x, ...) {
  f <- tryCatch(
    {stats::formula(x)[[1]]},
    error = function(x) { NULL }
  )
  get_model_random(f)
}


#' @export
model_random.MixMod <- function(x, ...) {
  x$id_name
}


#' @importFrom stats formula
#' @export
model_random.default <- function(x, ...) {
  f <- tryCatch(
    {stats::formula(x)},
    error = function(x) { NULL }
  )
  get_model_random(f)
}


get_model_random <- function(f) {
  if (!requireNamespace("lme4"))
    stop("To use this function, please install package 'lme4'.")

  re <- sapply(lme4::findbars(f), deparse)
  trim(substring(re, regexpr(pattern = "\\|", re) + 1))
}
