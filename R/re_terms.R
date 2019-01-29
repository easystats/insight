#' @title Get names of random effect terms
#' @name re_terms
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
re_terms <- function(x, ...) {
  UseMethod("re_terms")
}


#' @importFrom stats formula
#' @export
re_terms.brmsfit <- function(x) {
  f <- tryCatch(
    {stats::formula(x)[[1]]},
    error = function(x) { NULL }
  )
  get_re_terms(f)
}


#' @export
re_terms.MixMod <- function(x) {
  x$id_name
}


#' @importFrom stats formula
#' @export
re_terms.default <- function(x) {
  f <- tryCatch(
    {stats::formula(x)},
    error = function(x) { NULL }
  )
  get_re_terms(f)
}


get_re_terms <- function(f) {
  if (!requireNamespace("lme4"))
    stop("To use this function, please install package 'lme4'.")

  re <- sapply(lme4::findbars(f), deparse)
  trim(substring(re, regexpr(pattern = "\\|", re) + 1))
}


