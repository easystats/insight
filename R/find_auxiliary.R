#' @title Find auxiliary (distributional) parameters from models
#'
#' @description Returns the names of all auxiliary / distributional parameters
#' from brms-models, like dispersion, sigma, kappa, phi, or beta...
#'
#' @name find_auxiliary
#'
#' @param x A model of class `brmsfit`.
#' @param ... Currently not used.
#'
#' @return The names of all available auxiliary parameters used in the model.
#'
#' @export
find_auxiliary <- function(x, ...) {
  UseMethod("find_auxiliary")
}


#' @export
find_auxiliary.default <- function(x, ...) {
  format_error("`find_auxiliary()` currently only works for models from package brms.")
}


#' @export
find_auxiliary.brmsfit <- function(x, ...) {
  # formula object contains "pforms", which includes all auxiliary parameters
  f <- stats::formula(x)
  if (object_has_names(f, "forms")) {
    out <- unique(unlist(lapply(f$forms, function(i) names(i$pforms)), use.names = FALSE))
  } else {
    out <- names(f$pforms)
  }
  # add sigma
  fe <- dimnames(x$fit)$parameters
  if (any(startsWith(fe, "sigma_") | grepl("sigma", fe, fixed = TRUE))) {
    out <- c(out, "sigma")
  }

  unique(out)
}
