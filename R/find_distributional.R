#' @title Find distributional parameters from models
#'
#' @description Returns all distributional parameters from brms-models, like
#' dispersion, sigma, kappa, phi, or beta...
#'
#' @name find_distributional
#'
#' @param x A model of class `brmsfit`.
#' @param ... Currently not used.
#'
#' @return All available distributional parameters used in the model.
#'
#' @export
find_distributional <- function(x, ...) {
  UseMethod("find_distributional")
}


#' @export
find_distributional.default <- function(x, ...) {
  format_error("`find_distributional()` currently only works for models from package brms.")
}


#' @export
find_distributional.brmsfit <- function(x, ...) {
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
