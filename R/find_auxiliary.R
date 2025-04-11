#' @title Find auxiliary (distributional) parameters from models
#'
#' @description Returns the names of all auxiliary / distributional parameters
#' from brms-models, like dispersion, sigma, kappa, phi, or beta...
#'
#' @name find_auxiliary
#'
#' @param x A model of class `brmsfit`.
#' @param use_alias Logical, if `TRUE`, some of the element names will be
#' renamed into more expressive aliases, like they are already returned by some
#' other functions. E.g., `zi` will be renamed to `zero_inflated`, `zoi` will be
#' renamed to `zero_one_inflated` and `coi` will be renamed to
#' `conditional_one_inflated`. By default, the names are not changed.
#' @param add_alias Logical, if `TRUE`, more expressive aliases for the names of
#' distributional parameters will be additionally returned, together with the
#' original names. If `use_alias = TRUE`, this argument will be ignored.
#' @param verbose Toggle warnings.
#' @param ... Currently not used.
#'
#' @return The names of all available auxiliary parameters used in the model, or
#' `NULL` if no auxiliary parameters were found.
#'
#' @export
find_auxiliary <- function(x, ...) {
  UseMethod("find_auxiliary")
}


#' @export
find_auxiliary.default <- function(x, verbose = TRUE, ...) {
  if (verbose) {
    format_warning("`find_auxiliary()` currently only works for models from package brms.")
  }
  NULL
}


#' @rdname find_auxiliary
#' @export
find_auxiliary.brmsfit <- function(x,
                                   use_alias = FALSE,
                                   add_alias = FALSE,
                                   verbose = TRUE,
                                   ...) {
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

  if ((use_alias || add_alias) && !is.null(out) && length(out)) {
    # add aliases (zoi = zero_one_inflated, coi = conditional_one_inflated)
    if ("zi" %in% out) {
      if (use_alias) {
        out[out == "zii"] <- "zero_inflated"
      } else {
        out <- c(out, "zero_inflated")
      }
    }
    if ("zoi" %in% out) {
      if (use_alias) {
        out[out == "zoi"] <- "zero_one_inflated"
      } else {
        out <- c(out, "zero_one_inflated")
      }
    }
    if ("coi" %in% out) {
      if (use_alias) {
        out[out == "coi"] <- "conditional_one_inflated"
      } else {
        out <- c(out, "conditional_one_inflated")
      }
    }
  }

  unique(out)
}
