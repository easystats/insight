#' @title Find auxiliary (distributional) parameters from models
#'
#' @description Returns the names of all auxiliary / distributional parameters
#' from brms-models, like dispersion, sigma, kappa, phi, or beta...
#'
#' @name find_auxiliary
#'
#' @param x A model of class `brmsfit`.
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


#' @rdname find_auxiliary
#' @export
find_auxiliary.default <- function(x, verbose = TRUE, ...) {
  if (verbose) {
    format_warning(
      "`find_auxiliary()` currently only works for models from package brms."
    )
  }
  NULL
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
  # "pforms" only contains those distributional parameters that were modelled
  # with a formula. "sigma" usually is estimated as a single (constant)
  # parameter, and thus is missing from "pforms" - we then have to look at the
  # parameter names of the related stan-model. Note that we must check for
  # *exact* matches here, else auxiliary parameters of custom families, like
  # "sigmabias" or "sigmadrift", would be mistaken for "sigma" (see #1224).
  if (!"sigma" %in% out && .brms_has_sigma(x)) {
    out <- c(out, "sigma")
  }
  unique(out)
}


# checks whether a brms-model has an estimated residual standard deviation
# ("sigma") that was *not* modelled as a distributional parameter (i.e. that
# has no formula, and hence does not appear in the model's "pforms")
.brms_has_sigma <- function(x) {
  fe <- dimnames(x$fit)$parameters
  # "sigma" for univariate models, "sigma1", "sigma2" etc. for mixture models
  if ("sigma" %in% fe || any(grepl("^sigma[0-9]+$", fe))) {
    return(TRUE)
  }
  # multivariate models have one sigma per response, named "sigma_<response>"
  if (!any(startsWith(fe, "sigma_"))) {
    return(FALSE)
  }
  # brms uses "cleaned" response names for those (e.g. "SepalLength" instead
  # of "Sepal.Length"), which are stored in the names of the response-vector
  resp <- find_response(x, combine = FALSE)
  any(fe %in% paste0("sigma_", c(resp, names(resp))))
}
