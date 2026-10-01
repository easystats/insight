#' @title Find auxiliary (distributional) parameters from models
#'
#' @description Returns the names of all auxiliary / distributional parameters
#' from brms-models, like dispersion, sigma, kappa, phi, or beta... For
#' univariate non-linear models (`nl = TRUE`), the non-linear parameters of `mu`
#' are no auxiliary parameters, and are not returned.
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
    # for non-linear models (`nl = TRUE`), "pforms" also contains the
    # non-linear parameters of "mu". These are no auxiliary parameters, their
    # coefficients belong to the conditional component (see #1076)
    out <- setdiff(names(f$pforms), .brms_nlpars(x))
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
  # for non-linear models with only non-linear parameters in "pforms",
  # `setdiff()` returns `character(0)`, but we want `NULL` as for other models
  # without auxiliary parameters
  if (!length(out)) {
    return(NULL)
  }
  unique(out)
}


# returns the names of the non-linear parameters of "mu" for univariate
# non-linear brms-models (`nl = TRUE`), or `NULL` for all other models
.brms_nlpars <- function(x) {
  if (!inherits(x, "brmsfit")) {
    return(NULL)
  }
  f <- stats::formula(x)
  if (object_has_names(f, "forms") || !isTRUE(attr(f$formula, "nl"))) {
    return(NULL)
  }
  bt <- .safe(brms::brmsterms(f))
  if (is.null(bt)) {
    return(NULL)
  }
  out <- bt$dpars$mu$used_nlpars
  # non-linear parameters can be nested, e.g. `nlf(a ~ c + d)`, so we also
  # need the non-linear parameters of the non-linear parameters of "mu"
  repeat {
    nested <- unlist(lapply(bt$nlpars[out], function(i) i$used_nlpars), use.names = FALSE)
    nested <- setdiff(nested, out)
    if (!length(nested)) {
      break
    }
    out <- c(out, nested)
  }
  out
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
