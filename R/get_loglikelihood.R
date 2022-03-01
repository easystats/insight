#' Log-Likelihood
#'
#' A robust function to compute the log-likelihood of a model, as well as
#' individual log-likelihoods (for each observation) whenever possible. Can be
#' used as a replacement for `stats::logLik()` out of the box, as the
#' returned object is of the same class (and it gives the same results by
#' default).
#'
#' @param estimator Corresponds to the different estimators for the standard
#'   deviation of the errors. If `estimator="ML"` (default), the scaling is
#'   done by n (the biased ML estimator), which is then equivalent to using
#'   `stats::logLik()`. If `estimator="OLS"`, it returns the unbiased
#'   OLS estimator.
#' @param REML Only for linear models. This argument is present for
#'   compatibility with `stats::logLik()`. Setting it to `TRUE` will
#'   overwrite the `estimator` argument and is thus equivalent to setting
#'   `estimator="REML"`. It will give the same results as
#'   `stats::logLik(..., REML=TRUE)`. Note that individual log-likelihoods
#'   are not available under REML.
#' @param check_response Logical, if `TRUE`, checks if the response variable
#'   is transformed (like `log()` or `sqrt()`), and if so, returns a corrected
#'   log-likelihood. To get back to the original scale, the likelihood of the
#'   model is multiplied by the Jacobian/derivative of the transformation.
#' @param ... Passed down to `logLik()`, if possible.
#' @inheritParams get_residuals
#'
#' @return An object of class `"logLik"`, also containing the
#'   log-likelihoods for each observation as a `per_observation` attribute
#'   (`attributes(get_loglikelihood(x))$per_observation`) when possible.
#'   The code was partly inspired from the \CRANpkg{nonnest2} package.
#'
#' @examples
#' x <- lm(Sepal.Length ~ Petal.Width + Species, data = iris)
#'
#' get_loglikelihood(x, estimator = "ML") # Equivalent to stats::logLik(x)
#' get_loglikelihood(x, estimator = "REML") # Equivalent to stats::logLik(x, REML=TRUE)
#' get_loglikelihood(x, estimator = "OLS")
#' @export
get_loglikelihood <- function(x, ...) {
  UseMethod("get_loglikelihood")
}


#' @rdname get_loglikelihood
#' @export
loglikelihood <- get_loglikelihood


#' @export
get_loglikelihood.default <- function(x, ...) {
  .loglikelihood_prep_output(x, lls = NA, ...)
}

#' @export
get_loglikelihood.model_fit <- function(x,
                                        estimator = "ML",
                                        REML = FALSE,
                                        check_response = FALSE,
                                        verbose = TRUE,
                                        ...) {

  get_loglikelihood(x$fit, estimator = estimator, REML = REML, check_response = check_response, verbose = verbose, ...)
}

#' @export
get_loglikelihood.afex_aov <- function(x, ...) {
  get_loglikelihood(x$lm, ...)
}




# Methods WITH individual LLs ---------------------------------------------

# TODO: Complete for other families with https://github.com/cran/nonnest2/blob/master/R/llcont.R
# https://stats.stackexchange.com/questions/322038/input-format-for-response-in-binomial-glm-in-r


.get_loglikelihood_lm <- function(x,
                                  estimator = "ML",
                                  REML = FALSE,
                                  check_response = FALSE,
                                  verbose = TRUE,
                                  ...) {


  # Replace arg if compatibility base R is activated
  if (REML) estimator <- "REML"

  # Get weights
  w <- get_weights(x, null_as_ones = TRUE)

  # Get LogLikelihood
  estimator <- tolower(estimator)

  # REML (directly returned)
  # TODO: Find a way of reversing this formula to pull the sums out and get individual lls
  if (estimator == "reml") {
    if (!"qr" %in% names(x)) {
      stop("REML estimation not available for this model.")
    }
    N <- get_df(x, type = "residual") # n_obs - p
    val <- 0.5 * (sum(log(w)) - N * (log(2 * pi) + 1 - log(N) + log(sum(w * get_residuals(x, verbose = verbose)^2))))
    p <- n_parameters(x, remove_nonestimable = TRUE)
    ll <- val - sum(log(abs(diag(x$qr$qr)[1:p])))
    return(.loglikelihood_prep_output(x, ll, check_response = check_response, verbose = verbose))
  }

  # Get S2
  s <- as.numeric(get_sigma(x))
  if (estimator == "ols") {
    s2 <- s^2
  } else if (estimator == "ml") {
    s2 <- (s * sqrt(get_df(x, type = "residual") / n_obs(x)))^2
  } else {
    stop("'estimator' should be one of 'ML', 'REML' or 'OLS'.")
  }
  # Get individual log-likelihoods
  lls <- 0.5 * (log(w) - (log(2 * pi) + log(s2) + (w * get_residuals(x, verbose = verbose)^2) / s2))

  .loglikelihood_prep_output(x, lls, check_response = check_response, verbose = verbose)
}


.get_loglikelihood_glm <- function(x, info, verbose = TRUE, ...) {
  fam <- info$family
  resp <- get_response(x, verbose = verbose)
  w <- get_weights(x, null_as_ones = TRUE)
  dev <- stats::deviance(x)
  disp <- dev / sum(w)
  predicted <- get_predicted(x, verbose = verbose)

  # Make adjustment for binomial models with matrix as input
  if (info$is_binomial) {
    resp <- .factor_to_numeric(resp, lowest = 0)
    if (!is.null(ncol(resp))) {
      n <- apply(resp, 1, sum)
      resp <- ifelse(n == 0, 0, resp[, 1] / n)
    } else {
      n <- rep.int(1, length(resp))
    }
    n <- if (any(n > 1)) n else w
    w <- ifelse(n > 0, (w / n), 0)
  }

  # Calculate Log Likelihoods depending on the family
  lls <- switch(fam,
    binomial = {
      stats::dbinom(round(n * resp), round(n), predicted, log = TRUE) * w
    },
    quasibinomial = {
      NA
    },
    poisson = {
      stats::dpois(resp, predicted, log = TRUE) * w
    },
    quasipoisson = {
      NA
    },
    gaussian = {
      nobs <- length(resp)
      -((log(dev / nobs * 2 * pi) + 1) - log(w)) / 2
    },
    inverse.gaussian = {
      -((log(disp * 2 * pi) + 1) + 3 * log(resp)) / 2
    },
    Gamma = {
      stats::dgamma(resp, shape = 1 / disp, scale = predicted * disp, log = TRUE) * w
    }
  )

  .loglikelihood_prep_output(x, lls)
}


#' @rdname get_loglikelihood
#' @export
get_loglikelihood.lm <- function(x,
                                 estimator = "ML",
                                 REML = FALSE,
                                 check_response = FALSE,
                                 verbose = TRUE,
                                 ...) {
  if (inherits(x, "list") && .obj_has_name(x, "gam")) {
    x <- x$gam
  }

  info <- model_info(x, verbose = FALSE)
  if (info$is_linear) {
    ll <- .get_loglikelihood_lm(x,
      estimator = estimator,
      REML = REML,
      check_response = check_response,
      verbose = verbose,
      ...
    )
  } else {
    ll <- .get_loglikelihood_glm(x, info = info, verbose = verbose, ...)
  }
  ll
}

#' @export
get_loglikelihood.ivreg <- get_loglikelihood.lm

#' @export
get_loglikelihood.glm <- get_loglikelihood.lm

#' @export
get_loglikelihood.merMod <- get_loglikelihood.lm

#' @export
get_loglikelihood.gam <- get_loglikelihood.lm

#' @export
get_loglikelihood.gamm <- get_loglikelihood.lm

#' @export
get_loglikelihood.list <- get_loglikelihood.lm

#' @export
get_loglikelihood.stanreg <- function(x, centrality = stats::median, ...) {
  # installed?
  check_if_installed("rstanarm")

  # Get posterior distribution of logliks
  mat <- rstanarm::log_lik(x)
  # Point estimate using the function passed as the centrality argument
  lls <- sapply(as.data.frame(mat), centrality)

  .loglikelihood_prep_output(x, lls)
}


# Methods WITHOUT individual LLs ---------------------------------------------


#' @export
get_loglikelihood.iv_robust <- function(x, ...) {
  res <- get_residuals(x)
  p <- x$rank
  w <- x$weights

  N <- length(res)

  if (is.null(w)) {
    w <- rep.int(1, N)
  } else {
    excl <- w == 0
    if (any(excl)) {
      res <- res[!excl]
      N <- length(res)
      w <- w[!excl]
    }
  }

  val <- 0.5 * (sum(log(w)) - N * (log(2 * pi) + 1 - log(N) + log(sum(w * res^2))))

  attr(val, "nall") <- N
  attr(val, "nobs") <- N
  attr(val, "df") <- p + 1
  class(val) <- "logLik"

  val
}




#' @export
get_loglikelihood.svycoxph <- function(x, ...) {
  .loglikelihood_prep_output(x, lls = x$ll[2], df = x$degf.resid)
}


#' @export
get_loglikelihood.crr <- function(x, ...) {
  x$loglik
}


#' @export
get_loglikelihood.plm <- function(x, ...) {
  res <- get_residuals(x)
  w <- get_weights(x, null_as_ones = TRUE)
  N <- n_obs(x)

  ll <- 0.5 * (sum(log(w)) - N * (log(2 * pi) + 1 - log(N) + log(sum(w * res^2))))

  .loglikelihood_prep_output(x, lls = ll, df = get_df(x, type = "model"))
}


#' @export
get_loglikelihood.cpglm <- get_loglikelihood.plm



# Helpers -----------------------------------------------------------------

.loglikelihood_prep_output <- function(x, lls = NA, df = NULL, check_response = FALSE, verbose = FALSE, ...) {
  # Prepare output
  if (all(is.na(lls))) {
    out <- stats::logLik(x, ...)
    attr(out, "per_obs") <- NA
  } else if (length(lls) == 1) {
    out <- lls
  } else {
    out <- sum(lls)
    attr(out, "per_obs") <- lls # This is useful for some models comparison tests
  }


  if (isTRUE(check_response)) {
    # check if we have transformed response, and if so, adjust LogLik
    response_transform <- find_transformation(x)
    if (!is.null(response_transform) && !identical(response_transform, "identity")) {
      ll_transform <- switch(
        response_transform,
        "log" = .ll_log_adjustment(x),
        .ll_jacobian_adjustment(x)
      )

      if (is.null(ll_transform) && isTRUE(verbose)) {
        warning(insight::format_message("Could not compute corrected log-likelihood for models with transformed response. Log-likelihood value is probably inaccurate."), call. = FALSE)
      } else {
        out[1] <- ll_transform
      }
    }
  }

  # Some attributes present in stats::logLik (not sure what nall does)
  attr(out, "nall") <- attr(out, "nobs") <- n_obs(x)

  # See https://stats.stackexchange.com/questions/393016/what-does-the-degree-of-freedom-df-mean-in-the-results-of-log-likelihood-logl
  if (is.null(df)) df <- get_df(x, type = "model")
  attr(out, "df") <- df

  # Make of same class as returned by stats::logLik(x)
  class(out) <- c("logLik", class(x))
  out
}




.ll_log_adjustment <- function(x) {
  tryCatch(
    {
      sum(stats::dlnorm(
        x = get_response(x),
        meanlog = stats::fitted(x),
        sdlog = get_sigma(x, ci = NULL, verbose = FALSE),
        log = TRUE
      ))
    },
    error = function(e) {
      NULL
    }
  )
}


.ll_jacobian_adjustment <- function(model) {
  tryCatch(
    {
      trans <- get_transformation(model)$transformation
      sum(log(
        diag(attr(with(
          get_data(model),
          stats::numericDeriv(
            expr = quote(trans(
              get(find_response(model))
            )),
            theta = find_response(model)
          )
        ), "gradient"))
      ))
    },
    error = function(e) {
      NULL
    }
  )
}
