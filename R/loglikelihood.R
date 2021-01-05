#' Log-Likelihood
#'
#' A robust function to compute the log-likelihood of a model, as as individual log-likelihoods (for each observation) whenever possible. Can be used as a replacement for \code{stats::logLik()} out of the box, as the returned object is of the same class (and it gives the same results when \code{estimator = "ML"} is specified).
#'
#' @param estimator Corresponds to the different estimators for the standard deviation of the errors. If \code{estimator="ML"} (default), the scaling is done by n (the biased ML estimator), which is then equivalent to using \code{stats::logLik()}. If \code{estimator="OLS"}, it returns the unbiased OLS estimator (scaling by the degrees of freedom, i.e., \code{n-k}). In moderately large samples, the differences should be negligible, but it is possible that OLS would perform slightly better in small samples with Gaussian errors. See also \href{https://stats.stackexchange.com/questions/155474/why-does-lrtest-not-match-anovatest-lrt}{this thread} about a concrete application of these variants.
#' @param ... Passed down to \code{logLik()}, if possible.
#' @inheritParams get_residuals
#'
#' @return An object of class "logLik", also containing the log-likelihoods for each observation as a "per_observation" attribute  (\code{attributes(get_loglikelihood(x))$per_observation}) when possible. The code was partly inspired from the \href{https://CRAN.R-project.org/package=nonnest2}{\code{nonnest2}} package.
#'
#' @examples
#' x <- lm(Sepal.Length ~ Petal.Width + Species, data = iris)
#'
#' get_loglikelihood(x, estimator = "ML") # Equivalent to stats::logLik(x)
#' get_loglikelihood(x, estimator = "OLS")
#' @export
get_loglikelihood <- function(x, ...) {
  UseMethod("get_loglikelihood")
}


#' @rdname get_loglikelihood
#' @export
loglikelihood <- get_loglikelihood


#' @importFrom stats logLik
#' @export
get_loglikelihood.default <- function(x, ...) {
  .loglikelihood_prep_output(x, lls=NA, ...)
}



#' @rdname get_loglikelihood
#' @export
get_loglikelihood.lm <- function(x, estimator = "ML", ...) {

  # Calculate s2
  s <- as.numeric(get_sigma(x))
  if (tolower(estimator) == "ols") {
    s2 <- s^2  # OLS
  } else{
    s2 <- (s * sqrt(get_df(x) / n_obs(x)))^2  # ML
  }

  # Get weights
  w <- get_weights(x, null_as_ones = TRUE)

  # Get individual log-likelihoods
  lls <- 0.5 * (log(w) - (log(2 * pi) + log(s2) + (w * get_residuals(x)^2) / s2))

  .loglikelihood_prep_output(x, lls)
}





# TODO: Complete for other families with https://github.com/cran/nonnest2/blob/master/R/llcont.R
# https://stats.stackexchange.com/questions/322038/input-format-for-response-in-binomial-glm-in-r


#' @importFrom stats weights deviance dbinom dpois dgamma
#' @rdname get_loglikelihood
#' @export
get_loglikelihood.glm <- function(x, ...) {
  fam <- family(x)$family
  resp <- get_response(x)
  w <- get_weights(x, null_as_ones = TRUE)
  dev <- stats::deviance(x)
  disp <- dev / sum(w)
  predicted <- get_predicted(x)

  # Make adjustment for binomial models with matrix as input
  if(fam == "binomial"){
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
  lls <- switch(
    fam,
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


#' @export
get_loglikelihood.glmer <- get_loglikelihood.glm



# Helpers -----------------------------------------------------------------

.loglikelihood_prep_output <- function(x, lls=NA, ...) {
  # Prepare output
  if(all(is.na(lls))){
    out <- stats::logLik(x, ...)
    attr(out, "per_obs") <- NA
  } else{
    out <- sum(lls)
    attr(out, "per_obs") <- lls  # This is useful for some models comparison tests
  }

  # Some attributes present in stats::logLik (not sure what nall does)
  attr(out, "nall") <- attr(out, "nobs") <- n_obs(x)

  # See https://stats.stackexchange.com/questions/393016/what-does-the-degree-of-freedom-df-mean-in-the-results-of-log-likelihood-logl
  attr(out, "df") <- get_df(x, type = "model")
  class(out) <- c("logLik", class(x))  # The class returned by stats::logLik(x)
  out
}
