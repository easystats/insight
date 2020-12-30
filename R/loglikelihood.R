#' Log-Likelihood
#'
#' A robust function to compute the log-likelihood of a model. Can be used as a replacement for \code{stats::logLik()} out of the box, as the returned object is of the same class (and it gives the same results when \code{estimator = "ML"} is specified).
#'
#' @param estimator Corresponds to the different estimators for the standard deviation of the errors. If \code{estimator="ML"} (default), the scaling is done by n (the biased ML estimator), which is then equivalent to using \code{stats::logLik()}. If \code{estimator="OLS"}, it returns the unbiased OLS estimator (scaling by the degrees of freedom, i.e., \code{n-k}). In moderately large samples, the differences should be negligible, but it is possible that OLS would perform slightly better in small samples with Gaussian errors. See also \href{https://stats.stackexchange.com/questions/155474/why-does-lrtest-not-match-anovatest-lrt}{this thread} about a concrete application of these variants.
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
get_loglikelihood.default <- function(x, ...){
  stats::logLik(x, ...)
}



#' @importFrom stats df.residual dnorm
#' @rdname get_loglikelihood
#' @export
get_loglikelihood.lm <- function(x, estimator = "ML", ...) {
  if (is.character(estimator)) {
    if (tolower(estimator) == "ols") {
      # TODO: Replace df.residual by a more robust function
      estim <- function(x) sqrt(sum(get_residuals(x)^2) / stats::df.residual(x))
    } else {
      # In which case this is equivalent to `stats::logLik(x, ...)`
      estim <- function(x) sqrt(mean(get_residuals(x)^2))
    }
    estimator <- estim(x)
  }
  lls <- stats::dnorm(get_response(x), mean = get_predicted(x), sd = estimator, log = TRUE)

  .loglikelihood_prep_output(x, lls)
}





# TODO: Complete for other families with https://github.com/cran/nonnest2/blob/master/R/llcont.R



#' @importFrom stats weights deviance dbinom dpois dgamma
#' @rdname get_loglikelihood
#' @export
get_loglikelihood.glm <- function(x, ...) {
  fam <- x$family$family
  resp <- get_response(x)
  wt <- stats::weights(x)
  dev <- stats::deviance(x)
  disp <- dev / sum(wt)
  predicted <- get_predicted(x)

  # Calculate Log Likelihoods depending on the family
  lls <- switch(
    fam,
    binomial = {
      if (is.matrix(resp)) {
        n <- apply(resp, 1, sum)
        resp <- ifelse(n == 0, 0, resp[, 1] / n)
      } else {
        n <- rep.int(1, length(resp))
      }
      m <- if (any(n > 1)) n else wt
      wt <- ifelse(m > 0, (wt / m), 0)
      stats::dbinom(round(m * resp), round(m), predicted, log = TRUE) * wt
    },
    quasibinomial = {
      NA
    },
    poisson = {
      stats::dpois(resp, predicted, log = TRUE) * wt
    },
    quasipoisson = {
      NA
    },
    gaussian = {
      nobs <- length(resp)
      -((log(dev / nobs * 2 * pi) + 1) - log(wt)) / 2
    },
    inverse.gaussian = {
      -((log(disp * 2 * pi) + 1) + 3 * log(resp)) / 2
    },
    Gamma = {
      stats::dgamma(resp, shape = 1 / disp, scale = predicted * disp, log = TRUE) * wt
    }
  )

  .loglikelihood_prep_output(x, lls)
}


#' @export
get_loglikelihood.glmer <- get_loglikelihood.glm

# Helpers -----------------------------------------------------------------

.loglikelihood_prep_output <- function(x, lls) {
  # Prepare output
  out <- sum(lls)
  attr(out, "per_obs") <- lls # This is useful for some models comparison tests
  attr(out, "nall") <- attr(out, "nobs") <- n_obs(x)

  # See https://stats.stackexchange.com/questions/393016/what-does-the-degree-of-freedom-df-mean-in-the-results-of-log-likelihood-logl
  attr(out, "df") <- tryCatch({
    attributes(stats::logLik(x))$df
  }, warning = function(warning_condition) {
    length(find_parameters(x, effects = "all", component = "all", flatten = TRUE))
  }, error = function(error_condition) {
    length(find_parameters(x, effects = "all", component = "all", flatten = TRUE))
  })


  class(out) <- c("logLik", class(x)) # The class returned by stats::logLik(x)
  out
}
