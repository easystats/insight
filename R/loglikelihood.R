#' Log-Likelihood
#'
#' A robust function to compute the log-likelihood of a model.
#'
#' @param estimator Corresponds to the different estimators for the standard deviation of the errors. If \code{estimator="OLS"} (default), it returns the unbiased OLS estimator (scaling by the degrees of freedom, i.e., \code{n−k ()}). If \code{estimator="ML"}, the scaling is done by n (the biased ML estimator), which is then equivalent to using \code{stats::logLik()}. In moderately large samples, the differences should be negligible, but it is possible that OLS would perform slightly better in small samples with Gaussian errors.
#' @param summarize Currently not used.
#' @inheritParams get_residuals
#'
#' @examples
#' x <- lm(Sepal.Length ~ Petal.Width + Species, data=iris)
#'
#' get_loglikelihood(x, estimator = "OLS")
#' get_loglikelihood(x, estimator = "ML")  # Equivalent to stats::logLik(x)
#' @importFrom stats df.residual dnorm
#' @export
get_loglikelihood <- function(x, estimator = "OLS", summarize = TRUE, ...) {

  if (tolower(estimator) == "ols") {
    # TODO: Replace df.residual by a more robust function
    estim <- function(x) sqrt(sum(get_residuals(x)^2) / stats::df.residual(x))
  } else{
    # In which case this is equivalent to `stats::logLik(x, ...)`
    estim <- function(x) sqrt(mean(get_residuals(x)^2))
  }
  ll <- stats::dnorm(get_response(x), mean = get_predicted(x), sd = estim(x), log = TRUE)

  # Prepare output
  out <- sum(ll)
  attr(out, "per_obs") <- ll  # This is useful for some models comparison tests
  attr(out, "nall") <- attr(out, "nobs") <- n_obs(x)
  # Not sure about the following, this is only to match the printed output of stats::logLik()
  attr(out, "df") <- length(find_parameters(x, effects = "fixed", component = "conditional", flatten = TRUE)) + 1

  class(out) <- c("logLik", class(x))  # The class returned by stats::logLik(x)
  out
}


#' @rdname get_loglikelihood
#' @export
loglikelihood <- get_loglikelihood

# TODO: See what they do https://github.com/cran/nonnest2/blob/master/R/llcont.R
# As there seems to be different stuff going on depending on the model's family
