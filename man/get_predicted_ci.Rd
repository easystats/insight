% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_predicted_ci.R
\name{get_predicted_ci}
\alias{get_predicted_ci}
\alias{get_predicted_ci.default}
\title{Confidence and Prediction Interval for Model Predictions}
\usage{
get_predicted_ci(x, predictions = NULL, ...)

\method{get_predicted_ci}{default}(
  x,
  predictions = NULL,
  data = NULL,
  ci = 0.95,
  ci_type = "confidence",
  vcov_estimation = NULL,
  vcov_type = NULL,
  vcov_args = NULL,
  dispersion_method = "sd",
  ci_method = "quantile",
  ...
)
}
\arguments{
\item{x}{A statistical model (can also be a data.frame, in which case the
second argument has to be a model).}

\item{predictions}{A vector of predicted values (as obtained by
\code{stats::fitted()}, \code{stats::predict()} or
\code{\link{get_predicted}}).}

\item{...}{Not used for now.}

\item{data}{An optional data frame in which to look for variables with which
to predict. If omitted, the data used to fit the model is used.}

\item{ci}{The interval level (default \code{0.95}, i.e., 95\% CI).}

\item{ci_type}{Can be \code{"prediction"} or \code{"confidence"}. Prediction
intervals show the range that likely contains the value of a new
observation (in what range it would fall), whereas confidence intervals
reflect the uncertainty around the estimated parameters (and gives the
range of the link; for instance of the regression line in a linear
regressions). Prediction intervals account for both the uncertainty in the
model's parameters, plus the random variation of the individual values.
Thus, prediction intervals are always wider than confidence intervals.
Moreover, prediction intervals will not necessarily become narrower as the
sample size increases (as they do not reflect only the quality of the fit).
This applies mostly for "simple" linear models (like \code{lm}), as for
other models (e.g., \code{glm}), prediction intervals are somewhat useless
(for instance, for a binomial model for which the dependent variable is a
vector of 1s and 0s, the prediction interval is... \code{[0, 1]}).}

\item{vcov_estimation}{String, indicating the suffix of the
\code{vcov*()}-function from the \pkg{sandwich} or \pkg{clubSandwich}
package, e.g. \code{vcov_estimation = "CL"} (which calls
\code{\link[sandwich]{vcovCL}} to compute clustered covariance matrix
estimators), or \code{vcov_estimation = "HC"} (which calls
\code{\link[sandwich:vcovHC]{vcovHC()}} to compute
heteroskedasticity-consistent covariance matrix estimators).}

\item{vcov_type}{Character vector, specifying the estimation type for the
robust covariance matrix estimation (see
\code{\link[sandwich:vcovHC]{vcovHC()}} or \code{clubSandwich::vcovCR()}
for details).}

\item{vcov_args}{List of named vectors, used as additional arguments that are
passed down to the \pkg{sandwich}-function specified in
\code{vcov_estimation}.}

\item{dispersion_method, ci_method}{These arguments are only used in
the context of bootstrapped and Bayesian models. Possible values are
\code{dispersion_method = c("sd", "mad")} and
\code{ci_method = c("quantile", "hdi", "eti")}. For the latter, the
\pkg{bayestestR} package is required.}
}
\value{
The Confidence (or Prediction) Interval (CI).
}
\description{
Returns the Confidence (or Prediction) Interval (CI) associated with
predictions made by a model.
}
\examples{
data(mtcars)

# Linear model
# ------------
x <- lm(mpg ~ cyl + hp, data = mtcars)
predictions <- predict(x)
ci_vals <- get_predicted_ci(x, predictions, ci_type = "prediction")
head(ci_vals)
ci_vals <- get_predicted_ci(x, predictions, ci_type = "confidence")
head(ci_vals)
ci_vals <- get_predicted_ci(x, predictions, ci = c(0.8, 0.9, 0.95))
head(ci_vals)

# Bootstrapped
# ------------
predictions <- get_predicted(x, iterations = 500)
get_predicted_ci(x, predictions)

if (require("bayestestR")) {
  ci_vals <- get_predicted_ci(x, predictions, ci = c(0.80, 0.95))
  head(ci_vals)
  bayestestR::reshape_ci(ci_vals)

  ci_vals <- get_predicted_ci(x,
    predictions,
    dispersion_method = "MAD",
    ci_method = "HDI"
  )
  head(ci_vals)
}


# Logistic model
# --------------
x <- glm(vs ~ wt, data = mtcars, family = "binomial")
predictions <- predict(x, type = "link")
ci_vals <- get_predicted_ci(x, predictions, ci_type = "prediction")
head(ci_vals)
ci_vals <- get_predicted_ci(x, predictions, ci_type = "confidence")
head(ci_vals)
}
