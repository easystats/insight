#' Predicted values
#'
#' Returns values predicted by a model (i.e., fitted values). The Confidence/Credible
#' Intervals (CI) are stored as an attribute, which one can easily extract with
#' \code{as.data.frame()} (see examples below).
#'
#' @param ... Not used.
#' @param ci_type Can be \code{"prediction"} or \code{"confidence"}. Prediction intervals show the range that likely contains the value of a new observation (in what range it would fall), whereas confidence intervals reflect the uncertainty around the estimated parameters (and gives the range of the link; for instance of the regression line in a linear regressions). Prediction intervals account for both the uncertainty in the model's parameters, plus the random variation of the individual values. Thus, prediction intervals are always wider than confidence intervals. Moreover, prediction intervals will not necessarily become narrower as the sample size increases (as they do not reflect only the quality of the fit). This doesn't apply for GLMs, for which prediction intervals are somewhat useless (for instance, for a binomial model for which the dependent variable is a vector of 1s and 0s, the prediction interval is... \code{[0, 1]}).
#' @param ci The interval level (default \code{0.95}, i.e., 95\% CI).
#' @param transform Either \code{"response"} (default) or \code{"link"}. If "link", no transformation is applied and the values are on the scale of the linear predictors. If "response", the output is on the scale of the response variable. Thus for a default binomial model, "response" gives the predicted probabilities, and "link" makes predictions of log-odds (probabilities on logit scale).
#' @inheritParams get_residuals
#' @inheritParams stats::predict.lm
#'
#' @return The fitted values (i.e. predictions for the response).
#'
#' @note Currently, this function just calls \code{stats::fitted()}, but will
#' be extended to other objects that don't work with \code{stats::fitted()} in
#' future updates.
#'
#' @examples
#' data(mtcars)
#' x <- lm(mpg ~ cyl + hp, data = mtcars)
#' predicted <- get_predicted(x)
#' predicted
#'
#' # Get CI
#' attributes(predicted)$CI_low # Or CI_high
#' as.data.frame(predicted)
#' @export
get_predicted <- function(x, newdata = NULL, ...) {
  UseMethod("get_predicted")
}


#' @importFrom stats fitted predict
#' @export
get_predicted.default <- function(x, newdata = NULL, ...) {
  out <- tryCatch(
    {
      stats::predict(x, newdata = newdata, ...)
    },
    error = function(e) {
      NULL
    }
  )

  if (is.null(out)) {
    out <- tryCatch(
      {
        stats::fitted(x)
      },
      error = function(e) {
        NULL
      }
    )
  }
  out
}

#' @export
get_predicted.data.frame <- function(x, newdata = NULL, ...) {
  # This makes it pipe friendly; data %>% get_predicted(model)
  if (is.null(newdata)) {
    stop("Please provide a model to base the estimations on.")
  } else {
    get_predicted(newdata, x, ...)
  }
}


#' @importFrom stats predict qnorm qt
#' @export
get_predicted.lm <- function(x, newdata = NULL, ci = 0.95, ci_type = "confidence", ...) {
  rez <- stats::predict(x, newdata = newdata, se.fit = TRUE, interval = ci_type, level = ci, ...)
  fit <- as.data.frame(rez$fit)

  out <- fit$fit
  attr(out, "SE") <- rez$se.fit
  attr(out, "ci") <- ci
  attr(out, "CI_low") <- fit$lwr
  attr(out, "CI_high") <- fit$upr
  class(out) <- c("get_predicted", class(out))
  out
}

#' @export
get_predicted.glm <- function(x, newdata = NULL, ci = 0.95, ci_type = "confidence", transform = "response", ...) {
  rez <- stats::predict(x, newdata = newdata, se.fit = TRUE, type = "link", level = ci, ...)

  out <- rez$fit

  # Confidence CI (see https://fromthebottomoftheheap.net/2018/12/10/confidence-intervals-for-glms/)
  if (x$family$family %in% c("binomial", "poisson")) {
    crit_val <- stats::qnorm(p = 1 - ((1 - ci) / 2))
  } else {
    crit_val <- stats::qt(p = 1 - ((1 - ci) / 2), df = get_df(x, type = "residual"))
  }
  ci_low <- out + (crit_val * rez$se.fit)
  ci_high <- out - (crit_val * rez$se.fit)

  # Prediction CI
  # Seems to be debated: see https://stat.ethz.ch/pipermail/r-help/2003-May/033165.html
  # "Prediction intervals (i.e. intervals with 95% probability of catching a new observation) are somewhat tricky even to define for glms"
  # Essentially, the prediction interval for binomial is [0, 1], which is not really useful
  # But then see https://cran.r-project.org/web/packages/trending/vignettes/prediction_intervals.html


  # Transform
  if (transform == "response" || transform == TRUE) {
    out <- x$family$linkinv(out)
    ci_low <- x$family$linkinv(ci_low)
    ci_high <- x$family$linkinv(ci_high)
  }

  attr(out, "SE") <- rez$se.fit
  attr(out, "ci") <- ci
  attr(out, "CI_low") <- ci_low
  attr(out, "CI_high") <- ci_high
  class(out) <- c("get_predicted", class(out))
  out
}



#' @export
get_predicted.merMod <- function(x, newdata = NULL, ci = 0.95, ci_type = "confidence", transform = "response", re.form = NULL, ...) {

  # Get prediction of point-estimate
  transform <- ifelse(transform == TRUE, "response", ifelse(transform == FALSE, "link", transform))
  out <- stats::predict(x, newdata = newdata, re.form = re.form, type = transform, ...)
  ci_low <- ci_high <- se <- rep(NA, length(out))


  # CI
  if (!is.null(ci)) {

    # Using merTools
    # See https://cran.r-project.org/web/packages/merTools/vignettes/Using_predictInterval.html for a comparison of methods
    if (!requireNamespace("merTools", quietly = TRUE)) {
      stop("This function needs `merTools` to be installed. Please install it by running `install.packages('merTools')`.")
    }

    if (transform == "response" && !insight::model_info(x)$is_linear) {
      type <- "probability"
    } else {
      type <- "linear.prediction"
    }

    if(is.null(newdata)) newdata <- get_data(x)

    pred <- merTools::predictInterval(x, newdata = newdata, level = ci, stat = "median", type = type, include.resid.var = TRUE, ...)
    ci_low <- pred$lwr
    ci_high <- pred$upr
    se <- rep(NA, length(out))

    # Using emmeans
    # refgrid <- emmeans::ref_grid(x, at = as.list(newdata), data = newdata)
    # prediction <- as.data.frame(predict(refgrid, transform = transform, ci = ci, interval = ci_type))
    # prediction[names(newdata)] <- NULL
    # prediction$Predicted <- prediction[, 1]
    # prediction$CI_low <- prediction[, grepl("lower.|LCL", names(prediction))]
    # prediction$CI_high <- prediction[, grepl("upper.|UCL", names(prediction))]
    # prediction[!names(prediction) %in% c("Predicted", "CI_low", "CI_high")] <- NULL

  }

  attr(out, "SE") <- se
  attr(out, "ci") <- ci
  attr(out, "CI_low") <- ci_low
  attr(out, "CI_high") <- ci_high
  class(out) <- c("get_predicted", class(out))
  out
}



# See:
# predict.lm
# predict.glm
# lme4::predict.merMod
# rstanarm::posterior_epred(), rstanarm::posterior_linpred(), rstanarm::posterior_predict(), rstanarm::posterior_interval

# Also, https://github.com/jthaman/ciTools will be of help here





# Methods -----------------------------------------------------------------

#' @export
print.get_predicted <- function(x, ...) {
  print(as.numeric(x))
}

#' @export
as.data.frame.get_predicted <- function(x, ...) {
  out <- data.frame("Predicted" = as.numeric(x))
  if (all(c("SE") %in% names(attributes(x)))) {
    out$SE <- attributes(x)$SE
  }
  if (all(c("CI_low", "CI_high") %in% names(attributes(x)))) {
    out$CI <- attributes(x)$ci
    out$CI_low <- attributes(x)$CI_low
    out$CI_high <- attributes(x)$CI_high
  }
  out
}
