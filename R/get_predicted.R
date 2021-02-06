#' Predicted values
#'
#' Returns values predicted by a model (i.e., fitted values). The Confidence/Credible
#' Intervals (CI) are stored as an attribute, which one can easily extract with
#' \code{as.data.frame()} (see examples below).
#'
#' @param ... Not used.
#' @param ci_type Can be \code{"prediction"} or \code{"confidence"}. Prediction intervals show the range that likely contains the value of a new observation (in what range it would fall), whereas confidence intervals reflect the uncertainty around the estimated parameters (and gives the range of the link; for instance of the regression line in a linear regressions). Prediction intervals account for both the uncertainty in the model's parameters, plus the random variation of the individual values. Thus, prediction intervals are always wider than confidence intervals. Moreover, prediction intervals will not necessarily become narrower as the sample size increases (as they do not reflect only the quality of the fit). This applies mostly for "simple" linear models (like \code{lm}), as for other models (e.g., \code{glm}), prediction intervals are somewhat useless (for instance, for a binomial model for which the dependent variable is a vector of 1s and 0s, the prediction interval is... \code{[0, 1]}).
#' @param ci The interval level (default \code{0.95}, i.e., 95\% CI).
#' @param transform Either \code{"response"} (default) or \code{"link"}. If \code{"link"}, no transformation is applied and the values are on the scale of the linear predictors. If \code{"response"}, the output is on the scale of the response variable. Thus for a default binomial model, \code{"response"} gives the predicted probabilities, and \code{"link"} makes predictions of log-odds (probabilities on logit scale).
#' @param include_random If \code{TRUE} (default), include all random effects in the prediction. If \code{FALSE}, don't take them into account. Can also be a formula to specify which random effects to condition on when predicting (passed to the \code{re.form} argument). If \code{include_random = TRUE} and \code{newdata} is provided, make sure to include the random effect variables in \code{newdata} as well.
#' @param bootstrap Should confidence intervals (CIs) be computed via bootstrapping rather than analytically. If \code{TRUE}, you can specify the number of iterations by modifying the argument \code{iter = 500} (default).
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
get_predicted <- function(x, ...) {
  UseMethod("get_predicted")
}


#' @rdname get_predicted
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



# LM, GLMs ----------------------------------------------------------------


#' @rdname get_predicted
#' @importFrom stats predict qnorm qt
#' @export
get_predicted.lm <- function(x, newdata = NULL, ci = 0.95, ci_type = "confidence", ...) {

  # So that predict doesn't fail
  if (is.null(ci)) {
    level <- 0
  } else {
    level <- ci
  }

  rez <- stats::predict(x, newdata = newdata, se.fit = TRUE, interval = ci_type, level = level, ...)
  fit <- as.data.frame(rez$fit)


  out <- fit$fit
  attr(out, "SE") <- rez$se.fit
  attr(out, "ci") <- ci
  attr(out, "CI_low") <- fit$lwr
  attr(out, "CI_high") <- fit$upr
  class(out) <- c("get_predicted", class(out))
  out
}



#' @rdname get_predicted
#' @importFrom stats family
#' @export
get_predicted.glm <- function(x, newdata = NULL, ci = 0.95, transform = "response", ...) {
  rez <- stats::predict(x, newdata = newdata, se.fit = TRUE, type = "link", level = ci, ...)

  out <- rez$fit

  # Confidence CI (see https://fromthebottomoftheheap.net/2018/12/10/confidence-intervals-for-glms/)
  ci_vals <- .get_predicted_se_to_ci(x, predicted = out, se = rez$se.fit, ci = ci, family = stats::family(x)$family)
  ci_low <- ci_vals$ci_low
  ci_high <- ci_vals$ci_high

  # Prediction CI
  # Seems to be debated: see https://stat.ethz.ch/pipermail/r-help/2003-May/033165.html
  # "Prediction intervals (i.e. intervals with 95% probability of catching a new observation) are somewhat tricky even to define for glms"
  # Essentially, the prediction interval for binomial is [0, 1], which is not really useful
  # But then see https://cran.r-project.org/web/packages/trending/vignettes/prediction_intervals.html


  # Transform
  if (transform == "response" || transform == TRUE) {
    transform_function <- link_inverse(x)
    out <- transform_function(out)
    ci_low <- transform_function(ci_low)
    ci_high <- transform_function(ci_high)
  }

  attr(out, "SE") <- rez$se.fit
  attr(out, "ci") <- ci
  attr(out, "CI_low") <- ci_low
  attr(out, "CI_high") <- ci_high
  class(out) <- c("get_predicted", class(out))
  out
}



# lme4, glmmTMB -----------------------------------------------------------



#' @rdname get_predicted
#' @importFrom stats predict terms model.matrix family
#' @export
get_predicted.merMod <- function(x, newdata = NULL, ci = 0.95, ci_type = "confidence", transform = "response", include_random = TRUE, bootstrap = FALSE, ...) {

  # Get original data
  if (is.null(newdata)) newdata <- get_data(x)

  # In case include_random is TRUE, but there's actually no random factors in newdata
  if (include_random && !all(find_random(x, flatten = TRUE) %in% names(newdata))) {
    include_random <- FALSE
  }

  # Make prediction only using random if only random
  if (all(names(newdata) %in% find_random(x, flatten = TRUE))) {
    random.only <- TRUE
  } else {
    random.only <- FALSE
  }

  # Get prediction of point-estimate
  transform <- ifelse(transform == TRUE, "response", ifelse(transform == FALSE, "link", transform))
  predicted <- stats::predict(x, newdata = newdata, re.form = .format_reform(include_random), type = transform, allow.new.levels = TRUE, random.only = random.only)

  # CI
  if (!is.null(ci)) {
    if (bootstrap == FALSE) {
      ci_vals <- .get_predicted_ci_merMod_analytic(x, predicted, newdata, ci, ci_type)
    } else {
      ci_vals <- .get_predicted_ci_merMod_bootmer(x, newdata, ci, ci_type, include_random, ...)
    }
    attr(predicted, "CI_low") <- ci_vals$ci_low
    attr(predicted, "CI_high") <- ci_vals$ci_high
    attr(predicted, "SE") <- ci_vals$se
  }

  attr(predicted, "ci") <- ci
  class(predicted) <- c("get_predicted", class(predicted))
  predicted
}


.get_predicted_ci_merMod_analytic <- function(x, predicted, newdata, ci = 0.95, ci_type = "confidence") {
  # Matrix-multiply X by the parameter vector B to get the predictions, then
  # extract the variance-covariance matrix V of the parameters and compute XVX'
  # to get the variance-covariance matrix of the predictions. The square-root of
  # the diagonal of this matrix represent the standard errors of the predictions,
  # which are then multiplied by 1.96 for the confidence intervals.

  resp <- find_response(x)
  # fake response
  if (!all(resp %in% newdata)) {
    newdata[resp] <- 0
  }

  mm <- stats::model.matrix(stats::terms(x), newdata)
  var_matrix <- mm %*% get_varcov(x) %*% t(mm)

  if (ci_type == "prediction") {
    se <- sqrt(diag(var_matrix) + get_variance_residual(x))
  } else {
    se <- sqrt(diag(var_matrix))
  }

  .get_predicted_se_to_ci(x, predicted = predicted, se = se, ci = ci, family = stats::family(x)$family)
}


#' @importFrom stats quantile
.get_predicted_ci_merMod_bootmer <- function(x, newdata, ci = 0.95, ci_type = NULL, include_random = TRUE, iter = 500, ...) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("Package `lme4` needed for this function to work. Please install it.")
  }
  merBoot <- lme4::bootMer(x, predict, re.form = .format_reform(include_random), use.u = TRUE, nsim = iter, ...)

  ci_vals <- apply(merBoot$t, 2, function(x) as.numeric(stats::quantile(x, probs = c(1 - ci, 1 + ci) / 2, na.rm = TRUE)))

  list(ci_low = ci_vals[1, ],
       ci_high = ci_vals[2, ],
       se = NA)
}




#' @export
get_predicted.glmmTMB <- function(x, newdata = NULL, ci = 0.95, transform = "response", include_random = TRUE, ...) {

  # Get original data
  if (is.null(newdata)) newdata <- get_data(x)

  # In case include_random is TRUE, but there's actually no random factors in newdata
  if (include_random && !all(find_random(x, flatten = TRUE) %in% names(newdata))) {
    include_random <- FALSE
  }

  # TODO: is this needed?
  if (include_random == FALSE) {
    newdata[find_variables(x, effects = "random")$random] <- NA
  }

  # Get prediction
  rez <- as.data.frame(stats::predict(x, newdata = newdata, re.form = .format_reform(include_random), type = transform, se.fit = TRUE))
  out <- rez$fit

  # CI
  ci_vals <- .get_predicted_se_to_ci(x, predicted = out, se = rez$se.fit, ci = ci, family = stats::family(x)$family)
  ci_low <- ci_vals$ci_low
  ci_high <- ci_vals$ci_high

  # TODO: check if we need linkinverse
  if (transform != "zprob" && transform != "disp") {
    linkinverse <- link_inverse(x)
  }

  attr(out, "SE") <- rez$se.fit
  attr(out, "ci") <- ci
  attr(out, "CI_low") <- ci_low
  attr(out, "CI_high") <- ci_high
  class(out) <- c("get_predicted", class(out))
  out
}



# GAMs --------------------------------------------------------------------


#' @export
get_predicted.gam <- function(x, newdata = NULL, ci = 0.95, transform = "response", include_random = TRUE, ...) {

  # Sanitize input
  if (is.null(newdata)) newdata <- get_data(x)
  if (inherits(x, c("gamm", "list"))) x <- x$gam

  # Get prediction
  rez <- as.data.frame(stats::predict(x, newdata = newdata, re.form = .format_reform(include_random), type = transform, se.fit = TRUE))
  out <- rez$fit


  # CI
  ci_vals <- .get_predicted_se_to_ci(x, predicted = out, se = rez$se.fit, ci = ci, family = x$family$family)
  ci_low <- ci_vals$ci_low
  ci_high <- ci_vals$ci_high

  attr(out, "SE") <- rez$se.fit
  attr(out, "ci") <- ci
  attr(out, "CI_low") <- ci_low
  attr(out, "CI_high") <- ci_high
  class(out) <- c("get_predicted", class(out))
  out
}

#' @export
get_predicted.gamm <- get_predicted.gam

#' @export
get_predicted.list <- get_predicted.gam # gamm4



# Bayesian ----------------------------------------------------------------


#' @export
get_predicted.stanreg <- function(x, newdata = NULL, ci = 0.95, ci_type = "confidence", transform = "response", include_random = TRUE, ...) {

  # See:
  # rstanarm::posterior_epred(), rstanarm::posterior_linpred(), rstanarm::posterior_predict(), rstanarm::posterior_interval

  if (is.null(ci)) ci <- 0 # So that predict doesn't fail

  if (!requireNamespace("rstanarm", quietly = TRUE)) {
    stop("Package `rstanarm` needed for this function to work. Please install it.")
  }

  transform <- ifelse(transform == "response", TRUE, ifelse(transform == "link", FALSE, transform))


  if (ci_type == "confidence") {
    if (transform == TRUE) {
      out <- rstanarm::posterior_epred(x, newdata = newdata, re.form = .format_reform(include_random), ...)
    } else {
      out <- rstanarm::posterior_linpred(x, newdata = newdata, transform = FALSE, re.form = .format_reform(include_random), ...)
    }
  } else {
    out <- rstanarm::posterior_predict(x, newdata = newdata, transform = transform, re.form = .format_reform(include_random), ...)
  }
  out <- t(out)

  attr(out, "SE") <- NA
  attr(out, "ci") <- ci
  attr(out, "CI_low") <- NA
  attr(out, "CI_high") <- NA
  class(out) <- c("get_predicted", class(out))
  out
}



# Also, https://github.com/jthaman/ciTools will be of help here

# Other ----------------------------------------------------------------

#' @export
get_predicted.crr <- function(x, ...) {
  out <- as.data.frame(unclass(stats::predict(x, ...)))
  class(out) <- c("get_predicted", class(out))
  out
}



# Methods -----------------------------------------------------------------

#' @importFrom utils head
#' @export
print.get_predicted <- function(x, ...) {
  insight::print_colour("Predicted values:\n\n", "blue")
  if (inherits(x, "matrix")) {
    print(head(as.matrix(x)))
  } else {
    print(as.numeric(x))
  }
  insight::print_colour("\nNOTE: Confidence intervals, if available, are stored as attributes and can be acccessed using `as.data.frame()` on this output.", "yellow")
}


#' @export
as.data.frame.get_predicted <- function(x, ...) {

  # In the case of multiple draws (i.e., matrix)
  if (inherits(x, "matrix") || (!is.null(ncol(x)) && !is.na(ncol(x)) && ncol(x) > 1)) {
    out <- as.data.frame.matrix(x)

    # If no names (i.e., V1, V2, V3 etc., replace by iter_)
    names(out) <- gsub("^V(\\d)$", "iter_\\1", names(out))

    return(out)
  }

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

#' @export
as.matrix.get_predicted <- function(x, ...) {
  class(x) <- class(x)[class(x) != "get_predicted"]
  as.matrix(x)
}



# Helpers -----------------------------------------------------------------

#' @importFrom stats qnorm qt
.format_reform <- function(include_random = TRUE) {
  if (is.null(include_random) || is.na(include_random)) {
    re.form <- include_random
  } else if (include_random == TRUE) {
    re.form <- NULL
  } else if (include_random == FALSE) {
    re.form <- NA
  } else {
    re.form <- include_random
  }
  re.form
}

#' @importFrom stats qnorm qt
.get_predicted_se_to_ci <- function(x, predicted, se = NULL, ci = 0.95, family = "gaussian") {
  if (is.null(ci)) {
    return(list(ci_low = predicted, ci_high = predicted))
  } # Same as predicted

  # Return NA
  if (is.null(se)) {
    ci_low <- ci_high <- rep(NA, length(predicted))

    # Get CI
  } else {
    if (family %in% c("gaussian", "binomial", "poisson")) {
      crit_val <- stats::qnorm(p = (1 + ci) / 2)
    } else {
      crit_val <- stats::qt(p = (1 + ci) / 2, df = get_df(x, type = "residual"))
    }

    ci_low <- predicted - (se * crit_val)
    ci_high <- predicted + (se * crit_val)
  }

  list(ci_low = ci_low, ci_high = ci_high, se = se)
}
