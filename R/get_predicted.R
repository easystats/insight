#' Predicted values
#'
#' Returns values predicted by a model (i.e., fitted values). The Confidence/Credible
#' Intervals (CI) are stored as an attribute, which one can easily extract with
#' \code{as.data.frame()} (see examples below).
#'
#' @param ... Not used.
#' @param ci_type Can be \code{"prediction"} or \code{"confidence"}. Prediction intervals show the range that likely contains the value of a new observation (in what range it would fall), whereas confidence intervals reflect the uncertainty around the estimated parameters (and gives the range of the link; for instance of the regression line in a linear regressions). Prediction intervals account for both the uncertainty in the model's parameters, plus the random variation of the individual values. Thus, prediction intervals are always wider than confidence intervals. Moreover, prediction intervals will not necessarily become narrower as the sample size increases (as they do not reflect only the quality of the fit). This applies mostly for "simple" linear models (like \code{lm}), as for other models (e.g., \code{glm}), prediction intervals are somewhat useless (for instance, for a binomial model for which the dependent variable is a vector of 1s and 0s, the prediction interval is... \code{[0, 1]}). Importantly for Bayesian models, it is this argument that modulates the type of prediction - i.e., link (using  \code{\link[rstanarm:predictive_interval]{predictive_interval()}}) or predictive (using \code{\link[rstanarm:posterior_predict]{posterior_predict}}).
#' @param ci The interval level (default \code{0.95}, i.e., 95\% CI).
#' @param transform Either \code{"response"} (default) or \code{"link"}. If \code{"link"}, no transformation is applied and the values are on the scale of the linear predictors. If \code{"response"}, the output is on the scale of the response variable. Thus for a default binomial model, \code{"response"} gives the predicted probabilities, and \code{"link"} makes predictions of log-odds (probabilities on logit scale).
#' @param include_random If \code{TRUE} (default), include all random effects in the prediction. If \code{FALSE}, don't take them into account. Can also be a formula to specify which random effects to condition on when predicting (passed to the \code{re.form} argument). If \code{include_random = TRUE} and \code{newdata} is provided, make sure to include the random effect variables in \code{newdata} as well.
#' @param include_smooth For General Additive Models (GAMs). If \code{FALSE}, will fix the value of the smooth to its average, so that the predictions are not depending on it.
#' @param iterations For Bayesian models, it corresponds to the number of posterior draws. If \code{NULL}, will return all the draws (one for each iteration of the model).
#' @param bootstrap Should confidence intervals (CIs) be computed via bootstrapping rather than analytically. If \code{TRUE}, you can specify the number of iterations by modifying the argument \code{iter = 500} (default).
#' @param vcov_estimation String, indicating the suffix of the \code{vcov*()}-function
#'   from the \pkg{sandwich} or \pkg{clubSandwich} package, e.g. \code{vcov_estimation = "CL"}
#'   (which calls \code{\link[sandwich]{vcovCL}} to compute clustered covariance matrix
#'   estimators), or \code{vcov_estimation = "HC"} (which calls
#'   \code{\link[sandwich:vcovHC]{vcovHC()}} to compute heteroskedasticity-consistent
#'   covariance matrix estimators).
#' @param vcov_type Character vector, specifying the estimation type for the
#'   robust covariance matrix estimation (see \code{\link[sandwich:vcovHC]{vcovHC()}}
#'   or \code{clubSandwich::vcovCR()} for details).
#' @param vcov_args List of named vectors, used as additional arguments that
#'   are passed down to the \pkg{sandwich}-function specified in \code{vcov_estimation}.
#' @inheritParams get_residuals
#' @inheritParams stats::predict.lm
#'
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
#' as.data.frame(predicted) # To get everything
#'
#' \donttest{
#' # Bayesian models
#' if (require("rstanarm") && require("bayestestR")) {
#'   model <- stan_glm(mpg ~ am, data = mtcars, refresh = 0)
#'   insight::get_predicted(model, ci_type = "prediction")
#' }}
#' @export
get_predicted <- function(x, ...) {
  UseMethod("get_predicted")
}



# default methods ---------------------------


#' @rdname get_predicted
#' @importFrom stats fitted predict
#' @export
get_predicted.default <- function(x, newdata = NULL, ...) {
  out <- tryCatch(
    {
      if (!is.null(newdata)) {
        stats::predict(x, newdata = newdata, ...)
      } else {
        stats::predict(x, ...)
      }
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




# generic prediction function -------------------------------------


#' @importFrom stats predict
.generic_predictions <- function(x, newdata = NULL, predicted, ci = 0.95, ci_type = "confidence", vcov_estimation = NULL, vcov_type = NULL, vcov_args = NULL, transform = NULL, type, ...) {
  out <- predicted
  # (robust) CI
  if (!is.null(ci)) {
    ci_vals <- .get_predicted_ci_analytic(
      x,
      out,
      newdata,
      ci,
      ci_type,
      vcov_estimation = vcov_estimation,
      vcov_type = vcov_type,
      vcov_args = vcov_args,
      transform = transform
    )
    out <- ci_vals$predicted
    attr(out, "SE") <- ci_vals$se
    attr(out, "CI_low") <- ci_vals$ci_low
    attr(out, "CI_high") <- ci_vals$ci_high
  }

  attr(out, "ci") <- ci
  attr(out, "data") <- newdata
  class(out) <- c("get_predicted", class(out))
  out
}




# LM, GLMs ----------------------------------------------------------------


#' @rdname get_predicted
#' @export
get_predicted.lm <- function(x, newdata = NULL, ci = 0.95, ci_type = "confidence", vcov_estimation = NULL, vcov_type = NULL, vcov_args = NULL, ...) {

  predicted <- stats::predict(x, newdata = newdata, interval = "none", se.fit = FALSE, ...)
  .generic_predictions(
    x = x,
    newdata = newdata,
    predicted = as.vector(predicted),
    ci = ci,
    ci_type = ci_type,
    vcov_estimation = vcov_estimation,
    vcov_type = vcov_type,
    vcov_args = vcov_args,
    transform = NULL,
    type = "response",
    ...
  )
}


#' @export
get_predicted.glm <- function(x, newdata = NULL, ci = 0.95, transform = "response", ci_type = "confidence", vcov_estimation = NULL, vcov_type = NULL, vcov_args = NULL, ...) {

  predicted <- stats::predict(x, newdata = newdata, interval = "none", se.fit = FALSE, transform = "link", ...)
  # Prediction CI
  # Seems to be debated: see https://stat.ethz.ch/pipermail/r-help/2003-May/033165.html
  # "Prediction intervals (i.e. intervals with 95% probability of catching a new observation) are somewhat tricky even to define for glms"
  # Essentially, the prediction interval for binomial is [0, 1], which is not really useful
  # But then see https://cran.r-project.org/web/packages/trending/vignettes/prediction_intervals.html
  .generic_predictions(
    x = x,
    newdata = newdata,
    predicted = as.vector(predicted),
    ci = ci,
    ci_type = ci_type,
    vcov_estimation = vcov_estimation,
    vcov_type = vcov_type,
    vcov_args = vcov_args,
    transform = transform,
    type = "link",
    ...
  )
}



# lme4, glmmTMB -----------------------------------------------------------



#' @rdname get_predicted
#' @importFrom stats predict terms model.matrix family
#' @export
get_predicted.merMod <- function(x, newdata = NULL, ci = 0.95, ci_type = "confidence", transform = "response", include_random = TRUE, bootstrap = FALSE, vcov_estimation = NULL, vcov_type = NULL, vcov_args = NULL, ...) {

  # In case include_random is TRUE, but there's actually no random factors in newdata
  if (!is.null(newdata) && include_random && !all(find_random(x, flatten = TRUE) %in% names(newdata))) {
    include_random <- FALSE
  }

  # Make prediction only using random if only random
  if (!is.null(newdata) && all(names(newdata) %in% find_random(x, flatten = TRUE))) {
    random.only <- TRUE
  } else {
    random.only <- FALSE
  }

  # Get prediction of point-estimate
  predicted <- stats::predict(x, newdata = newdata, re.form = .format_reform(include_random), type = "link", allow.new.levels = TRUE, random.only = random.only)

  # CI
  if (!is.null(ci)) {
    if (bootstrap == FALSE) {
      ci_vals <- .get_predicted_ci_analytic(
        x,
        predicted,
        newdata,
        ci,
        ci_type,
        vcov_estimation = vcov_estimation,
        vcov_type = vcov_type,
        vcov_args = vcov_args,
        transform = transform
      )
      predicted <- ci_vals$predicted
    } else {
      ## TODO transform response
      ci_vals <- .get_predicted_ci_merMod_bootmer(x, newdata, ci, ci_type, include_random, ...)
    }

    attr(predicted, "CI_low") <- ci_vals$ci_low
    attr(predicted, "CI_high") <- ci_vals$ci_high
    attr(predicted, "SE") <- ci_vals$se
  }

  attr(predicted, "ci") <- ci
  attr(predicted, "data") <- newdata
  class(predicted) <- c("get_predicted", class(predicted))
  predicted
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

  # Get data
  newdata <- .get_predicted_newdata(x, newdata = newdata, include_random = include_random)

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
  ci_vals <- .get_predicted_se_to_ci(x, predicted = out, se = rez$se.fit, ci = ci)
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
  attr(out, "data") <- newdata
  class(out) <- c("get_predicted", class(out))
  out
}



# GAMs --------------------------------------------------------------------


#' @export
get_predicted.gam <- function(x, newdata = NULL, ci = 0.95, transform = "response", include_random = TRUE, include_smooth = TRUE, ...) {

  # Get data
  newdata <- .get_predicted_newdata(x, newdata = newdata, include_random = include_random, include_smooth = include_smooth)

  # Sanitize input
  if (inherits(x, c("gamm", "list"))) x <- x$gam

  # Get prediction
  rez <- as.data.frame(stats::predict(x, newdata = newdata, re.form = .format_reform(include_random), type = transform, se.fit = TRUE))
  out <- rez$fit


  # CI
  ci_vals <- .get_predicted_se_to_ci(x, predicted = out, se = rez$se.fit, ci = ci)
  ci_low <- ci_vals$ci_low
  ci_high <- ci_vals$ci_high

  attr(out, "SE") <- rez$se.fit
  attr(out, "ci") <- ci
  attr(out, "CI_low") <- ci_low
  attr(out, "CI_high") <- ci_high
  attr(out, "data") <- newdata
  class(out) <- c("get_predicted", class(out))
  out
}

#' @export
get_predicted.gamm <- get_predicted.gam

#' @export
get_predicted.list <- get_predicted.gam # gamm4



# Bayesian ----------------------------------------------------------------

#' @rdname get_predicted
#' @export
get_predicted.stanreg <- function(x, newdata = NULL, ci = 0.95, ci_type = "confidence", transform = "response", include_random = TRUE, include_smooth = TRUE, iterations = NULL, ...) {

  # See:
  # rstanarm::posterior_epred(), rstanarm::posterior_linpred(), rstanarm::posterior_predict(), rstanarm::posterior_interval

  # Get data
  newdata <- .get_predicted_newdata(x, newdata = newdata, include_random = include_random, include_smooth = include_smooth)

  if (is.null(ci)) ci <- 0 # So that predict doesn't fail

  if (!requireNamespace("rstanarm", quietly = TRUE)) {
    stop("Package `rstanarm` needed for this function to work. Please install it.")
  }

  transform <- ifelse(transform == "response", TRUE, ifelse(transform == "link", FALSE, transform))


  if (ci_type == "confidence") {
    if (transform == TRUE) {
      out <- rstanarm::posterior_epred(x, newdata = newdata, re.form = .format_reform(include_random), draws = iterations, ...)
    } else {
      out <- rstanarm::posterior_linpred(x, newdata = newdata, transform = FALSE, re.form = .format_reform(include_random), draws = iterations, ...)
    }
  } else {
    out <- rstanarm::posterior_predict(x, newdata = newdata, transform = transform, re.form = .format_reform(include_random), draws = iterations, ...)
  }
  out <- as.data.frame(t(out))

  # If no names (i.e., V1, V2, V3 etc., replace by iter_)
  names(out) <- gsub("^V(\\d+)$", "iter_\\1", names(out))

  attr(out, "SE") <- NA
  attr(out, "ci") <- ci
  attr(out, "CI_low") <- NA
  attr(out, "CI_high") <- NA
  attr(out, "data") <- newdata
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
  if (inherits(x, "data.frame") && "iter_1" %in% names(x)) {
    print.data.frame(.print_bigdata(x, ...))
    insight::print_colour("\nNOTE: You can get CIs by running `bayestestR::describe_posterior()` on this output, and reshape it to a long format with `bayestestR::reshape_iterations()`.", "yellow")
  } else {
    print(as.numeric(x))
    insight::print_colour("\nNOTE: Confidence intervals, if available, are stored as attributes and can be acccessed using `as.data.frame()` on this output.", "yellow")
  }
}


#' @export
as.data.frame.get_predicted <- function(x, ...) {

  # In the case it's already a dataframe
  if (inherits(x, "data.frame")) {
    class(x) <- class(x)[class(x) != "get_predicted"]
    return(x)
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


.print_bigdata <- function(x, nrows = 3, ncols = 3, ...) {
  out <- x[1:nrows, 1:ncols]

  # Add row
  row <- out[1, ]
  row[1, ] <- "..."
  out <- rbind(out, row)

  # Add col
  out[[paste0("...x", ncol(x) - ncols)]] <- "..."

  row.names(out)[nrows + 1] <- paste0("...x", nrow(x) - nrows)

  class(out) <- "data.frame"
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


#' @importFrom stats model.matrix terms reformulate
.get_predicted_ci_analytic <- function(x, predicted, newdata, ci = 0.95, ci_type = "confidence", vcov_estimation = NULL, vcov_type = NULL, vcov_args = NULL, transform = NULL) {

  # Matrix-multiply X by the parameter vector B to get the predictions, then
  # extract the variance-covariance matrix V of the parameters and compute XVX'
  # to get the variance-covariance matrix of the predictions. The square-root of
  # the diagonal of this matrix represent the standard errors of the predictions,
  # which are then multiplied by 1.96 for the confidence intervals.


  ci_type <- match.arg(ci_type, c("confidence", "prediction"))
  resp <- find_response(x)
  # fake response
  if (!is.null(newdata) && !all(resp %in% newdata)) {
    newdata[[resp]] <- 0
  }

  # (robust) variance-covariance matrix
  if (!is.null(vcov_estimation)) {
    # check for existing vcov-prefix
    if (!grepl("^vcov", vcov_estimation)) {
      vcov_estimation <- paste0("vcov", vcov_estimation)
    }
    # set default for clubSandwich
    if (vcov_estimation == "vcovCR" && is.null(vcov_type)) {
      vcov_type <- "CR0"
    }
    if (!is.null(vcov_type) && vcov_type %in% c("CR0", "CR1", "CR1p", "CR1S", "CR2", "CR3")) {
      if (!requireNamespace("clubSandwich", quietly = TRUE)) {
        stop("Package `clubSandwich` needed for this function. Please install and try again.")
      }
      robust_package <- "clubSandwich"
      vcov_estimation <- "vcovCR"
    } else {
      if (!requireNamespace("sandwich", quietly = TRUE)) {
        stop("Package `sandwich` needed for this function. Please install and try again.")
      }
      robust_package <- "sandwich"
    }
    # compute robust standard errors based on vcov
    if (robust_package == "sandwich") {
      vcov_estimation <- get(vcov_estimation, asNamespace("sandwich"))
      vcm <- as.matrix(do.call(vcov_estimation, c(list(x = x, type = vcov_type), vcov_args)))
    } else {
      vcov_estimation <- clubSandwich::vcovCR
      vcm <- as.matrix(do.call(vcov_estimation, c(list(obj = x, type = vcov_type), vcov_args)))
    }
  } else {
    # get variance-covariance-matrix, depending on model type
    vcm <- get_varcov(x, component = "conditional")
  }


  if (is.null(newdata)) {
    mm <- stats::model.matrix(x)
  } else {
    # model terms, required for model matrix
    model_terms <- tryCatch({
      stats::terms(x)
    },
    error = function(e) {
      find_formula(x)$conditional
    })

    # drop offset from model_terms
    if (inherits(x, c("zeroinfl", "hurdle", "zerotrunc"))) {
      all_terms <- find_terms(x)$conditional
      off_terms <- grepl("^offset\\((.*)\\)", all_terms)
      if (any(off_terms)) {
        all_terms <- all_terms[!off_terms]
        ## TODO preserve interactions
        vcov_names <- dimnames(vcm)[[1]][grepl(":", dimnames(vcm)[[1]], fixed = TRUE)]
        if (length(vcov_names)) {
          vcov_names <- gsub(":", "*", vcov_names, fixed = TRUE)
          all_terms <- unique(c(all_terms, vcov_names))
        }
        off_terms <- grepl("^offset\\((.*)\\)", all_terms)
        model_terms <- stats::reformulate(all_terms[!off_terms], response = find_response(x))
      }
    }
    mm <- stats::model.matrix(model_terms, newdata)
  }

  # compute vcov for predictions
  var_matrix <- mm %*% vcm %*% t(mm)

  # add sigma to standard errors, i.e. confidence or prediction intervals
  if (ci_type == "prediction") {
    if (is_mixed_model(x)) {
      se <- sqrt(diag(var_matrix) + get_variance_residual(x))
    } else {
      se <- sqrt(diag(var_matrix) + get_sigma(x)^2)
    }
  } else {
    se <- sqrt(diag(var_matrix))
  }

  out <- .get_predicted_se_to_ci(x, predicted = predicted, se = se, ci = ci)

  # Transform
  if (!is.null(transform) && (transform == "response" || transform == TRUE)) {
    transform_function <- link_inverse(x)
    predicted <- transform_function(predicted)
    out$ci_low <- transform_function(out$ci_low)
    out$ci_high <- transform_function(out$ci_high)
  }

  list(predicted = predicted, ci_low = out$ci_low, ci_high = out$ci_high, se = se)
}


#' @importFrom stats qnorm qt
.get_predicted_se_to_ci <- function(x, predicted, se = NULL, ci = 0.95) {
  if (is.null(ci)) {
    return(list(ci_low = predicted, ci_high = predicted))
  } # Same as predicted

  m_info <- model_info(x)
  dof <- get_df(x, type = "residual")

  # Return NA
  if (is.null(se)) {
    ci_low <- ci_high <- rep(NA, length(predicted))

    # Get CI
  } else {
    if (is.null(dof) || is.infinite(dof) || find_statistic(x) == "z-statistic") {
      crit_val <- stats::qnorm(p = (1 + ci) / 2)
    } else {
      crit_val <- stats::qt(p = (1 + ci) / 2, df = dof)
    }

    ci_low <- predicted - (se * crit_val)
    ci_high <- predicted + (se * crit_val)
  }

  list(ci_low = ci_low, ci_high = ci_high, se = se)
}





.get_predicted_newdata <- function(x, newdata = NULL, include_random = TRUE, include_smooth = TRUE, ...) {
  if (is.null(newdata)) newdata <- get_data(x)

  # Smooth
  smooths <- insight::clean_names(find_smooth(x, flatten = TRUE))
  if (!is.null(smooths)) {
    for (smooth in smooths) {
      # Fix smooth to average value
      if (!smooth %in% names(newdata) || include_smooth == FALSE) {
        newdata[[smooth]] <- mean(get_data(x)[[smooth]], na.rm = TRUE)
      }
    }
  }
  newdata
}






.get_predicted_simulate <- function(x, iterations = 500, include_random = NULL, ...) {
  out <- tryCatch(
    {
      stats::simulate(x, nsim = iterations, re.form = .format_reform(include_random), ...)
    },
    error = function(e) { NULL }
  )

  if (is.null(out)) {
    stop(sprintf("Could not simulate responses. Maybe there is no 'simulate()' for objects of class '%s'? You can open an issue.", class(x)[1]), call. = FALSE)
  }

  names(out) <- paste0("iter_", 1:ncol(out))
  out
}


