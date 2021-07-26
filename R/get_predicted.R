#' Model Predictions (robust)
#'
#' The `get_predicted()` function is a robust, flexible and user-friendly alternative to base R [predict()] function. Additional features and advantages include availability of uncertainty intervals (CI), bootstrapping, a more intuitive API and the support of more models than base R's `predict` function. However, although the interface are simplified, it is still very important to read the documentation of the arguments. This is because making "predictions" (a lose term for a variety of things) is a non-trivial process, with lots of caveats and complications. Read the `Details` section for more information.
#'
#' @param x A statistical model (can also be a data.frame, in which case the
#'   second argument has to be a model).
#' @param data An optional data frame in which to look for variables with which
#'   to predict. If omitted, the data used to fit the model is used.
#' @param predict Can be `"link"`, `"expectation"` (default), `"prediction"`, or `"response"`. You can see these 4 options for predictions as on a gradient from "close to the model" to "close to the response data". More specifically, the `predict` argument modulates two things; the scale of the output as well as the type of certainty interval (see the details and examples). More specifically, `"link"` returns predictions on the model's link-scale (for logistic models, that means the log-odds scale) with a confidence interval (CI). `"expectation"` (default) also returns confidence intervals, but this time the output is on the response scale (for logistic models, that means probabilities). `"predict"` also gives an output on the response scale, but this time associated with a prediction interval (PI), which is larger than a confidence interval (though it mostly make sense for linear models). Finally, `"response"` only differs from the previous option for binomial models where it additionally transforms the predictions into the original response's type (for instance, to a factor). Read more about in the **Details** section below.
#' @param iterations For Bayesian models, this corresponds to the number of
#'   posterior draws. If `NULL`, will return all the draws (one for each
#'   iteration of the model). For frequentist models, if not `NULL`, will
#'   generate bootstrapped draws, from which bootstrapped CIs will be computed.
#'   Iterations can be accessed via `as.data.frame`.
#' @param include_random If `TRUE` (default), include all random effects in
#'   the prediction. If `FALSE`, don't take them into account. Can also be
#'   a formula to specify which random effects to condition on when predicting
#'   (passed to the `re.form` argument). If `include_random = TRUE`
#'   and `newdata` is provided, make sure to include the random effect
#'   variables in `newdata` as well.
#' @param include_smooth For General Additive Models (GAMs). If `FALSE`,
#'   will fix the value of the smooth to its average, so that the predictions
#'   are not depending on it. (default), `mean()`, or
#'   `bayestestR::map_estimate()`.
#' @param ... Other argument to be passed for instance to
#'   [get_predicted_ci()].
#' @inheritParams get_df
#'
#' @seealso [get_predicted_ci()]
#'
#' @return The fitted values (i.e. predictions for the response). For Bayesian
#'   or bootstrapped models (when `iterations != NULL`), iterations (as
#'   columns and observations are rows) can be accessed via `as.data.frame`.
#'
#' @details
#' In `insight::get_predicted()`, the `predict` argument jointly
#' modulates two separate concepts, the **scale** and the **uncertainty interval**.
#'
#' \subsection{Confidence Interval (CI) vs. Prediction Interval (PI))}{
#' \itemize{
#'   \item **Linear models** - `lm()`: For linear models, Prediction
#'   intervals (`predict = "prediction"`) show the range that likely
#'   contains the value of a new observation (in what range it is likely to
#'   fall), whereas confidence intervals (`predict = "expectation"` or
#'   `predict = "link"`) reflect the uncertainty around the estimated
#'   parameters (and gives the range of uncertainty of the regression line). In
#'   general, Prediction Intervals (PIs) account for both the uncertainty in the
#'   model's parameters, plus the random variation of the individual values.
#'   Thus, prediction intervals are always wider than confidence intervals.
#'   Moreover, prediction intervals will not necessarily become narrower as the
#'   sample size increases (as they do not reflect only the quality of the fit,
#'   but also the variability within the data).
#'   \item **General Linear models** - `glm()`: For binomial models,
#'   prediction intervals are somewhat useless (for instance, for a binomial
#'   (bernoulli) model for which the dependent variable is a vector of 1s and
#'   0s, the prediction interval is... `[0, 1]`).
#' }}
#'
#'
#' \subsection{Link scale vs. Response scale}{
#' Having the output is on the scale of the response variable is arguably the
#' most convenient to understand and visualize the relationships. If on the
#' link-scale, no transformation is applied and the values are on the scale of
#' the model. For instance, for a logistic model, the response
#' scale corresponds to the predicted probabilities, whereas the link-scale
#' makes predictions of log-odds (probabilities on the logit scale). Note that,
#' when `predict = "response"`, the probabilities are rounded (so that the
#' prediction corresponds to the most likely outcome).
#' }
#'
#' @examples
#' data(mtcars)
#' x <- lm(mpg ~ cyl + hp, data = mtcars)
#'
#' predictions <- get_predicted(x)
#' predictions
#'
#' # Options and methods ---------------------
#' get_predicted(x, predict = "prediction")
#'
#' # Get CI
#' as.data.frame(predictions)
#'
#' # Bootstrapped
#' as.data.frame(get_predicted(x, iterations = 4))
#' summary(get_predicted(x, iterations = 4)) # Same as as.data.frame(..., keep_iterations = F)
#'
#' # Different predicttion types ------------------------
#' data(iris)
#' data <- droplevels(iris[1:100, ])
#'
#' # Fit a logistic model
#' x <- glm(Species ~ Sepal.Length, data = data, family = "binomial")
#'
#' # Expectation (default): response scale + CI
#' pred <- get_predicted(x, predict = "expectation")
#' head(as.data.frame(pred))
#'
#' # Prediction: response scale + PI
#' pred <- get_predicted(x, predict = "prediction")
#' head(as.data.frame(pred))
#'
#' # Link: link scale + CI
#' pred <- get_predicted(x, predict = "link")
#' head(as.data.frame(pred))
#'
#' # Response: response "type" + PI
#' pred <- get_predicted(x, predict = "response")
#' head(as.data.frame(pred))
#' @export
get_predicted <- function(x, ...) {
  UseMethod("get_predicted")
}


# default methods ---------------------------

#' @export
get_predicted.default <- function(x, data = NULL, verbose = TRUE, ...) {
  out <- tryCatch(
    {
      if (!is.null(data)) {
        stats::predict(x, newdata = data, ...)
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
get_predicted.data.frame <- function(x, data = NULL, verbose = TRUE, ...) {
  # This makes it pipe friendly; data %>% get_predicted(model)
  if (is.null(data)) {
    stop("Please provide a model to base the estimations on.")
  } else {
    get_predicted(data, x, verbose = verbose, ...)
  }
}



# LM and GLM --------------------------------------------------------------
# =========================================================================


#' @rdname get_predicted
#' @export
get_predicted.lm <- function(x,
                             data = NULL,
                             predict = "expectation",
                             iterations = NULL,
                             verbose = TRUE,
                             ...) {
  args <- .get_predicted_args(x, data = data, predict = predict, verbose = verbose, ...)

  predict_function <- function(x, data, ...) {
    stats::predict(x, newdata = data, interval = "none", type = args$type, ...)
  }

  if (is.null(iterations)) {
    predictions <- predict_function(x, data = args$data)
  } else {
    predictions <- .get_predicted_boot(
      x,
      data = args$data,
      predict_function = predict_function,
      iterations = iterations,
      verbose = verbose,
      ...
    )
  }

  ci_data <- get_predicted_ci(x,
    predictions,
    data = args$data,
    ci_type = args$ci_type,
    ...
  )

  out <- .get_predicted_transform(x, predictions, args, ci_data)

  .get_predicted_out(out$predictions, args = args, ci_data = out$ci_data)
}

#' @export
get_predicted.glm <- get_predicted.lm



# Mixed Models (lme4, glmmTMB) ------------------------------------------
# =======================================================================

#' @export
get_predicted.lmerMod <- function(x,
                                  data = NULL,
                                  predict = "expectation",
                                  ci = 0.95,
                                  include_random = TRUE,
                                  iterations = NULL,
                                  verbose = TRUE,
                                  ...) {

  # Sanitize input
  args <- .get_predicted_args(
    x,
    data = data,
    predict = predict,
    ci = ci,
    include_random = include_random,
    verbose = verbose,
    ...
  )

  # Make prediction only using random if only random
  if (all(names(args$data) %in% find_random(x, flatten = TRUE))) {
    random.only <- TRUE
  } else {
    random.only <- FALSE
  }

  # Prediction function
  predict_function <- function(x, ...) {
    stats::predict(
      x,
      newdata = args$data,
      type = args$type,
      re.form = args$re.form,
      random.only = random.only,
      ...
    )
  }

  if (is.null(iterations)) {
    predictions <- predict_function(x)
  } else {
    predictions <- .get_predicted_boot(
      x,
      data = args$data,
      predict_function = predict_function,
      iterations = iterations,
      verbose = verbose,
      ...
    )
  }

  ci_data <- get_predicted_ci(x, predictions, data = args$data, ci = ci, ci_type = args$ci_type, ...)
  out <- .get_predicted_transform(x, predictions, args, ci_data)
  .get_predicted_out(out$predictions, args = args, ci_data = out$ci_data)
}

#' @export
get_predicted.merMod <- get_predicted.lmerMod



#' @export
get_predicted.glmmTMB <- function(x,
                                  data = NULL,
                                  predict = c("expectation", "link", "prediction", "response", "relation"),
                                  ci = 0.95,
                                  include_random = TRUE,
                                  iterations = NULL,
                                  verbose = TRUE,
                                  ...) {
  predict <- match.arg(predict)

  # Sanity checks
  if (predict == "prediction") {
    if (verbose) {
      warning(
        format_message(
          "`predict = 'prediction'` is currently not available for glmmTMB models.",
          "Changing to `predict = 'expectation'`."
        )
      )
    }
    predict <- "expectation"
  }
  # TODO: prediction intervals
  # https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#predictions-andor-confidence-or-prediction-intervals-on-predictions

  # Sanitize input
  args <- .get_predicted_args(
    x,
    data = data,
    predict = predict,
    ci = ci,
    include_random = include_random,
    verbose = verbose,
    ...
  )

  # Prediction function
  predict_function <- function(x, data, ...) {
    stats::predict(
      x,
      newdata = data,
      type = args$type,
      re.form = args$re.form,
      unconditional = FALSE,
      ...
    )
  }

  # Get prediction
  rez <- predict_function(x, data = args$data, se.fit = TRUE)

  if (is.null(iterations)) {
    predictions <- as.numeric(rez$fit)
  } else {
    predictions <- .get_predicted_boot(
      x,
      data = args$data,
      predict_function = predict_function,
      iterations = iterations,
      verbose = verbose,
      ...
    )
  }

  # Get CI
  ci_data <- .get_predicted_se_to_ci(x, predictions = predictions, se = rez$se.fit, ci = ci)
  out <- .get_predicted_transform(x, predictions, args, ci_data)
  .get_predicted_out(out$predictions, args = args, ci_data = out$ci_data)
}



# GAM -------------------------------------------------------------------
# =======================================================================

#' @export
get_predicted.gam <- function(x,
                              data = NULL,
                              predict = c("expectation", "link", "prediction", "response", "relation"),
                              ci = 0.95,
                              include_random = TRUE,
                              include_smooth = TRUE,
                              iterations = NULL,
                              verbose = TRUE,
                              ...) {
  predict <- match.arg(predict)

  # Sanity checks
  if (predict == "prediction") {
    if (verbose) {
      warning(
        format_message(
          "`predict = 'prediction'` is currently not available for GAM models.",
          "Changing to `predict = 'expectation'`."
        )
      )
    }
    predict <- "expectation"
  }
  # TODO: check this for prediction intervals:
  # https://fromthebottomoftheheap.net/2016/12/15/simultaneous-interval-revisited/
  # https://github.com/gavinsimpson/gratia/blob/master/R/confint-methods.R
  # https://github.com/gavinsimpson/gratia/blob/master/R/posterior-samples.R

  # Sanitize input
  args <- .get_predicted_args(
    x,
    data = data,
    predict = predict,
    ci = ci,
    include_random = include_random,
    include_smooth = include_smooth,
    verbose = verbose,
    ...
  )

  if (inherits(x, c("gamm", "list"))) x <- x$gam


  # Prediction function
  predict_function <- function(x, data, ...) {
    stats::predict(
      x,
      newdata = data,
      type = args$type,
      re.form = args$re.form,
      unconditional = FALSE,
      ...
    )
  }

  # Get prediction
  rez <- predict_function(x, data = args$data, se.fit = TRUE)
  if (is.null(iterations)) {
    predictions <- rez$fit
  } else {
    predictions <- .get_predicted_boot(
      x,
      data = args$data,
      predict_function = predict_function,
      iterations = iterations,
      verbose = verbose,
      ...
    )
  }

  # Get CI
  ci_data <- .get_predicted_se_to_ci(x, predictions = predictions, se = rez$se.fit, ci = ci)
  out <- .get_predicted_transform(x, predictions, args, ci_data)
  .get_predicted_out(out$predictions, args = args, ci_data = out$ci_data)
}

#' @export
get_predicted.gamm <- get_predicted.gam

#' @export
get_predicted.list <- get_predicted.gam # gamm4



# Bayesian --------------------------------------------------------------
# =======================================================================


#' @rdname get_predicted
#' @export
get_predicted.stanreg <- function(x,
                                  data = NULL,
                                  predict = "expectation",
                                  iterations = NULL,
                                  include_random = TRUE,
                                  include_smooth = TRUE,
                                  verbose = TRUE,
                                  ...) {
  if (!requireNamespace("rstantools", quietly = TRUE) || utils::packageVersion("rstantools") < "2.1.0") {
    stop(
      format_message(
        "Package `rstantools` in version 2.1.0 or higher needed for this function to work.",
        "Please install it."
      )
    )
  }

  args <- .get_predicted_args(
    x,
    data = data,
    predict = predict,
    include_random = include_random,
    include_smooth = include_smooth,
    verbose = verbose,
    ...
  )


  # Get draws
  if (args$predict %in% c("link")) {
    draws <- rstantools::posterior_linpred(
      x,
      newdata = args$data,
      re.form = args$re.form,
      nsamples = iterations,
      draws = iterations,
      ...
    )
  } else if (args$predict %in% c("expectation")) {
    draws <- rstantools::posterior_epred(
      x,
      newdata = args$data,
      re.form = args$re.form,
      nsamples = iterations,
      draws = iterations,
      ...
    )
  } else {
    draws <- rstantools::posterior_predict(
      x,
      newdata = args$data,
      re.form = args$re.form,
      draws = iterations,
      nsamples = iterations,
      ...
    )
  }
  draws <- as.data.frame(t(draws))
  names(draws) <- gsub("^V(\\d+)$", "iter_\\1", names(draws))

  # Get predictions (summarize)
  predictions <- .get_predicted_centrality_from_draws(x, draws, ...)

  # Output
  ci_data <- get_predicted_ci(
    x,
    predictions = predictions,
    data = args$data,
    ci_type = args$ci_type,
    ...
  )

  .get_predicted_out(predictions, args = args, ci_data = ci_data)
}


#' @export
get_predicted.brmsfit <- get_predicted.stanreg




# Other models ----------------------------------------------------------
# =======================================================================


#' @export
get_predicted.crr <- function(x, verbose = TRUE, ...) {
  out <- as.data.frame(unclass(stats::predict(x, ...)))
  class(out) <- c("get_predicted", class(out))
  out
}



# FA / PCA -------------------------------------------------------------
# ======================================================================


#' @export
get_predicted.principal <- function(x, data = NULL, ...) {
  if (is.null(data)) {
    out <- as.data.frame(x$scores)
  } else {
    out <- as.data.frame(stats::predict(x, data, ...))
  }
  class(out) <- c("get_predicted", class(out))
  out
}

#' @export
get_predicted.fa <- get_predicted.principal

#' @export
get_predicted.prcomp <- function(x, data = NULL, ...) {
  if (is.null(data)) {
    out <- as.data.frame(x$x)
  } else {
    out <- as.data.frame(stats::predict(x, data, ...))
  }
  class(out) <- c("get_predicted", class(out))
  out
}

#' @export
get_predicted.faMain <- function(x, data = NULL, ...) {
  check_if_installed("fungible")

  if (is.null(data)) {
    stop("A dataframe (either the original of a new one) must be provided (`get_predicted(fa_results, data = df`).")
  } else {
    out <- as.data.frame(fungible::faScores(X = data, faMainObject = x)$fscores)
  }
  class(out) <- c("get_predicted", class(out))
  out
}


# ====================================================================
# Utils --------------------------------------------------------------
# ====================================================================


.format_reform <- function(include_random = TRUE) {

  # Format re.form
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


# -------------------------------------------------------------------------


.get_predicted_args <- function(x,
                                data = NULL,
                                predict = c("expectation", "link", "prediction", "response", "response", "relation"),
                                include_random = TRUE,
                                include_smooth = TRUE,
                                ci = 0.95,
                                newdata = NULL,
                                verbose = TRUE,
                                ...) {

  # Sanitize input
  predict <- match.arg(predict, choices = c("expectation", "link", "prediction", "response", "relation"))
  # Other names: "response", "expected", "distribution", "observations"
  if (predict == "relation") {
    message(format_message(
      '`predict = "relation"` is deprecated.',
      'Use `predict = "expectation"` instead.'
    ))
    predict <- "expectation"
  }

  # Get info
  info <- model_info(x)

  # Data
  if (!is.null(newdata) && is.null(data)) data <- newdata
  if (is.null(data)) data <- get_data(x, verbose = verbose)

  # CI
  if (is.null(ci)) ci <- 0

  # Prediction and CI type
  if (predict == "link") {
    ci_type <- "confidence"
    scale <- "link"
  } else if (predict == "expectation") {
    ci_type <- "confidence"
    scale <- "response"
  } else if (predict %in% c("prediction", "response")) {
    ci_type <- "prediction"
    scale <- "response"
  }

  # Type (that's for the initial call to stats::predict)
  if (info$is_linear) {
    type <- "response"
  } else {
    type <- "link"
  }

  # Transform
  if (info$is_linear == FALSE && scale == "response") {
    transform <- TRUE
  } else {
    transform <- FALSE
  }

  # Smooth
  smooths <- clean_names(find_smooth(x, flatten = TRUE))
  if (!is.null(smooths)) {
    for (smooth in smooths) {
      # Fix smooth to average value
      if (!smooth %in% names(data) || include_smooth == FALSE) {
        include_smooth <- FALSE
        data[[smooth]] <- mean(get_data(x)[[smooth]], na.rm = TRUE)
      }
    }
  }

  # Random
  # In case include_random is TRUE, but there's actually no random factors in data
  if (include_random && !is.null(data) && !is.null(x) && !all(find_random(x, flatten = TRUE) %in% names(data))) {
    include_random <- FALSE
  }

  # Add (or set) random variables to "NA"
  if (include_random == FALSE) {
    if (inherits(x, c("stanreg", "brmsfit"))) {
      # rstantools predictions doens't allow for NaNs in newdata
      data[find_variables(x, effects = "random", verbose = FALSE)$random] <- NULL
    } else {
      data[find_variables(x, effects = "random", verbose = FALSE)$random] <- NA
    }
  }

  re.form <- .format_reform(include_random)

  # Return all args
  list(
    data = data,
    include_random = include_random,
    re.form = re.form,
    include_smooth = include_smooth,
    ci_type = ci_type,
    ci = ci,
    type = type,
    predict = predict,
    scale = scale,
    transform = transform,
    info = info
  )
}


# -------------------------------------------------------------------------
.get_predict_transform_response <- function(predictions, response) {
  predictions <- round(predictions)
  if (is.factor(response)) {
    predictions[predictions == 0] <- levels(response)[1]
    predictions[predictions == 1] <- levels(response)[2]
    predictions <- as.factor(predictions)
    levels(predictions) <- levels(response)
  } else {
    predictions[predictions == 0] <- unique(response)[1]
    predictions[predictions == 1] <- unique(response)[2]
  }
  predictions
}

.get_predicted_transform <- function(x,
                                     predictions,
                                     args = NULL,
                                     ci_data = NULL,
                                     ...) {

  # Transform to response scale
  if (args$transform == TRUE) {
    if (!is.null(ci_data)) {
      # Transform CI
      se_col <- names(ci_data) == "SE"
      ci_data[!se_col] <- lapply(ci_data[!se_col], link_inverse(x))

      # Transform SE (https://github.com/SurajGupta/r-source/blob/master/src/library/stats/R/predict.glm.R#L60)
      # Delta method; SE * deriv( inverse_link(x) wrt lin_pred(x) )
      mu_eta <- abs(get_family(x)$mu.eta(predictions))
      ci_data[se_col] <- ci_data[se_col] * mu_eta
    }

    # Transform predictions
    predictions <- link_inverse(x)(predictions)

    # Transform iterations
    if ("iterations" %in% names(attributes(predictions))) {
      attr(predictions, "iterations") <- as.data.frame(sapply(attributes(predictions)$iterations, link_inverse(x)))
    }
  }

  # Transform to response "type"
  if (args$predict == "response" && model_info(x)$is_binomial) {
    response <- get_response(x)
    ci_data[!se_col] <- lapply(ci_data[!se_col], .get_predict_transform_response, response = response)
    predictions <- .get_predict_transform_response(predictions, response = response)
    if ("iterations" %in% names(attributes(predictions))) {
      attr(predictions, "iterations") <- as.data.frame(sapply(attributes(predictions)$iterations, .get_predict_transform_response, response = response))
    }
  }

  list(predictions = predictions, ci_data = ci_data)
}



# -------------------------------------------------------------------------


.get_predicted_out <- function(predictions, args = NULL, ci_data = NULL, ...) {
  if (!is.null(ci_data)) {
    attr(predictions, "ci_data") <- ci_data
  }
  if (!is.null(args)) {
    attr(predictions, "data") <- args$data
    attr(predictions, "ci") <- args$ci
    attr(predictions, "predict") <- args$predict
  }

  class(predictions) <- c("get_predicted", class(predictions))
  predictions
}

# Bootstrap ==============================================================

.get_predicted_boot <- function(x,
                                data = NULL,
                                predict_function = NULL,
                                iterations = 500,
                                verbose = TRUE,
                                ...) {
  if (is.null(data)) data <- get_data(x, verbose = verbose)

  # TODO: how to make it work with the seed argument??

  # Using bootMer
  if (inherits(x, "merMod")) {
    # installed
    check_if_installed("lme4")

    draws <- lme4::bootMer(x, predict_function, nsim = iterations, use.u = TRUE, ...)

    # Using boot
  } else {
    check_if_installed("boot")

    boot_fun <- function(data, indices, predict_data, ...) {
      model <- stats::update(x, data = data[indices, , drop = FALSE])
      if (verbose) {
        predict_function(model, data = predict_data, ...)
      } else {
        suppressWarnings(predict_function(model, data = predict_data, ...))
      }
    }
    draws <- boot::boot(data = get_data(x), boot_fun, R = iterations, predict_data = data, ...)
  }

  # Format draws
  draws <- as.data.frame(t(draws$t))
  names(draws) <- paste0("iter_", 1:ncol(draws))

  .get_predicted_centrality_from_draws(x, draws, ...)
}


# -------------------------------------------------------------------------


.get_predicted_centrality_from_draws <- function(x,
                                                 iter,
                                                 centrality_function = base::mean,
                                                 ...) {
  predictions <- apply(iter, 1, centrality_function)
  attr(predictions, "iterations") <- iter
  predictions
}
