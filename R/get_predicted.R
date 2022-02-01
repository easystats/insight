#' Model Predictions (robust)
#'
#' The `get_predicted()` function is a robust, flexible and user-friendly alternative to base R [predict()] function. Additional features and advantages include availability of uncertainty intervals (CI), bootstrapping, a more intuitive API and the support of more models than base R's `predict` function. However, although the interface are simplified, it is still very important to read the documentation of the arguments. This is because making "predictions" (a lose term for a variety of things) is a non-trivial process, with lots of caveats and complications. Read the `Details` section for more information.
#'
#' @param x A statistical model (can also be a data.frame, in which case the
#'   second argument has to be a model).
#' @param data An optional data frame in which to look for variables with which
#'   to predict. If omitted, the data used to fit the model is used.
#' @param predict string or `NULL`
#' * `"link"` returns predictions on the model's link-scale (for logistic models, that means the log-odds scale) with a confidence interval (CI).
#' * `"expectation"` (default) also returns confidence intervals, but this time the output is on the response scale (for logistic models, that means probabilities).
#' * `"prediction"` also gives an output on the response scale, but this time associated with a prediction interval (PI), which is larger than a confidence interval (though it mostly make sense for linear models).
#' * `"classification"` only differs from `"prediction"` for binomial models where it additionally transforms the predictions into the original response's type (for instance, to a factor).
#' * Other strings are passed directly to the `type` argument of the `predict()` method supplied by the modelling package.
#' * When `predict = NULL`, alternative arguments such as `type` will be captured by the `...` ellipsis and passed directly to the `predict()` method supplied by the modelling package. Note that this might result in conflicts with multiple matching `type` arguments - thus, the recommendation is to use the `predict` argument for those values.
#' * Notes: You can see the 4 options for predictions as on a gradient from "close to the model" to "close to the response data": "link", "expectation", "prediction", "classification". The `predict` argument modulates two things: the scale of the output and the type of certainty interval. Read more about in the **Details** section below.
#' @param iterations For Bayesian models, this corresponds to the number of
#'   posterior draws. If `NULL`, will return all the draws (one for each
#'   iteration of the model). For frequentist models, if not `NULL`, will
#'   generate bootstrapped draws, from which bootstrapped CIs will be computed.
#'   Iterations can be accessed by running `as.data.frame()` on the output.
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
#' @param ... Other argument to be passed, for instance to [get_predicted_ci()].
#'   This can be used to request confidence intervals based on robust standard
#'   errors, e.g. by specifying the `vcov_*` arguments from [get_predicted_ci()]
#'   directly in the call to `get_predicted()`.
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
#'   intervals (`predict="prediction"`) show the range that likely
#'   contains the value of a new observation (in what range it is likely to
#'   fall), whereas confidence intervals (`predict="expectation"` or
#'   `predict="link"`) reflect the uncertainty around the estimated
#'   parameters (and gives the range of uncertainty of the regression line). In
#'   general, Prediction Intervals (PIs) account for both the uncertainty in the
#'   model's parameters, plus the random variation of the individual values.
#'   Thus, prediction intervals are always wider than confidence intervals.
#'   Moreover, prediction intervals will not necessarily become narrower as the
#'   sample size increases (as they do not reflect only the quality of the fit,
#'   but also the variability within the data).
#'   \item **Generalized Linear models** - `glm()`: For binomial models,
#'   prediction intervals are somewhat useless (for instance, for a binomial
#'   (Bernoulli) model for which the dependent variable is a vector of 1s and
#'   0s, the prediction interval is... `[0, 1]`).
#' }}
#'
#' \subsection{Link scale vs. Response scale}{
#' When users set the `predict` argument to `"expectation"`, the predictions
#' are returned on the response scale, which is arguably the most convenient
#' way to understand and visualize relationships of interest. When users set
#' the `predict` argument to `"link"`, predictions are returned on the link
#' scale, and no transformation is applied. For instance, for a logistic
#' regression model, the response scale corresponds to the predicted
#' probabilities, whereas the link-scale makes predictions of log-odds
#' (probabilities on the logit scale). Note that when users select
#' `predict="classification"` in binomial models, the `get_predicted()`
#' function will first calculate predictions as if the user had selected
#' `predict="expectation"`. Then, it will round the responses in order to
#' return the most likely outcome.
#' }
#'
#' \subsection{Heteroscedasticity consistent standard errors}{
#' The arguments `vcov_estimation`, `vcov_type` and `vcov_args` can be used
#' to calculate robust standard errors for confidence intervals of predictions.
#' These arguments, when provided in `get_predicted()`, are passed down to
#' [get_predicted_ci()], thus, see the related documentation there for more
#' details.
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
#' # Different prediction types ------------------------
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
#' # Classification: classification "type" + PI
#' pred <- get_predicted(x, predict = "classification")
#' head(as.data.frame(pred))
#' @export
get_predicted <- function(x, ...) {
  UseMethod("get_predicted")
}


# default methods ---------------------------

#' @export
get_predicted.default <- function(x, data = NULL, verbose = TRUE, ...) {

  # many predict.CLASS methods do not work when `newdata` is explicitly specified, even if it is NULL
  if (is.null(data)) {
    args <- c(list(x), list(...))
  } else {
    args <- c(list(x, "newdata" = data), list(...))
  }

  out <- tryCatch(do.call("predict", args), error = function(e) NULL)

  if (is.null(out)) {
    out <- tryCatch(do.call("fitted", args), error = function(e) NULL)
  }

  if (!is.null(out)) {
    out <- .get_predicted_out(out, args = list("data" = data))
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
    stats::predict(x, newdata = data, interval = "none", type = args$type, se.fit = FALSE, ...)
  }

  # 1. step: predictions
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

  # 2. step: confidence intervals
  ci_data <- get_predicted_ci(
    x,
    predictions,
    data = args$data,
    ci_type = args$ci_type,
    ...
  )

  # 3. step: back-transform
  out <- .get_predicted_transform(x, predictions, args, ci_data)

  # 4. step: final preparation
  .get_predicted_out(out$predictions, args = args, ci_data = out$ci_data)
}

#' @export
get_predicted.glm <- get_predicted.lm




# rms -------------------------------------------------------------------
# =======================================================================

# the rms::lrm function produces an object of class c("lrm", "rms", glm"). The
# `get_predicted.glm` function breaks when trying to calculate standard errors,
# so we use the default method.

#' @export
get_predicted.lrm <- get_predicted.default




# bife ------------------------------------------------------------------
# =======================================================================
#' @export
get_predicted.bife <- function(x,
                               predict = "expectation",
                               data = NULL,
                               verbose = TRUE,
                               ...) {
  args <- .get_predicted_args(x,
    data = data,
    predict = predict,
    verbose = TRUE,
    ...
  )

  out <- tryCatch(stats::predict(x, type = args$scale, X_new = args$data), error = function(e) NULL)

  if (!is.null(out)) {
    out <- .get_predicted_out(out, args = list("data" = data))
  }

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




# back-transformation ------------------------------------------------------

.get_predict_transform_response <- function(predictions, response) {
  predictions <- round(predictions)
  if (is.factor(response)) {
    predictions[predictions == 0] <- levels(response)[1]
    predictions[predictions == 1] <- levels(response)[2]
    predictions <- as.factor(predictions)
    levels(predictions) <- levels(response)
  } else {
    resp <- unique(response)
    predictions <- resp[match(predictions, resp)]
  }
  predictions
}


.get_predicted_transform <- function(x,
                                     predictions,
                                     args = NULL,
                                     ci_data = NULL,
                                     link_inv = NULL,
                                     ...) {

  # Transform to response scale
  if (isTRUE(args$transform)) {

    # retrieve link-inverse, for back transformation...
    if (is.null(link_inv)) {
      link_inv <- link_inverse
    }

    if (!is.null(ci_data)) {
      # Transform CI
      se_col <- names(ci_data) == "SE"

      # fix for R 3.4
      row.names(ci_data) <- NULL

      ci_data[!se_col] <- lapply(ci_data[!se_col], link_inv(x))

      # Transform SE (https://github.com/SurajGupta/r-source/blob/master/src/library/stats/R/predict.glm.R#L60)
      # Delta method; SE * deriv( inverse_link(x) wrt lin_pred(x) )
      mu_eta <- abs(get_family(x)$mu.eta(predictions))
      ci_data[se_col] <- ci_data[se_col] * mu_eta
    }

    # Transform predictions
    predictions <- link_inv(x)(predictions)

    # Transform iterations
    if ("iterations" %in% names(attributes(predictions))) {
      attr(predictions, "iterations") <- as.data.frame(sapply(attributes(predictions)$iterations, link_inv(x)))
    }

    # Transform to response "type"
    if (args$predict == "classification" && model_info(x, verbose = FALSE)$is_binomial) {
      response <- get_response(x)
      ci_data[!se_col] <- lapply(ci_data[!se_col], .get_predict_transform_response, response = response)
      predictions <- .get_predict_transform_response(predictions, response = response)
      if ("iterations" %in% names(attributes(predictions))) {
        attr(predictions, "iterations") <- as.data.frame(sapply(attributes(predictions)$iterations, .get_predict_transform_response, response = response))
      }
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

  # multidimensional or "grouped" predictions (e.g., nnet::multinom with `predict(type="probs")`)
  if (is.matrix(predictions) && ncol(predictions) > 1) {
    predictions <- as.data.frame(predictions)
    predictions$Row <- 1:nrow(predictions)
    predictions <- stats::reshape(predictions,
      direction = "long",
      varying = setdiff(colnames(predictions), "Row"),
      times = setdiff(colnames(predictions), "Row"),
      v.names = "Predicted",
      timevar = "Response",
      idvar = "Row"
    )
    row.names(predictions) <- NULL
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

  # outcome: ordinal/multinomial/multivariate produce a 3D array of predictions,
  # which we stack in "long" format
  if (length(dim(iter)) == 3) {
    # 3rd dimension of the array is the response level. This stacks the draws into:
    # Rows * Response ~ Draws
    iter_stacked <- apply(iter, 1, c)
    predictions <- data.frame(
      # rows repeated for each response level
      Row = rep(1:ncol(iter), times = dim(iter)[3]),
      # response levels repeated for each row
      Response = rep(dimnames(iter)[[3]], each = dim(iter)[2]),
      Predicted = apply(iter_stacked, 1, centrality_function),
      stringsAsFactors = FALSE
    )
    iter <- as.data.frame(iter_stacked)
    names(iter) <- paste0("iter_", names(iter))
    # outcome with a single level
  } else {
    # .get_predicted_boot already gives us the correct observation ~ draws format
    if (is.null(colnames(iter)) || !all(grepl("^iter", colnames(iter)))) {
      iter <- as.data.frame(t(iter))
      names(iter) <- gsub("^V(\\d+)$", "iter_\\1", names(iter))
    }
    predictions <- apply(iter, 1, centrality_function)
  }
  attr(predictions, "iterations") <- iter
  predictions
}
