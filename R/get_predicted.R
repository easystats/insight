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




# process predict-specific arguments ------------------------------------------

.get_predicted_args <- function(x,
                                data = NULL,
                                predict = "expectation",
                                include_random = TRUE,
                                include_smooth = TRUE,
                                ci = 0.95,
                                newdata = NULL,
                                verbose = TRUE,
                                ...) {

  # check whether user possibly used the "type" instead of "predict" argument
  dots <- list(...)

  # one of "type" or "predict" must be provided...
  if (is.null(dots$type) && is.null(predict)) {
    stop(format_message("Please supply a value for the `predict` argument."))
  }

  # ...but not both
  if (!is.null(dots$type) && !is.null(predict)) {
    stop(format_message(
      '`predict` and `type` cannot both be given. The preferred argument for `get_predicted()` is `predict`.',
      'To use the `type` argument, set `predict = NULL` explicitly, e.g.,:',
      '`get_predicted(model, predict = NULL, type = "response")`'
    ))
  }

  # copy "type" to "predict"
  if (!is.null(dots$type)) {
    predict <- dots$type
  }

  if (length(predict) > 1) {
    predict <- predict[1]
    if (isTRUE(verbose)) {
      msg <- format_message(sprintf("More than one option provided in `predict`. Using first option '%s' now."), predict[1])
      warning(msg, call. = FALSE)
    }
  }

  # Get info
  info <- model_info(x, verbose = FALSE)

  # Data
  if (!is.null(newdata) && is.null(data)) data <- newdata
  if (is.null(data)) data <- get_data(x, verbose = verbose)

  # CI
  if (is.null(ci)) ci <- 0

  # check `predict` user-input
  predict_method <- lapply(
    class(x), function(i) {
      tryCatch(utils::getS3method("predict", i),
               error = function(e) NULL)
    }
  )
  predict_method <- tryCatch(predict_method[!sapply(predict_method, is.null)][[1]], error = function(e) NULL)

  # check aliases
  if (predict %in% c("expected", "response")) {
    predict <- "expectation"
  }
  if (predict == "predicted") {
    predict <- "prediction"
  }

  # backward compatibility
  if (identical(predict, "relation")) {
    message(format_message(
      '`predict = "relation" is deprecated.',
      'Please use `predict = "expectation" instead.'
    ))
    predict <- "expectation"
  }

  # Warn if get_predicted() is not called with an easystats- or
  # model-supported predicted type
  easystats_methods <- c("expectation", "link", "prediction", "classification")
  type_methods <- suppressWarnings(eval(formals(predict_method)$type))
  supported <- c(easystats_methods, type_methods)
  if (isTRUE(verbose) && !is.null(predict) && !predict %in% supported) {
    msg <- format_message(
      sprintf('`predict` = "%s"` is not officially supported by `get_predicted()`.', predict),
      '`predict` will be passed directly to the `predict()` method for the model and not validated.',
      'Please check the validity and scale of the results.',
      'Set `verbose = FALSE` to silence this warning, or use one of the supported values for the `predict` argument:',
      paste(" ", paste(sprintf('"%s"', setdiff(easystats_methods, c("expected", "predicted"))), collapse = ", "))
    )
    warning(msg, call. = FALSE)
  }

  # Arbitrate conflicts between the `predict` and `type` from the ellipsis. We
  # create a new variable called `predict_arg` to resolve conflicts. This avoids
  # modifying the values of `type` and `predict` on the fly, which allows us to
  # keep track of the original user input.

  predict_arg <- predict
  transform <- FALSE

  # Type (that's for the initial call to stats::predict)
  if (predict_arg == "terms") {
    type_arg <- "terms"
  } else if (info$is_linear) {
    type_arg <- "response"
    # response always on link-scale - for later back-transformation
  } else if (predict_arg %in% c("expectation", "response")) {
    type_arg <- "link"
    # user provided a valid "type" value, which is not one of our "predict" values
  } else if (predict_arg %in% type_methods) {
    type_arg <- predict_arg
  } else {
    type_arg <- "link"
  }

  # Prediction and CI type
  if (predict_arg == "link") {
    ci_type <- "confidence"
    scale <- "link"
  } else if (predict_arg %in% c("expectation", "response")) {
    ci_type <- "confidence"
    scale <- "response"
    transform <- TRUE
  } else if (predict_arg %in% c("prediction", "classification")) {
    ci_type <- "prediction"
    scale <- "response"
    transform <- TRUE
  } else {
    ## TODO need to check for exceptions
    ci_type <- "confidence"
    scale <- predict_arg
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
    type = type_arg,
    predict = predict_arg,
    scale = scale,
    transform = transform,
    info = info,
    allow_new_levels = isTRUE(list(...)$allow.new.levels)
  )
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
                                     ...) {

  # Transform to response scale
  if (isTRUE(args$transform)) {
    if (!is.null(ci_data)) {
      # Transform CI
      se_col <- names(ci_data) == "SE"

      # fix for R 3.4
      row.names(ci_data) <- NULL

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
