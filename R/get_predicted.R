#' @title Model predictions (robust) and their confidence intervals
#' @name get_predicted
#'
#' @description
#' The `get_predicted()` function is a robust, flexible and user-friendly
#' alternative to base R [predict()] function. Additional features and
#' advantages include availability of uncertainty intervals (CI), bootstrapping,
#' a more intuitive API and the support of more models than base R's `predict()`
#' function. However, although the interface are simplified, it is still very
#' important to read the documentation of the arguments. This is because making
#' "predictions" (a lose term for a variety of things) is a non-trivial process,
#' with lots of caveats and complications. Read the 'Details' section for more
#' information.
#' \cr \cr
#' `get_predicted_ci()` returns the confidence (or prediction) interval (CI)
#' associated with predictions made by a model. This function can be called
#' separately on a vector of predicted values. `get_predicted()` usually
#' returns confidence intervals (included as attribute, and accessible via the
#' `as.data.frame()` method) by default.
#'
#' @param x A statistical model (can also be a data.frame, in which case the
#'   second argument has to be a model).
#' @param data An optional data frame in which to look for variables with which
#'   to predict. If omitted, the data used to fit the model is used. Visualization matrices can be generated using [get_datagrid()].
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
#'   Iterations can be accessed by running `as.data.frame(..., keep_iterations = TRUE)`
#'   on the output.
#' @param include_random If `"default"`, include all random effects in the
#'   prediction, unless random effect variables are not in the data.  If `TRUE`,
#'   include all random effects in the prediction (in this case, it will be
#'   checked if actually all random effect variables are in `data`). If `FALSE`,
#'   don't take them into account. Can also be a formula to specify which random
#'   effects to condition on when predicting (passed to the `re.form` argument).
#'   If `include_random = TRUE` and `data` is provided, make sure to include
#'   the random effect variables in `data` as well.
#' @param include_smooth For General Additive Models (GAMs). If `FALSE`,
#'   will fix the value of the smooth to its average, so that the predictions
#'   are not depending on it. (default), `mean()`, or
#'   `bayestestR::map_estimate()`.
#' @param ci The interval level (default `0.95`, i.e., `95%` CI).
#' @param ci_type Can be `"prediction"` or `"confidence"`. Prediction
#'   intervals show the range that likely contains the value of a new
#'   observation (in what range it would fall), whereas confidence intervals
#'   reflect the uncertainty around the estimated parameters (and gives the
#'   range of the link; for instance of the regression line in a linear
#'   regressions). Prediction intervals account for both the uncertainty in the
#'   model's parameters, plus the random variation of the individual values.
#'   Thus, prediction intervals are always wider than confidence intervals.
#'   Moreover, prediction intervals will not necessarily become narrower as the
#'   sample size increases (as they do not reflect only the quality of the fit).
#'   This applies mostly for "simple" linear models (like `lm`), as for
#'   other models (e.g., `glm`), prediction intervals are somewhat useless
#'   (for instance, for a binomial model for which the dependent variable is a
#'   vector of 1s and 0s, the prediction interval is... `[0, 1]`).
#' @param ci_method The method for computing p values and confidence intervals. Possible values depend on model type.
#'   + `NULL` uses the default method, which varies based on the model type.
#'   + Most frequentist models: "gaussian" (default).
#'   + Bayesian models: "quantile" (default), "hdi", "eti".
#'   + Mixed effects `lme4` models: "gaussian" (default), "satterthwaite", "kenward-roger".
#' @param dispersion_method Bootstrap dispersion and Bayesian posterior summary: "sd" or "mad".
#' @param ... Other argument to be passed, for instance to `get_predicted_ci()`.
#' @inheritParams get_varcov
#' @inheritParams get_df
#'
#' @return The fitted values (i.e. predictions for the response). For Bayesian
#'   or bootstrapped models (when `iterations != NULL`), iterations (as
#'   columns and observations are rows) can be accessed via `as.data.frame()`.
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
#' The arguments `vcov` and `vcov_args` can be used to calculate robust
#' standard errors for confidence intervals of predictions. These arguments,
#' when provided in `get_predicted()`, are passed down to `get_predicted_ci()`,
#' thus, see the related documentation there for more
#' details.
#' }
#'
#' \subsection{Bayesian and Bootstrapped models and iterations}{
#' For predictions based on multiple iterations, for instance in the case of Bayesian models and bootstrapped predictions, the function used to compute the centrality (point-estimate predictions) can be modified via the `centrality_function` argument. For instance, `get_predicted(model, centrality_function = stats::median)`. The default is `mean`.
#' Individual draws can be accessed by running `iter <- as.data.frame(get_predicted(model))`, and their iterations can be reshaped into a long format by `bayestestR::reshape_iterations(iter)`.
#' }
#'
#' @seealso [get_datagrid()]
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
#' if (require("boot")) {
#'   # Bootstrapped
#'   as.data.frame(get_predicted(x, iterations = 4))
#'   # Same as as.data.frame(..., keep_iterations = FALSE)
#'   summary(get_predicted(x, iterations = 4))
#' }
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
#'
#' @export
get_predicted <- function(x, ...) {
  UseMethod("get_predicted")
}


# default methods ---------------------------

#' @rdname get_predicted
#' @export
get_predicted.default <- function(x,
                                  data = NULL,
                                  predict = "expectation",
                                  ci = 0.95,
                                  ci_type = "confidence",
                                  ci_method = NULL,
                                  dispersion_method = "sd",
                                  vcov = NULL,
                                  vcov_args = NULL,
                                  verbose = TRUE,
                                  ...) {

  # evaluate arguments
  args <- .get_predicted_args(x, data = data, predict = predict, verbose = verbose, ...)

  # evaluate dots, remove some arguments that might be duplicated else
  dot_args <- list(...)
  dot_args[["newdata"]] <- NULL
  dot_args[["type"]] <- NULL


  # 1. step: predictions
  predict_args <- compact_list(list(x, newdata = args$data, type = args$type, dot_args))
  predictions <- tryCatch(do.call("predict", predict_args), error = function(e) NULL)

  # may fail due to invalid "dot_args", so try shorter argument list
  if (is.null(predictions)) {
    predictions <- tryCatch(do.call("predict", compact_list(list(x, newdata = args$data, type = args$type))), error = function(e) NULL)
  }

  # still fails? try fitted()
  if (is.null(predictions)) {
    predictions <- tryCatch(do.call("fitted", predict_args), error = function(e) NULL)
  }

  # 2. step: confidence intervals
  ci_data <- tryCatch(
    {
      get_predicted_ci(
        x,
        predictions,
        data = args$data,
        ci_type = args$ci_type,
        ci_method = ci_method,
        vcov = vcov,
        vcov_args = vcov_args,
        ...
      )
    },
    error = function(e) {
      NULL
    }
  )

  # 3. step: back-transform
  if (!is.null(predictions)) {
    out <- .get_predicted_transform(x, predictions, args, ci_data, verbose = verbose)
  } else {
    out <- NULL
  }

  # 4. step: final preparation
  if (!is.null(out)) {
    out <- .get_predicted_out(out$predictions, args = args, ci_data = out$ci_data)
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
  out <- .get_predicted_transform(x, predictions, args, ci_data, verbose = verbose)

  # 4. step: final preparation
  .get_predicted_out(out$predictions, args = args, ci_data = out$ci_data)
}

#' @rdname get_predicted
#' @export
get_predicted.glm <- get_predicted.lm




# rms -------------------------------------------------------------------
# =======================================================================

# the rms::lrm function produces an object of class c("lrm", "rms", glm"). The
# `get_predicted.glm` function breaks when trying to calculate standard errors,
# so we use the default method.

#' @export
get_predicted.lrm <- get_predicted.default




# survival: survreg -----------------------------------------------------
# =======================================================================

#' @rdname get_predicted
#' @export
get_predicted.survreg <- get_predicted.lm




# survival: coxph -------------------------------------------------------
# =======================================================================

#' @rdname get_predicted
#' @export
get_predicted.coxph <- function(x, data = NULL, predict = "expectation", iterations = NULL, verbose = TRUE, ...) {
  args <- .get_predicted_args(x, data = data, predict = predict, verbose = verbose, ...)
  se <- NULL

  predict_function <- function(x, data, ...) {
    stats::predict(x, newdata = data, type = args$type, ...)
  }

  # 1. step: predictions
  if (is.null(iterations)) {
    predictions <- predict_function(x, data = args$data, se.fit = TRUE)
    if (is.list(predictions)) {
      se <- as.vector(predictions$se.fit)
      predictions <- as.vector(predictions$fit)
    }
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
    se = se,
    ...
  )

  # 3. step: back-transform
  out <- .get_predicted_transform(x, predictions, args, ci_data, link_inv = exp, verbose = verbose)

  # 4. step: final preparation
  .get_predicted_out(out$predictions, args = args, ci_data = out$ci_data)
}




# bife ------------------------------------------------------------------
# =======================================================================

#' @rdname get_predicted
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




# afex ------------------------------------------------------------------
# =======================================================================

#' @rdname get_predicted
#' @export
get_predicted.afex_aov <- function(x, data = NULL, ...) {
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




# ====================================================================
# Utils --------------------------------------------------------------
# ====================================================================


.format_reform <- function(include_random = TRUE) {

  # Format re.form
  if (is.null(include_random) || is.na(include_random)) {
    re.form <- include_random
  } else if (isTRUE(include_random)) {
    re.form <- NULL
  } else if (isFALSE(include_random)) {
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
                                     verbose = FALSE,
                                     ...) {

  # Transform to response scale
  if (isTRUE(args$transform)) {

    # retrieve link-inverse, for back transformation...
    if (is.null(link_inv)) {
      link_inv <- link_inverse(x)
    }

    if (!is.null(ci_data)) {
      # Transform CI
      se_col <- names(ci_data) == "SE"

      # fix for R 3.4
      row.names(ci_data) <- NULL

      ci_data[!se_col] <- lapply(ci_data[!se_col], link_inv)

      # Transform SE (https://github.com/SurajGupta/r-source/blob/master/src/library/stats/R/predict.glm.R#L60)
      # Delta method; SE * deriv( inverse_link(x) wrt lin_pred(x) )
      mu_eta <- tryCatch(abs(get_family(x)$mu.eta(predictions)), error = function(e) NULL)
      if (is.null(mu_eta)) {
        ci_data[se_col] <- NULL
        if (isTRUE(verbose)) {
          warning(format_message(
            'Could not apply Delta method to transform standard errors.',
            'You may be able to obtain standard errors by using the ',
            '`predict="link"` argument value.'
          ), call. = FALSE)
        }
      } else {
        ci_data[se_col] <- ci_data[se_col] * mu_eta
      }
    }

    # Transform predictions
    predictions <- link_inv(predictions)

    # Transform iterations
    if ("iterations" %in% names(attributes(predictions))) {
      attr(predictions, "iterations") <- as.data.frame(sapply(attributes(predictions)$iterations, link_inv))
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
    vary <- colnames(predictions)
    predictions$Row <- 1:nrow(predictions)
    # if we have any focal predictors, add those as well, so we have
    # the associated levels/values for "Row"
    if (!is.null(args$data)) {
      focal_predictors <- tryCatch(names(which(n_unique(args$data) > 1)),
        error = function(e) NULL
      )
      if (!is.null(focal_predictors)) {
        predictions <- cbind(predictions, args$data[focal_predictors])
      }
    }
    predictions <- stats::reshape(predictions,
      direction = "long",
      varying = vary,
      times = vary,
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
    # outcome with a single level
  } else {
    # .get_predicted_boot already gives us the correct observation ~ draws format
    if (is.null(colnames(iter)) || !all(grepl("^iter", colnames(iter)))) {
      iter <- as.data.frame(t(iter))
    }
    predictions <- apply(iter, 1, centrality_function)
  }
  # Rename iterations
  names(iter) <- paste0("iter_", 1:ncol(iter))
  # Store as attribute
  attr(predictions, "iterations") <- iter
  predictions
}
