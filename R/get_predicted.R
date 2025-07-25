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
#'
#' [`get_predicted_ci()`] returns the confidence (or prediction) interval (CI)
#' associated with predictions made by a model. This function can be called
#' separately on a vector of predicted values. `get_predicted()` usually
#' returns confidence intervals (included as attribute, and accessible via the
#' `as.data.frame()` method) by default. It is preferred to rely on the
#' `get_predicted()` function for standard errors and confidence intervals -
#' use `get_predicted_ci()` only if standard errors and confidence intervals
#' are not available otherwise.
#'
#' @param x A statistical model (can also be a data.frame, in which case the
#'   second argument has to be a model).
#' @param data An optional data frame in which to look for variables with which
#'   to predict. If omitted, the data used to fit the model is used. Visualization
#'   matrices can be generated using [get_datagrid()].
#' @param predict string or `NULL`
#' * `"link"` returns predictions on the model's link-scale (for logistic models,
#'   that means the log-odds scale) with a confidence interval (CI). This option
#'   should also be used for finite mixture models (currently only family
#'   [`brms::mixture()`] from package *brms*), when predicted values of the
#'   response for each class is required.
#' * `"expectation"` (default) also returns confidence intervals, but this time
#'   the output is on the response scale (for logistic models, that means
#'   probabilities).
#' * `"prediction"` also gives an output on the response scale, but this time
#'   associated with a prediction interval (PI), which is larger than a confidence
#'   interval (though it mostly make sense for linear models).
#' * `"classification"` is relevant only for binomial, ordinal or mixture models.
#'   - For binomial models, `predict = "classification"` will additionally
#'     transform the predictions into the original response's type (for
#'     instance, to a factor).
#'   - For ordinal models (e.g., classes `clm` or `multinom`), gives the
#'     predicted response class membership, defined as highest probability
#'     prediction.
#'   - For finite mixture models (currently only family [`brms::mixture()`] from
#'     package *brms*) also returns the predicted response class membership
#'     (similar as for ordinal models).
#' * Other strings are passed directly to the `type` argument of the `predict()`
#'   method supplied by the modelling package.
#' * Specifically for models of class `brmsfit` (package *brms*), the `predict`
#'   argument can be any valid option for the `dpar` argument, to predict
#'   distributional parameters (such as `"sigma"`, `"beta"`, `"kappa"`, `"phi"`
#'   and so on, see `?brms::brmsfamily`).
#' * When `predict = NULL`, alternative arguments such as `type` will be captured
#'   by the `...` ellipsis and passed directly to the `predict()` method supplied
#'   by the modelling package. Note that this might result in conflicts with
#'   multiple matching `type` arguments - thus, the recommendation is to use the
#'   `predict` argument for those values.
#' * Notes: You can see the four options for predictions as on a gradient from
#'   "close to the model" to "close to the response data": "link", "expectation",
#'   "prediction", "classification". The `predict` argument modulates two things:
#'   the scale of the output and the type of certainty interval. Read more about
#'   in the **Details** section below.
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
#' @param include_smooth For General Additive Models (GAMs). If `FALSE`, will
#'   fix the value of the smooth to its average, so that the predictions are not
#'   depending on it. (default), `mean()`, or `bayestestR::map_estimate()`.
#' @param ci The interval level. Default is `NULL`, to be fast even for larger
#'   models. Set the interval level to an explicit value, e.g. `0.95`, for `95%`
#'   CI).
#' @param ci_type Can be `"prediction"` or `"confidence"`. Prediction intervals
#'   show the range that likely contains the value of a new observation (in what
#'   range it would fall), whereas confidence intervals reflect the uncertainty
#'   around the estimated parameters (and gives the range of the link; for
#'   instance of the regression line in a linear regressions). Prediction
#'   intervals account for both the uncertainty in the model's parameters, plus
#'   the random variation of the individual values. Thus, prediction intervals
#'   are always wider than confidence intervals. Moreover, prediction intervals
#'   will not necessarily become narrower as the sample size increases (as they
#'   do not reflect only the quality of the fit). This applies mostly for
#'   "simple" linear models (like `lm`), as for other models (e.g., `glm`),
#'   prediction intervals are somewhat useless (for instance, for a binomial
#'   model for which the dependent variable is a vector of 1s and 0s, the
#'   prediction interval is... `[0, 1]`).
#' @param ci_method The method for computing p values and confidence intervals.
#'   Possible values depend on model type.
#'   + `NULL` uses the default method, which varies based on the model type.
#'   + Most frequentist models: `"wald"` (default), `"residual"` or `"normal"`.
#'   + Bayesian models:  `"quantile"`  (default), `"hdi"`, `"eti"`, and `"spi"`.
#'   + Mixed effects **lme4** models: `"wald"` (default), `"residual"`,
#'     `"normal"`, `"satterthwaite"`, and `"kenward-roger"`.
#'
#'   See [`get_df()`] for details.
#' @param dispersion_method Bootstrap dispersion and Bayesian posterior summary:
#'   `"sd"` or `"mad"`.
#' @param ... Other argument to be passed, for instance to the model's `predict()`
#' method, or `get_predicted_ci()`.
#' @inheritParams get_varcov
#' @inheritParams get_df
#'
#' @return The fitted values (i.e. predictions for the response). For Bayesian
#' or bootstrapped models (when `iterations != NULL`), iterations (as columns
#' and observations are rows) can be accessed via `as.data.frame()`.
#'
#' @details
#' In `insight::get_predicted()`, the `predict` argument jointly modulates two
#' separate concepts, the **scale** and the **uncertainty interval**.
#'
#' @section Confidence Interval (CI) vs. Prediction Interval (PI)):
#' - **Linear models** - `lm()`: For linear models, prediction intervals
#'   (`predict="prediction"`) show the range that likely contains the value of a
#'   new observation (in what range it is likely to fall), whereas confidence
#'   intervals (`predict="expectation"` or `predict="link"`) reflect the
#'   uncertainty around the estimated parameters (and gives the range of
#'   uncertainty of the regression line). In general, Prediction Intervals (PIs)
#'   account for both the uncertainty in the model's parameters, plus the random
#'   variation of the individual values. Thus, prediction intervals are always
#'   wider than confidence intervals. Moreover, prediction intervals will not
#'   necessarily become narrower as the sample size increases (as they do not
#'   reflect only the quality of the fit, but also the variability within the
#'   data).
#' - **Generalized Linear models** - `glm()`: For binomial models, prediction
#'   intervals are somewhat useless (for instance, for a binomial (Bernoulli)
#'   model for which the dependent variable is a vector of 1s and 0s, the
#'   prediction interval is... `[0, 1]`).
#'
#' @section Link scale vs. Response scale:
#' When users set the `predict` argument to `"expectation"`, the predictions are
#' returned on the response scale, which is arguably the most convenient way to
#' understand and visualize relationships of interest. When users set the
#' `predict` argument to `"link"`, predictions are returned on the link scale,
#' and no transformation is applied. For instance, for a logistic regression
#' model, the response scale corresponds to the predicted probabilities, whereas
#' the link-scale makes predictions of log-odds (probabilities on the logit
#' scale). Note that when users select `predict = "classification"` in binomial
#' models, the `get_predicted()` function will first calculate predictions as if
#' the user had selected `predict = "expectation"`. Then, it will round the
#' responses in order to return the most likely outcome. For ordinal or mixture
#' models, it returns the predicted class membership, based on the highest
#' probability of classification.
#'
#' @section Heteroscedasticity consistent standard errors:
#' The arguments `vcov` and `vcov_args` can be used to calculate robust standard
#' errors for confidence intervals of predictions. These arguments, when
#' provided in `get_predicted()`, are passed down to `get_predicted_ci()`, thus,
#' see the related documentation there for more details.
#'
#' @section Finite mixture models:
#' For finite mixture models (currently, only the `mixture()` family from package
#' *brms* is supported), use `predict = "classification"` to predict the class
#' membership. To predict outcome values by class, use `predict = "link"`. Other
#' `predict` options will return predicted values of the outcome for the full
#' data, not stratified by class membership.
#'
#' @section Bayesian and Bootstrapped models and iterations:
#' For predictions based on multiple iterations, for instance in the case of
#' Bayesian models and bootstrapped predictions, the function used to compute
#' the centrality (point-estimate predictions) can be modified via the
#' `centrality_function` argument. For instance,
#' `get_predicted(model, centrality_function = stats::median)`. The default is
#' `mean`. Individual draws can be accessed by running
#' `iter <- as.data.frame(get_predicted(model))`, and their iterations can be
#' reshaped into a long format by `bayestestR::reshape_iterations(iter)`.
#'
#' @section Hypothesis tests:
#' There is limited support for hypothesis tests, i.e. objects of class `htest`:
#' - `chisq.test()`: returns the expected values of the contingency table.
#'
#' @seealso [get_datagrid()]
#'
#' @examplesIf require("boot")
#' data(mtcars)
#' x <- lm(mpg ~ cyl + hp, data = mtcars)
#'
#' predictions <- get_predicted(x, ci = 0.95)
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
#' # Same as as.data.frame(..., keep_iterations = FALSE)
#' summary(get_predicted(x, iterations = 4))
#'
#' # Different prediction types ------------------------
#' data(iris)
#' data <- droplevels(iris[1:100, ])
#'
#' # Fit a logistic model
#' x <- glm(Species ~ Sepal.Length, data = data, family = "binomial")
#'
#' # Expectation (default): response scale + CI
#' pred <- get_predicted(x, predict = "expectation", ci = 0.95)
#' head(as.data.frame(pred))
#'
#' # Prediction: response scale + PI
#' pred <- get_predicted(x, predict = "prediction", ci = 0.95)
#' head(as.data.frame(pred))
#'
#' # Link: link scale + CI
#' pred <- get_predicted(x, predict = "link", ci = 0.95)
#' head(as.data.frame(pred))
#'
#' # Classification: classification "type" + PI
#' pred <- get_predicted(x, predict = "classification", ci = 0.95)
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
                                  ci = NULL,
                                  ci_type = "confidence",
                                  ci_method = NULL,
                                  dispersion_method = "sd",
                                  vcov = NULL,
                                  vcov_args = NULL,
                                  verbose = TRUE,
                                  ...) {
  # evaluate arguments
  my_args <- .get_predicted_args(x, data = data, predict = predict, verbose = verbose, ...)

  # evaluate dots, remove some arguments that might be duplicated else
  dot_args <- list(...)
  dot_args[["newdata"]] <- NULL
  dot_args[["type"]] <- NULL


  # 1. step: predictions
  predict_args <- compact_list(list(x, newdata = my_args$data, type = my_args$type, dot_args))
  predictions <- .safe(do.call("predict", predict_args))

  # may fail due to invalid "dot_args", so try shorter argument list
  if (is.null(predictions)) {
    predictions <- .safe(
      do.call("predict", compact_list(list(x, newdata = my_args$data, type = my_args$type)))
    )
  }

  # still fails? try fitted()
  if (is.null(predictions)) {
    predictions <- .safe(do.call("fitted", predict_args))
  }

  # stop here if we have no predictions
  if (is.null(predictions) && isTRUE(verbose)) {
    format_warning(
      paste0("Could not compute predictions for model of class `", class(x)[1], "`.")
    )
  }

  # 2. step: confidence intervals
  ci_data <- .safe({
    get_predicted_ci(
      x,
      predictions,
      data = my_args$data,
      ci_type = my_args$ci_type,
      ci_method = ci_method,
      vcov = vcov,
      vcov_args = vcov_args,
      ...
    )
  })

  # 3. step: back-transform
  if (is.null(predictions)) {
    out <- NULL
  } else {
    out <- .get_predicted_transform(x, predictions, my_args = my_args, ci_data, verbose = verbose, ...)
  }

  # 4. step: final preparation
  if (!is.null(out)) {
    out <- .get_predicted_out(out$predictions, my_args = my_args, ci_data = out$ci_data)
  }

  out
}

#' @export
get_predicted.data.frame <- function(x, data = NULL, verbose = TRUE, ...) {
  # This makes it pipe friendly; data %>% get_predicted(model)
  if (is.null(data)) {
    format_error("Please provide a model to base the estimations on.")
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
                             ci = NULL,
                             iterations = NULL,
                             verbose = TRUE,
                             ...) {
  predict_function <- function(x, data, ...) {
    stats::predict(x,
      newdata = data, interval = "none",
      type = my_args$type, se.fit = FALSE, ...
    )
  }

  my_args <- .get_predicted_args(x, data = data, predict = predict, verbose = verbose, ...)

  # 0. step: convert matrix variable types attributes to numeric, if necessary.
  # see https://github.com/easystats/insight/pull/671
  dataClasses <- attributes(x[["terms"]])$dataClasses
  if ("nmatrix.1" %in% dataClasses) {
    dataClasses[dataClasses == "nmatrix.1"] <- "numeric"
    attributes(x$terms)$dataClasses <- dataClasses
    attributes(attributes(x$model)$terms)$dataClasses <- dataClasses
    my_args$data[] <- lapply(my_args$data, function(x) {
      if (all(class(x) == c("matrix", "array"))) { # nolint
        as.numeric(x)
      } else {
        x
      }
    })
  }

  # 1. step: predictions
  if (is.null(iterations)) {
    predictions <- predict_function(x, data = my_args$data)
  } else {
    predictions <- .get_predicted_boot(
      x,
      data = my_args$data,
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
    data = my_args$data,
    ci = ci,
    ci_type = my_args$ci_type,
    verbose = verbose,
    ...
  )

  # 3. step: back-transform
  out <- .get_predicted_transform(x, predictions, my_args, ci_data, verbose = verbose, ...)

  # 4. step: final preparation
  .get_predicted_out(out$predictions, my_args = my_args, ci_data = out$ci_data)
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


# MASS: rlm -----------------------------------------------------
# =======================================================================

# these objects inherit from `lm`, but `get_predicted.lm` do not return
# confidence intervals.

#' @export
get_predicted.rlm <- get_predicted.default


# survival: survreg -----------------------------------------------------
# =======================================================================

#' @export
get_predicted.survreg <- get_predicted.lm


# survival: coxph -------------------------------------------------------
# =======================================================================

#' @export
get_predicted.coxph <- function(x,
                                data = NULL,
                                predict = "expectation",
                                ci = NULL,
                                iterations = NULL,
                                verbose = TRUE,
                                ...) {
  my_args <- .get_predicted_args(x, data = data, predict = predict, verbose = verbose, ...)
  se <- NULL

  predict_function <- function(x, data, ...) {
    stats::predict(x, newdata = data, type = my_args$type, ...)
  }

  # 1. step: predictions
  if (is.null(iterations)) {
    predictions <- predict_function(x, data = my_args$data, se.fit = TRUE)
    if (is.list(predictions)) {
      se <- as.vector(predictions$se.fit)
      predictions <- as.vector(predictions$fit)
    }
  } else {
    predictions <- .get_predicted_boot(
      x,
      data = my_args$data,
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
    data = my_args$data,
    ci = ci,
    ci_type = my_args$ci_type,
    se = se,
    ...
  )

  # 3. step: back-transform
  out <- .get_predicted_transform(x, predictions, my_args, ci_data, link_inv = exp, verbose = verbose)

  # 4. step: final preparation
  .get_predicted_out(out$predictions, my_args = my_args, ci_data = out$ci_data)
}


# bife ------------------------------------------------------------------
# =======================================================================

#' @export
get_predicted.bife <- function(x,
                               predict = "expectation",
                               data = NULL,
                               verbose = TRUE,
                               ...) {
  my_args <- .get_predicted_args(x,
    data = data,
    predict = predict,
    verbose = TRUE,
    ...
  )

  out <- .safe(stats::predict(x, type = my_args$scale, X_new = my_args$data))

  if (!is.null(out)) {
    out <- .get_predicted_out(out, my_args = list(data = data))
  }

  out
}


# rma -------------------------------------------------------------------
# =======================================================================
#' @export
get_predicted.rma <- function(x,
                              predict = "expectation",
                              data = NULL,
                              ci = NULL,
                              verbose = TRUE,
                              transf = NULL,
                              transf_args = NULL,
                              ...) {
  my_args <- .get_predicted_args(x,
    data = data,
    predict = predict,
    verbose = TRUE,
    ...
  )

  has_scale_model <- inherits(x, "rma.ls")
  # TODO: Handle tau2.levels and gamma2.levels arguments for rma.mv()

  # metafor requires data for predict to be a model matrix (with no intercept)
  if (!is.null(data)) {
    newmods <- .create_newmods_rma(x, data)
    if (has_scale_model) {
      newscale <- .create_newscale_rma(x, data)
    }
  }

  if (predict %in% c("link", "expectation", "prediction")) {
    if (!is.null(data)) {
      out <- .safe(stats::predict(x, transf = transf, targs = transf_args))
    } else if (has_scale_model) {
      out <- .safe(stats::predict(x, newmods = newmods, newscale = newscale, transf = transf, targs = transf_args))
    } else {
      out <- .safe(stats::predict(x, newmods = newmods, transf = transf, targs = transf_args))
    }
    if (predict == "prediction") {
      out <- stats::setNames(
        as.data.frame(out)[, c("pred", "se", "pi.lb", "pi.ub")],
        c("Predicted", "SE", "CI_low", "CI_high")
      )
    } else {
      out <- stats::setNames(
        as.data.frame(out)[, c("pred", "se", "ci.lb", "ci.ub")],
        c("Predicted", "SE", "CI_low", "CI_high")
      )
    }
  } else if (predict == "blup") {
    if (!is.null(data)) {
      check_if_installed("metafor")
      out <- .safe(metafor::blup(x, transf = transf, targs = transf_args))
    } else if (has_scale_model) {
      # TODO: Remove this helper function if metafor adds support for newmods/newscale in metafor::blup()
      out <- .safe(
        .get_blup_rma(
          x,
          ci = ci,
          newmods = newmods,
          newscale = newscale,
          transf = transf,
          targs = transf_args
        )
      )
    } else {
      # TODO: Remove this helper function if metafor adds support for newmods in metafor::blup()
      out <- .safe(.get_blup_rma(
        x,
        ci = ci,
        newmods = newmods,
        transf = transf,
        targs = transf_args
      ))
    }
    out <- stats::setNames(as.data.frame(out), c("Predicted", "SE", "CI_low", "CI_high"))
  } else {
    format_error("`predict` must be one of 'link', 'expectation', 'prediction', or 'blup'.")
  }

  if (!is.null(out)) {
    # Handle single-row output from intercept-only models
    if (nrow(out) == 1) {
      out <- do.call(rbind, lapply(seq_along(x$slab), function(i) out))
    }
    out <- .get_predicted_out(out, my_args = list(data = data))
  }

  out
}


# afex ------------------------------------------------------------------
# =======================================================================

#' @export
get_predicted.afex_aov <- function(x, data = NULL, ...) {
  if (is.null(data)) {
    my_args <- c(list(x), list(...))
  } else {
    my_args <- c(list(x, newdata = data), list(...))
  }

  out <- .safe(do.call("predict", my_args))

  if (is.null(out)) {
    out <- .safe(do.call("fitted", my_args))
  }

  if (!is.null(out)) {
    out <- .get_predicted_out(out, my_args = list(data = data))
  }

  out
}


# phylolm ---------------------------------------------------------------
# =======================================================================

#' @export
get_predicted.phylolm <- function(x,
                                  data = NULL,
                                  predict = "expectation",
                                  verbose = TRUE,
                                  ...) {
  # evaluate arguments
  my_args <- .get_predicted_args(x, data = data, predict = predict, verbose = verbose, ...)

  # evaluate dots, remove some arguments that might be duplicated else
  dot_args <- list(...)
  dot_args[["newdata"]] <- NULL
  dot_args[["type"]] <- NULL


  # 1. step: predictions
  predict_args <- compact_list(list(x, newdata = my_args$data, type = my_args$type, dot_args))
  predictions <- .safe(do.call("predict", predict_args))

  # may fail due to invalid "dot_args", so try shorter argument list
  if (is.null(predictions)) {
    predictions <- .safe(
      do.call("predict", compact_list(list(x, newdata = my_args$data, type = my_args$type)))
    )
  }

  # stop here if we have no predictions
  if (is.null(predictions) && isTRUE(verbose)) {
    format_warning(
      paste0("Could not compute predictions for model of class `", class(x)[1], "`.")
    )
  }

  # sometimes, a mtrix is returned
  if (is.matrix(predictions)) {
    predictions <- predictions[, 1]
  }
  # 2. step: final preparation
  if (!is.null(out)) {
    out <- .get_predicted_out(predictions, my_args = my_args, ci_data = NULL)
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
                                     my_args = NULL,
                                     ci_data = NULL,
                                     link_inv = NULL,
                                     verbose = FALSE,
                                     ...) {
  # Transform to response scale
  if (isTRUE(my_args$transform)) {
    # retrieve link-inverse, for back transformation...
    if (is.null(link_inv)) {
      link_inv <- .link_inverse(model = x, verbose = verbose, ...)
    }

    if (!is.null(ci_data)) {
      # Transform CI
      se_col <- names(ci_data) == "SE"

      # fix for R 3.4
      row.names(ci_data) <- NULL

      ci_data[!se_col] <- lapply(ci_data[!se_col], link_inv)

      # Transform SE (https://github.com/SurajGupta/r-source/blob/master/src/library/stats/R/predict.glm.R#L60)
      # Delta method; SE * deriv( inverse_link(x) wrt lin_pred(x) )
      mu_eta <- .safe(abs(get_family(x)$mu.eta(predictions)))
      if (is.null(mu_eta)) {
        ci_data[se_col] <- NULL
        if (isTRUE(verbose)) {
          format_warning(
            "Could not apply Delta method to transform standard errors.",
            "You may be able to obtain standard errors by using the ",
            "`predict=\"link\"` argument value."
          )
        }
      } else {
        ci_data[se_col] <- ci_data[se_col] * mu_eta
      }
    }

    # Transform predictions
    predictions <- link_inv(predictions)

    # Transform iterations
    if ("iterations" %in% names(attributes(predictions))) {
      attr(predictions, "iterations") <- as.data.frame(sapply(attributes(predictions)$iterations, link_inv)) # nolint
    }

    # Transform to response "type"
    if (my_args$predict == "classification" && model_info(x, response = 1, verbose = FALSE)$is_binomial) {
      response <- get_response(x, as_proportion = TRUE)
      ci_data[!se_col] <- lapply(ci_data[!se_col], .get_predict_transform_response, response = response)
      predictions <- .get_predict_transform_response(predictions, response = response)
      if ("iterations" %in% names(attributes(predictions))) {
        attr(predictions, "iterations") <- as.data.frame(
          sapply( # nolint
            attributes(predictions)$iterations,
            .get_predict_transform_response,
            response = response
          )
        )
      }
    }
  }

  list(predictions = predictions, ci_data = ci_data)
}


# internal to return possibly bias correct link-function
.link_inverse <- function(model = NULL,
                          bias_correction = FALSE,
                          sigma = NULL,
                          verbose = TRUE,
                          ...) {
  if (isTRUE(bias_correction)) {
    dots <- list(...)
    if (!is.null(sigma) && !is.na(sigma)) {
      residual_variance <- sigma^2
    } else {
      residual_variance <- NULL
    }
    l <- .bias_correction(model, residual_variance, verbose)$linkinv
    if (is.null(l)) {
      l <- link_inverse(model)
    }
  } else {
    l <- link_inverse(model)
  }
  l
}


# apply bias-correction for back-transformation of predictions on the link-scale
# we want sigma^2 (residual_variance) here to calculate the correction
.bias_correction <- function(model = NULL, residual_variance = NULL, verbose = TRUE) {
  # we need a model object
  if (is.null(model)) {
    return(NULL)
  }
  # extract residual variance, if not provided
  if (is.null(residual_variance)) {
    residual_variance <- .get_residual_variance(model) # returns sigma^2
  }
  # we need residual variance
  if (is.null(residual_variance)) {
    if (verbose) {
      format_alert("Could not extract residual variance to apply bias correction. No bias adjustment carried out.") # nolint
    }
    return(NULL)
  }

  # extract current link function
  link <- .safe(get_family(model))
  # we need a link function
  if (is.null(link)) {
    if (verbose) {
      format_alert("Could not extract information about the model's link-function to apply bias correction. No bias adjustment carried out.") # nolint
    }
    return(NULL)
  }

  link$inv <- link$linkinv
  link$der <- link$mu.eta
  link$residual_variance <- residual_variance / 2

  link$der2 <- function(eta) {
    with(link, 1000 * (der(eta + 5e-4) - der(eta - 5e-4)))
  }
  link$linkinv <- function(eta) {
    with(link, inv(eta) + residual_variance * der2(eta))
  }
  link$mu.eta <- function(eta) {
    with(link, der(eta) + 1000 * residual_variance * (der2(eta + 5e-4) - der2(eta - 5e-4)))
  }
  link
}


.get_residual_variance <- function(x) {
  if (is_mixed_model(x)) {
    out <- .safe(get_variance_residual(x))
  } else {
    out <- .safe(.get_sigma(x, no_recursion = TRUE, verbose = FALSE)^2, 0)
    if (!length(out)) {
      out <- 0
    }
  }
  out
}


# -------------------------------------------------------------------------

.get_predicted_out <- function(predictions, my_args = NULL, ci_data = NULL, ...) {
  if (!is.null(ci_data)) {
    attr(predictions, "ci_data") <- ci_data
  }
  if (!is.null(my_args)) {
    attr(predictions, "data") <- my_args$data
    attr(predictions, "ci") <- my_args$ci
    attr(predictions, "predict") <- my_args$predict
  }

  # multidimensional or "grouped" predictions (e.g., nnet::multinom with `predict(type="probs")`)
  if (is.matrix(predictions) && ncol(predictions) > 1L) {
    predictions <- as.data.frame(predictions)
    vary <- colnames(predictions)
    predictions$Row <- seq_len(nrow(predictions))
    # if we have any focal predictors, add those as well, so we have
    # the associated levels/values for "Row"
    if (!is.null(my_args$data)) {
      focal_predictors <- .safe(names(which(n_unique(my_args$data) > 1L)))
      if (!is.null(focal_predictors)) {
        predictions <- cbind(predictions, my_args$data[focal_predictors])
      }
    }
    # we have "Component" instead of "Response" for Wiener models
    if ("Component" %in% colnames(predictions)) {
      time_var <- "Component"
    } else {
      time_var <- "Response"
    }
    predictions <- stats::reshape(predictions,
      direction = "long",
      varying = vary,
      times = vary,
      v.names = "Predicted",
      timevar = time_var,
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
  if (is.null(data)) data <- get_data(x, verbose = FALSE)

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
    draws <- boot::boot(data = get_data(x, verbose = FALSE), boot_fun, R = iterations, predict_data = data, ...)
  }

  # Format draws
  draws <- as.data.frame(t(draws$t))
  names(draws) <- paste0("iter_", seq_len(ncol(draws)))

  .get_predicted_centrality_from_draws(x, draws, ...)
}


# -------------------------------------------------------------------------

.get_predicted_centrality_from_draws <- function(x,
                                                 iter,
                                                 centrality_function = base::mean,
                                                 datagrid = NULL,
                                                 is_wiener = FALSE,
                                                 is_rtchoice = FALSE,
                                                 is_mixture = FALSE,
                                                 ...) {
  # outcome: ordinal/multinomial/multivariate/mixture produce a 3D array of
  # predictions, which we stack in "long" format
  if (length(dim(iter)) == 3) {
    # 3rd dimension of the array is the response level. This stacks the draws into:
    # Rows * Response ~ Draws
    iter_stacked <- apply(iter, 1, c)
    # create name for groups. for mixture, this is NULL
    dim_names <- dimnames(iter)[[3]]
    if (is.null(dim_names)) {
      dim_names <- as.character(seq_len(dim(iter)[3]))
    }

    predictions <- data.frame(
      # rows repeated for each response level
      Row = rep(seq_len(ncol(iter)), times = dim(iter)[3]),
      # response levels repeated for each row
      .Response_dummy = rep(dim_names, each = dim(iter)[2]),
      Predicted = apply(iter_stacked, 1, centrality_function),
      stringsAsFactors = FALSE
    )
    # make sure we have the correct name for the "Response column"
    if (is_wiener || is_rtchoice) {
      new_name <- "Component"
    } else if (is_mixture) {
      new_name <- "Class"
    } else {
      new_name <- "Response"
    }
    names(predictions)[names(predictions) == ".Response_dummy"] <- new_name
    # for ordinal etc. outcomes, we need to include the data from the grid, too
    if (!is.null(datagrid)) {
      # due to reshaping predictions into long format, we to repeat the
      # datagrid multiple times, to have same number of rows
      times <- nrow(predictions) / nrow(datagrid)
      if (nrow(predictions) %% times == 0) {
        datagrid <- do.call(rbind, replicate(times, datagrid, simplify = FALSE))
        predictions <- cbind(predictions[1:2], datagrid, predictions[3])
      }
    }
    iter <- as.data.frame(iter_stacked)
    # outcome with a single level
  } else {
    # .get_predicted_boot already gives us the correct observation ~ draws format
    if (is.null(colnames(iter)) || !all(startsWith(colnames(iter), "iter"))) {
      iter <- as.data.frame(t(iter))
    }
    predictions <- apply(iter, 1, centrality_function)
  }
  # Rename iterations
  names(iter) <- paste0("iter_", seq_len(ncol(iter)))
  # Store as attribute
  attr(predictions, "iterations") <- iter
  predictions
}


# -------------------------------------------------------------------------

.create_newmods_rma <- function(x, data, ...) {}

.create_newscale_rma <- function(x, data, ...) {}

.get_blup_rma <- function(x, data, ci = NULL, ...) {
  if (is.element(x$test, c("knha", "adhoc", "t"))) {
    crit <- stats::qt(ci / 2, df = x$ddf, lower.tail = FALSE)
  } else {
    crit <- stats::qnorm(ci / 2, lower.tail = FALSE)
  }
}
