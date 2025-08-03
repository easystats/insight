# Other models ----------------------------------------------------------
# =======================================================================


#' @export
get_predicted.crr <- function(x, verbose = TRUE, ...) {
  out <- as.data.frame(unclass(stats::predict(x, ...)))
  class(out) <- c("get_predicted", class(out))
  out
}


#' @export
get_predicted.sdmTMB <- function(x,
                                 data = NULL,
                                 predict = "response",
                                 ci = NULL,
                                 verbose = TRUE,
                                 ...) {
  # evaluate arguments
  my_args <- .get_predicted_args(x, data = data, predict = predict, ci = ci, verbose = verbose, ...)

  # evaluate dots, remove some arguments that might be duplicated else
  dot_args <- list(...)
  dot_args[["newdata"]] <- NULL
  dot_args[["type"]] <- NULL

  # 1. step: predictions
  predictions <- stats::predict(
    x,
    newdata = as.data.frame(my_args$data),
    se_fit = !is.null(my_args$ci),
    re_form = NA, # i.e., spatial/spatiotemporal random fields off
    re_form_iid = NA,
    ...
  )

  # copy standard errors and predictions
  se <- as.vector(predictions$est_se)
  predictions <- as.vector(predictions$est)

  # 2. step: confidence intervals
  ci_data <- .safe({
    get_predicted_ci(
      x,
      predictions,
      data = my_args$data,
      ci = my_args$ci,
      se = se,
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


# FA / PCA -------------------------------------------------------------
# ======================================================================


#' @rdname get_predicted
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
    format_error(
      "A dataframe (either the original of a new one) must be provided (`get_predicted(fa_results, data = df`)."
    )
  } else {
    out <- as.data.frame(fungible::faScores(X = data, faMainObject = x)$fscores)
  }
  class(out) <- c("get_predicted", class(out))
  out
}


#' @export
get_predicted.glmgee <- function(x,
                                 data = NULL,
                                 predict = "expectation",
                                 ci = NULL,
                                 vcov = NULL,
                                 verbose = TRUE,
                                 ...) {
  # sanitize argument
  if (is.null(vcov)) {
    vcov <- "robust"
  }
  vcov <- validate_argument(vcov, c("robust", "df-adjusted", "model", "bias-corrected"))

  # setup predict function
  predict_function <- function(x, data, ...) {
    as.data.frame(stats::predict(
      x,
      newdata = data,
      se.fit = TRUE,
      type = "link",
      varest = vcov,
      ...
    ))
  }

  my_args <- .get_predicted_args(x, data = data, predict = predict, verbose = verbose, ...)

  # 1. step: predictions
  predictions <- predict_function(x, data = my_args$data)
  se <- predictions$se.fit
  predictions <- predictions$fit

  # 2. step: confidence intervals
  ci_data <- get_predicted_ci(
    x,
    predictions,
    data = my_args$data,
    se = se,
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


# htest -------------------------------------------------------------
# ======================================================================


#' @export
get_predicted.htest <- function(x, ...) {
  info <- model_info(x)
  if (info$is_chi2test) {
    return(x$expected)
  }
  NULL
}
