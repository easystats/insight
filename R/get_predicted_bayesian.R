# Bayesian --------------------------------------------------------------
# =======================================================================


#' @rdname get_predicted
#' @export
get_predicted.stanreg <- function(x,
                                  data = NULL,
                                  predict = "expectation",
                                  iterations = NULL,
                                  ci = NULL,
                                  ci_method = NULL,
                                  include_random = "default",
                                  include_smooth = TRUE,
                                  verbose = TRUE,
                                  ...) {
  check_if_installed("rstantools")

  if (is.null(ci_method)) {
    ci_method <- "quantile"
  }
  ci_method <- match.arg(
    tolower(ci_method),
    choices = c("quantile", "eti", "hdi")
  )

  my_args <- .get_predicted_args(
    x,
    data = data,
    predict = predict,
    include_random = include_random,
    include_smooth = include_smooth,
    ci = ci,
    verbose = verbose,
    ...
  )

  # we have now a validated "predict"...
  predict <- my_args$predict

  # when the `type` argument is passed through ellipsis, we need to manually set
  # the `my_args$predict` value, because this is what determines which `rstantools`
  # function we will use to draw from the posterior predictions.
  # dots <- list(...)
  # if (is.null(predict) && "type" %in% names(dots)) {
  #   if (dots$type == "link") {
  #     my_args$predict <- "link"
  #   } else if (dots$type == "response") {
  #     my_args$predict <- "expectation"
  #   }
  # }

  # prepare arguments, avoid possible matching by multiple actual arguments
  fun_args <- list(x,
    newdata = my_args$data,
    re.form = my_args$re.form,
    dpar = my_args$distributional_parameter,
    draws = iterations
  )

  # The following makes the argument passed to predict "nsamples" or "ndraws",
  # as it got changed in recent brms versions (but we want to preserve compatibility)
  check_s3_method <- tryCatch(
    expr = {
      names(formals(utils::getS3method("posterior_predict", class(x)[1])))
    },
    error = function(e) NULL
  )
  if (!is.null(check_s3_method) && "nsamples" %in% check_s3_method) {
    fun_args <- c(fun_args, nsamples = iterations)
  } else {
    fun_args <- c(fun_args, ndraws = iterations)
  }

  # Fix dots content
  dots <- list(...)
  dots[["newdata"]] <- NULL
  fun_args <- c(fun_args, dots)

  # Get draws
  if (my_args$predict == "link") {
    draws <- do.call(rstantools::posterior_linpred, fun_args)
  } else if (my_args$predict %in% c("expectation", "response")) {
    draws <- do.call(rstantools::posterior_epred, fun_args)
  } else {
    draws <- do.call(rstantools::posterior_predict, fun_args)
  }

  # Get predictions (summarize)
  predictions <- .get_predicted_centrality_from_draws(
    x,
    iter = draws,
    datagrid = my_args$data,
    ...
  )

  # Output
  ci_data <- get_predicted_ci(
    x,
    predictions = predictions,
    data = my_args$data,
    ci_type = my_args$ci_type,
    ci = ci,
    ci_method = ci_method,
    ...
  )

  .get_predicted_out(predictions, my_args = my_args, ci_data = ci_data)
}


#' @export
get_predicted.brmsfit <- get_predicted.stanreg
