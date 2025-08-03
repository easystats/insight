# Bayesian --------------------------------------------------------------
# =======================================================================

#' @rdname get_predicted
#' @export
get_predicted.stanreg <- function(
    x,
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
  fun_args <- list(
    x,
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
  # these arguments have already been set
  dots[names(fun_args)] <- NULL

  fun_args <- c(fun_args, dots)

  model_family <- get_family(x)
  # exceptions
  is_wiener <- inherits(model_family, "brmsfamily") && model_family$family == "wiener"
  is_rtchoice <- inherits(model_family, "brmsfamily") &&
    model_family$family == "custom" &&
    model_family$name == "lnr"
  is_mixture <- inherits(model_family, "brmsfamily") && model_family$family == "mixture"

  # Special case for rwiener (get choice 1 as negative values)
  # Note that for mv models, x$family returns a list of families
  if (is_wiener) {
    fun_args$negative_rt <- TRUE
  }

  # Get draws
  if (my_args$predict == "link") {
    draws <- do.call(rstantools::posterior_linpred, fun_args)
  } else if (my_args$predict %in% c("expectation", "response")) {
    draws <- do.call(rstantools::posterior_epred, fun_args)
  } else {
    draws <- do.call(rstantools::posterior_predict, fun_args)
  }

  # Handle special cases
  if (
    !my_args$predict %in% c("expectation", "response", "link") &&
      inherits(model_family, "brmsfamily")
  ) {
    if (is_wiener) {
      # Wiener (Drift Diffusion) Models --------------------
      # ----------------------------------------------------
      # Separate RT from Choice and assemble into 3D matrix (as if it was a multivariate)
      response <- as.numeric(draws >= 0)
      draws <- abs(draws)
      draws <- array(
        c(draws, response),
        dim = c(dim(draws), 2),
        dimnames = list(NULL, NULL, c("rt", "response"))
      )
    } else if (is_rtchoice) {
      # Reaction time and Choice Models --------------------
      # ----------------------------------------------------
      # LogNormal Race models (cogmod package) return RT and Choice as odd and even columns
      response <- as.matrix(draws[, seq(2, ncol(draws), 2)])
      draws <- as.matrix(draws[, seq(1, ncol(draws), 2)])
      draws <- array(
        c(draws, response),
        dim = c(dim(draws), 2),
        dimnames = list(NULL, NULL, c("rt", "response"))
      )
    } else if (is_mixture && identical(my_args$predict, "classification")) {
      # Mixture (multi membership) Models --------------------
      # ------------------------------------------------------
      # confidence intervals?
      if (!is.null(ci) && !is.na(ci)) {
        probs <- c((1 - ci) / 2, (1 + ci) / 2)
        fun_args$probs <- probs
      }
      # for mixture models, which predict the class membership, we stop
      # here and just return the predicted class membership
      mixture_output <- do.call(brms::pp_mixture, fun_args)
      # pp_mixture() returns an array with predicted probability for class
      # assignment for each observation. we here return the class definition
      # with the highest probability per observation
      predictions <- apply(mixture_output[, 1, ], 1, which.max)
      # add confidence intervals
      if (is.null(ci) || is.na(ci)) {
        ci_data <- NULL
      } else {
        # pp_mixture returns an array with probs, SE and intervals.
        # if requested, we extract the intervals here for the "ci_data"
        # data.frame
        res <- lapply(seq_len(nrow(mixture_output)), function(i) {
          max_prob <- which.max(mixture_output[i, 1, ])
          data.frame(
            Probability = mixture_output[i, 1, max_prob],
            CI_low = mixture_output[i, 3, max_prob],
            CI_high = mixture_output[i, 4, max_prob]
          )
        })
        ci_data <- do.call(rbind, res)
      }
      return(.get_predicted_out(predictions, my_args = my_args, ci_data = ci_data))
    }
  }

  # Get predictions (summarize)
  predictions <- .get_predicted_centrality_from_draws(
    x,
    iter = draws,
    datagrid = my_args$data,
    is_wiener = is_wiener,
    is_rtchoice = is_rtchoice,
    is_mixture = is_mixture,
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
