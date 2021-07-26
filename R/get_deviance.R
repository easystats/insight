#' Model Deviance
#'
#' Returns model deviance (see `stats::deviance()`).
#'
#' @param ... Not used.
#' @inheritParams get_residuals
#'
#' @return The model deviance.
#'
#' @details For GLMMs of class `glmerMod`, `glmmTMB` or `MixMod`,
#' the *absolute unconditional* deviance is returned (see 'Details' in
#' `?lme4::`merMod-class``), i.e. minus twice the log-likelihood. To get
#' the *relative conditional* deviance (relative to a saturated model,
#' conditioned on the conditional modes of random effects), use `deviance()`.
#' The value returned `get_deviance()` usually equals the deviance-value
#' from the `summary()`.
#'
#' @examples
#' data(mtcars)
#' x <- lm(mpg ~ cyl, data = mtcars)
#' get_deviance(x)
#' @export
get_deviance <- function(x, ...) {
  UseMethod("get_deviance")
}


#' @rdname get_deviance
#' @export
get_deviance.default <- function(x, verbose = TRUE, ...) {
  dev <- tryCatch(
    {
      stats::deviance(x, ...)
    },
    error = function(e) {
      NULL
    }
  )

  if (is.null(dev)) {
    dev <- tryCatch(
      {
        x$deviance
      },
      error = function(e) {
        NULL
      }
    )
  }

  if (is.null(dev)) {
    dev <- tryCatch(
      {
        sum(get_residuals(x, weighted = TRUE, verbose = verbose)^2, na.rm = TRUE)
      },
      error = function(e) {
        NULL
      }
    )
  }

  dev
}



#' @export
get_deviance.stanreg <- function(x, verbose = TRUE, ...) {
  info <- model_info(x)

  if (info$is_linear) {
    res <- get_residuals(x, weighted = TRUE, verbose = verbose)
    dev <- sum(res^2, na.rm = TRUE)
  } else if (info$is_binomial) {
    w <- get_weights(x, null_as_ones = TRUE, verbose = verbose)
    n <- n_obs(x)
    y <- get_response(x, verbose = FALSE)
    mu <- stats::fitted(x)

    dev_resids_fun <- x$family$dev.resids

    dev <- sum(dev_resids_fun(y, mu, w))
  } else {
    stop("Could not compute deviance for this type of model")
  }

  # Not sure if it generalizes to other models though since deviance.glm
  # extracts it via x@deviance
  dev
}



#' @export
get_deviance.lmerMod <- function(x, ...) {
  stats::deviance(x, REML = FALSE, ...)
}


#' @export
get_deviance.lrm <- function(x, ...) {
  d <- stats::deviance(x, ...)
  d[length(d)]
}


#' @export
get_deviance.glmmTMB <- function(x, ...) {
  tryCatch(
    {
      -2 * as.numeric(get_loglikelihood(x, ...))
    },
    error = function(e) {
      NULL
    }
  )
}

#' @export
get_deviance.glmerMod <- get_deviance.glmmTMB

#' @export
get_deviance.MixMod <- get_deviance.glmmTMB


#' @export
get_deviance.model_fit <- function(x, ...) {
  get_deviance(x$fit, ...)
}
