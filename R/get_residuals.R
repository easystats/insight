#' @title Extract model residuals
#'
#' @description Returns the residuals from regression models.
#'
#' @name get_residuals
#'
#' @param x A model.
#' @param weighted Logical, if `TRUE`, returns weighted residuals.
#' @param verbose Toggle warnings and messages.
#' @param ... Passed down to `residuals()`, if possible.
#'
#' @return The residuals, or `NULL` if this information could not be
#'   accessed.
#'
#' @note This function returns the default type of residuals, i.e. for the
#' response from linear models, the deviance residuals for models of class
#' `glm` etc. To access different types, pass down the `type` argument (see
#' 'Examples').
#'
#' This function is a robust alternative to `residuals()`, as it works for
#' some special model objects that otherwise do not respond properly to calling
#' `residuals()`.
#'
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
#' get_residuals(m)
#'
#' m <- glm(vs ~ wt + cyl + mpg, data = mtcars, family = binomial())
#' get_residuals(m) # type = "deviance" by default
#' get_residuals(m, type = "response")
#' @export
get_residuals <- function(x, ...) {
  UseMethod("get_residuals")
}


#' @rdname get_residuals
#' @export
get_residuals.default <- function(x, weighted = FALSE, verbose = TRUE, ...) {
  # setup, check if user requested specific type of residuals
  # later, we can only catch response residuals, in such cases, give warning
  # when type is not "response"...

  dot_args <- list(...)
  no_response_resid <- !is.null(dot_args[["type"]]) && dot_args[["type"]] != "response"
  res_type <- dot_args[["type"]]
  yield_warning <- FALSE

  if (isTRUE(weighted)) {
    return(.weighted_residuals(x, verbose))
  }

  if (identical(res_type, "pearson")) {
    return(.pearson_residuals(x))
  }

  res <- .safe(stats::residuals(x, ...))

  if (is.null(res)) {
    res <- .safe(x$residuals)
  }

  # For gamm4 objects
  if (is.null(res)) {
    res <- .safe(x$gam$residuals)
  }

  if (is.null(res)) {
    res <- .safe({
      yield_warning <- no_response_resid && verbose
      pred <- stats::predict(x, type = "response")
      observed <- .factor_to_numeric(get_response(x, verbose = FALSE))
      observed - pred
    })
  }

  if (is.null(res)) {
    res <- .safe({
      yield_warning <- no_response_resid && verbose
      pred <- stats::fitted(x)
      observed <- .factor_to_numeric(get_response(x, verbose = FALSE))
      observed - pred
    })
  }

  if (is.null(res) || all(is.na(res))) {
    if (verbose) {
      format_warning("Can't extract residuals from model.")
    }
    res <- NULL
  } else if (yield_warning) {
    format_warning(paste0(
      "Can't extract '", res_type, "' residuals. Returning response residuals."
    ))
  }

  # fix for haven_labelled
  if (inherits(res, "haven_labelled")) {
    res <- as.numeric(res)
  }

  res
}


#' @export
get_residuals.vgam <- function(x, weighted = FALSE, verbose = TRUE, ...) {
  if (isTRUE(weighted)) {
    return(.weighted_residuals(x, verbose))
  }
  x@residuals
}

#' @export
get_residuals.vglm <- get_residuals.vgam


#' @export
get_residuals.model_fit <- function(x, weighted = FALSE, verbose = TRUE, ...) {
  get_residuals(x$fit, weighted = weighted, verbose = verbose, ...)
}


#' @export
get_residuals.hglm <- function(x, verbose = TRUE, ...) {
  x$resid
}


#' @export
get_residuals.coxph <- function(x, weighted = FALSE, verbose = TRUE, ...) {
  if (isTRUE(weighted)) {
    return(.weighted_residuals(x, verbose))
  }
  stats::residuals(x, ...)
}


#' @export
get_residuals.parameters_efa <- function(x, weighted = FALSE, verbose = TRUE, ...) {
  if (isTRUE(weighted) && isTRUE(verbose)) {
    format_warning("Weighted residuals are not supported for factor analysis models.")
  }

  # extract FA object
  model <- attributes(x)$model

  # sanity check
  if (is.null(model)) {
    if (isTRUE(verbose)) {
      format_warning("The `model` attribute is missing or `NULL`.")
    }
    return(NULL)
  }

  model$residual[upper.tri(model$residual)]
}

#' @export
get_residuals.psych <- function(x, weighted = FALSE, verbose = TRUE, ...) {
  NextMethod()
}

#' @export
get_residuals.fa <- function(x, weighted = FALSE, verbose = TRUE, ...) {
  if (isTRUE(weighted) && isTRUE(verbose)) {
    format_warning("Weighted residuals are not supported for factor analysis models.")
  }
  x$residual[upper.tri(x$residual)]
}

#' @export
get_residuals.omega <- function(x, weighted = FALSE, verbose = TRUE, ...) {
  if (isTRUE(weighted) && isTRUE(verbose)) {
    format_warning("Weighted residuals are not supported for omega.")
  }
  x$stats$residual[upper.tri(x$stats$residual)]
}

#' @export
get_residuals.principal <- get_residuals.fa


#' @export
get_residuals.crr <- function(x, weighted = FALSE, verbose = TRUE, ...) {
  if (isTRUE(weighted) && isTRUE(verbose)) {
    format_warning("Weighted residuals are not supported for `crr` models.")
  }
  x$res
}


#' @export
get_residuals.slm <- function(x, weighted = FALSE, verbose = TRUE, ...) {
  if (isTRUE(weighted)) {
    return(.weighted_residuals(x, verbose))
  }

  res <- .safe({
    junk <- utils::capture.output(pred <- stats::predict(x, type = "response")) # nolint
    observed <- .factor_to_numeric(get_response(x))
    observed - pred
  })

  if (is.null(res) || all(is.na(res))) {
    if (verbose) {
      format_warning("Can't extract residuals from model.")
    }
    res <- NULL
  }

  res
}


#' @export
get_residuals.afex_aov <- function(x, weighted = FALSE, verbose = TRUE, ...) {
  suppressMessages(stats::residuals(x, ...))
}


## brms / stan - special handling -------------------------------------
## ====================================================================

#' @export
get_residuals.brmsfit <- function(x, ...) {
  r_attr <- stats::residuals(x, ...)
  r <- as.vector(r_attr[, "Estimate"])
  attr(r, "full") <- r_attr
  class(r) <- c("insight_residuals", class(r))
  r
}

#' @export
as.data.frame.insight_residuals <- function(x, ...) {
  as.data.frame(attributes(x)$full)
}

#' @export
print.insight_residuals <- function(x, ...) {
  print_colour("Residuals:\n\n", "blue")
  print.default(as.vector(x))
  print_colour("\nNOTE: Credible intervals are stored as attributes and can be accessed using `as.data.frame()` on this output.\n", "yellow") # nolint
}


## weighted residuals -----------------------------
## ================================================

.weighted_residuals <- function(x, verbose = TRUE) {
  w <- get_weights(x, null_as_ones = TRUE)
  tryCatch(
    {
      res_resp <- as.vector(get_residuals(
        x,
        weighted = FALSE,
        type = "response",
        verbose = FALSE
      ))

      res_dev <- as.vector(get_residuals(
        x,
        weighted = FALSE,
        type = "deviance",
        verbose = FALSE
      ))

      if (!is.null(w) && !is.null(res_dev) && !all(w == 1)) {
        if (!is.null(res_resp) && identical(res_resp, res_dev)) {
          res_dev <- res_dev * w^0.5
        }
        res_dev <- res_dev[!is.na(w) & w != 0]
      } else if (verbose) {
        if (is.null(w)) {
          format_warning("Can't calculate weighted residuals from model. Model doesn't seem to have weights.")
        } else if (is.null(res_dev)) {
          format_warning("Can't calculate weighted residuals from model. Could not extract deviance-residuals.")
        }
      }
      res_dev
    },
    error = function(e) {
      if (verbose) {
        format_warning("Can't calculate weighted residuals from model.")
      }
      NULL
    }
  )
}


## pearson residuals -------------------------------------------
## =============================================================


.pearson_residuals <- function(x) {
  pr <- tryCatch(stats::residuals(x, type = "pearson"), error = function(e) NULL)

  if (is_empty_object(pr) && inherits(x, c("glmmTMB", "MixMod"))) {
    faminfo <- insight::model_info(x)
    if (faminfo$is_zero_inflated) {
      if (faminfo$is_negbin) {
        pr <- .resid_zinb(x, faminfo)
      } else {
        pr <- .resid_zip(x, faminfo)
      }
    }
  } else if (is_empty_object(pr)) {
    yhat <- stats::fitted(x)
    pr <- (get_response(x, verbose = FALSE) - yhat) / sqrt(yhat)
  }

  pr
}


.resid_zinb <- function(model, faminfo) {
  if (inherits(model, "glmmTMB")) {
    v <- stats::family(model)$variance
    # zi probability
    p <- stats::predict(model, type = "zprob")
    # mean of conditional distribution
    mu <- stats::predict(model, type = "conditional")
    # sigma
    betadisp <- model$fit$par["betadisp"]
    k <- switch(faminfo$family,
      gaussian = exp(0.5 * betadisp),
      Gamma = exp(-0.5 * betadisp),
      exp(betadisp)
    )
    pvar <- (1 - p) * v(mu, k) + mu^2 * (p^2 + p)
    pred <- stats::predict(model, type = "response") ## (1 - p) * mu
  } else if (inherits(model, "MixMod")) {
    sig <- get_variance_distribution(model, verbose = FALSE)
    p <- stats::plogis(stats::predict(model, type_pred = "link", type = "zero_part"))
    mu <- stats::predict(model, type_pred = "link", type = "mean_subject")
    v <- mu * (1 + sig)
    k <- sig
    pvar <- (1 - p) * v(mu, k) + mu^2 * (p^2 + p)
    pred <- stats::predict(model, type_pred = "response", type = "mean_subject")
  } else {
    sig <- get_variance_distribution(model, verbose = FALSE)
    pvar <- mu * (1 + sig)
    pred <- stats::predict(model, type = "response")
  }

  # pearson residuals
  (get_response(model, verbose = FALSE) - pred) / sqrt(pvar)
}


.resid_zip <- function(model, faminfo) {
  if (inherits(model, "glmmTMB")) {
    p <- stats::predict(model, type = "zprob")
    mu <- stats::predict(model, type = "conditional")
    pvar <- (1 - p) * (mu + p * mu^2)
    pred <- stats::predict(model, type = "response") ## (1 - p) * mu
  } else if (inherits(model, "MixMod")) {
    p <- stats::plogis(stats::predict(model, type_pred = "link", type = "zero_part"))
    mu <- stats::predict(model, type = "mean_subject")
    pvar <- (1 - p) * (mu + p * mu^2)
    pred <- stats::predict(model, type_pred = "response", type = "mean_subject")
  } else {
    sig <- get_variance_distribution(model, verbose = FALSE)
    pvar <- mu * (1 + sig)
    pred <- stats::predict(model, type = "response")
  }

  # pearson residuals
  (get_response(model) - pred) / sqrt(pvar)
}
