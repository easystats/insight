#' @title Extract model residuals
#'
#' @description Returns the residuals from regression models.
#'
#' @name get_residuals
#'
#' @param x A model.
#' @param weighted Logical, if \code{TRUE}, returns weighted residuals.
#' @param verbose Toggle warnings and messages.
#' @param ... Passed down to \code{residuals()}, if possible.
#'
#' @return The residuals, or \code{NULL} if this information could not be accessed.
#'
#' @note This function returns the default type of residuals, i.e. for the
#' response from linear models, the deviance residuals for models of class
#' \code{glm} etc. To access different types, pass down the \code{type} argument
#' (see 'Examples').
#' \cr \cr
#' This function is a robust alternative to \code{residuals()}, as it works
#' for some special model objects that otherwise do not respond properly to
#' calling \code{residuals()}.
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
#' @importFrom stats predict residuals fitted
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

  res <- tryCatch(
    {
      stats::residuals(x, ...)
    },
    error = function(e) {
      NULL
    }
  )

  if (is.null(res)) {
    res <- tryCatch(
      {
        x$residuals
      },
      error = function(e) {
        NULL
      }
    )
  }

  # For gamm4 objects
  if (is.null(res)) {
    res <- tryCatch(
      {
        x$gam$residuals
      },
      error = function(e) {
        NULL
      }
    )
  }

  if (is.null(res)) {
    res <- tryCatch(
      {
        yield_warning <- no_response_resid && verbose
        pred <- stats::predict(x, type = "response")
        observed <- .factor_to_numeric(get_response(x))
        observed - pred
      },
      error = function(e) {
        NULL
      }
    )
  }

  if (is.null(res)) {
    res <- tryCatch(
      {
        yield_warning <- no_response_resid && verbose
        pred <- stats::fitted(x)
        observed <- .factor_to_numeric(get_response(x))
        observed - pred
      },
      error = function(e) {
        NULL
      }
    )
  }

  if (is.null(res) || all(is.na(res))) {
    if (verbose) warning("Can't extract residuals from model.")
    res <- NULL
  } else if (yield_warning) {
    warning(paste0("Can't extract '", res_type, "' residuals. Returning response residuals."), call. = FALSE)
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
get_residuals.coxph <- function(x, weighted = FALSE, verbose = TRUE, ...) {
  if (isTRUE(weighted)) {
    return(.weighted_residuals(x, verbose))
  }
  stats::residuals(x, ...)
}



#' @export
get_residuals.crr <- function(x, weighted = FALSE, verbose = TRUE, ...) {
  if (isTRUE(weighted) && isTRUE(verbose)) {
    warning("Weighted residuals are not supported for 'crr' models.", call. = FALSE)
  }
  x$res
}



#' @importFrom utils capture.output
#' @export
get_residuals.slm <- function(x, weighted = FALSE, verbose = TRUE, ...) {
  if (isTRUE(weighted)) {
    return(.weighted_residuals(x, verbose))
  }

  res <- tryCatch(
    {
      junk <- utils::capture.output(pred <- stats::predict(x, type = "response"))
      observed <- .factor_to_numeric(get_response(x))
      observed - pred
    },
    error = function(e) {
      NULL
    }
  )

  if (is.null(res) || all(is.na(res))) {
    if (verbose) warning("Can't extract residuals from model.")
    res <- NULL
  }

  res
}




.weighted_residuals <- function(x, verbose = TRUE) {
  w <- get_weights(x)
  tryCatch(
    {
      res_resp <- as.vector(get_residuals(x, weighted = FALSE, type = "response"))
      res_dev <- as.vector(get_residuals(x, weighted = FALSE, type = "deviance"))

      if (!is.null(w) && !is.null(res_dev)) {
        if (!is.null(res_resp) && identical(res_resp, res_dev)) {
          res_dev <- res_dev * w^0.5
        }
        res_dev <- res_dev[!is.na(w) & w != 0]
      } else if (verbose) {
        if (is.null(w)) {
          warning("Can't calculate weighted residuals from model. Model doesn't seem to have weights.", call. = FALSE)
        } else if (is.null(res_dev)) {
          warning("Can't calculate weighted residuals from model. Could not extract deviance-residuals.", call. = FALSE)
        }
      }
      res_dev
    },
    error = function(e) {
      if (verbose) {
        warning("Can't calculate weighted residuals from model.", call. = FALSE)
      }
      NULL
    }
  )
}
