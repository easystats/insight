#' @title Extract model residuals
#'
#' @description Returns the residuals from regression models.
#'
#' @name get_residuals
#'
#' @param x A model.
#' @param verbose Toggle warnings and messages.
#' @param ... Passed down to \code{residuals()}, if possible.
#'
#' @return The residuals, or \code{NULL} if this information could not be accessed.
#'
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
#' @importFrom stats predict residuals
#' @export
get_residuals.default <- function(x, verbose = TRUE, ...) {

  res <- tryCatch(
    {
      x$residuals
    },
    error = function(e) {
      NULL
    }
  )

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
        stats::residuals(x, ...)
      },
      error = function(e) {
        NULL
      }
    )
  }

  if (is.null(res)) {
    res <- tryCatch(
      {
        pred <- stats::predict(x, type = "response")
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
  }

  res
}


#' @export
get_residuals.vgam <- function(x, ...) {
  x@residuals
}


#' @export
get_residuals.vglm <- get_residuals.vgam


#' @export
get_residuals.coxph <- function(x, ...) {
  stats::residuals(x, ...)
}


#' @importFrom utils capture.output
#' @export
get_residuals.slm <- function(x, verbose = TRUE, ...) {
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
