#' @title Extract model residuals
#'
#' @description Returns the residuals from regression models.
#'
#' @name get_residuals
#'
#' @param x A model.
#' @param verbose Toggle warnings and messages.
#' @param ... Not used.
#'
#' @return The residuals, or \code{NULL} if this information could not be accessed.
#'
#'
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
#' get_residuals(m)
#' @export
get_residuals <- function(x, ...) {
  UseMethod("get_residuals")
}



#' @importFrom stats predict residuals
#' @export
get_residuals.default <- function(x, verbose = TRUE, ...) {
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

  if (is.null(res)) {
    res <- tryCatch(
      {
        stats::residuals(x, type = "response")
      },
      error = function(e) {
        NULL
      }
    )
  }

  if (is.null(res) || all(is.na(res))) {
    if (verbose) warning("Can't extract residuals from model.")
    return(NULL)
  }
}


#' @export
get_residuals.vgam <- function(x, ...) {
  x@residuals
}


#' @export
get_residuals.vglm <- get_residuals.vgam


#' @export
get_residuals.coxph <- function(x, ...) {
  stats::residuals(x)
}
