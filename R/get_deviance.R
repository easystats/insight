#' Model Deviance
#'
#' Returns model deviance (see \code{stats::deviance()}).
#'
#' @param ... Not used.
#' @inheritParams get_residuals
#'
#' @return The model deviance.
#'
#' @examples
#' data(mtcars)
#' x <- lm(mpg ~ cyl, data = mtcars)
#' get_deviance(x)
#'
#' if (require("rstanarm")) {
#'   x <- rstanarm::stan_glm(mpg ~ cyl, data = mtcars, refresh = 0)
#'   get_deviance(x)
#' }
#' @export
get_deviance <- function(x, ...) {
  UseMethod("get_deviance")
}

#' @importFrom stats deviance
#' @export
get_deviance.default <- function(x, ...) {
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
  dev
}



#' @export
get_deviance.stanreg <- function(x, ...) {

  info <- model_info(x)
  # That's the formula for stats::deviance.lm
  # sum(weighted.residuals(x)^2, na.rm=TRUE)
  if (info$is_linear) {
    res <- get_residuals(x, weighted = TRUE) # TODO: currently this arg doesn't do anything, but should
    dev <- sum(res^2, na.rm = TRUE)
  } else{
    stop("Could not compute deviance for this type of model")
  }

  # Not sure if it generalizes to other models though since deviance.glm
  # extracts it via x@deviance
  dev
}
