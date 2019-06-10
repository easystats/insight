#' @title Get model weights
#' @name get_weights
#'
#' @description Returns weighting variable of a model.
#'
#' @param x A fitted model.
#'
#' @return The weighting variable, or \code{NULL} if no weights were specified.
#'
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
#' find_weights(m)
#' @export
get_weights <- function(x) {
  w <- NULL
  tryCatch(
    {
      w <- stats::weights(x)
    },
    error = function(x) { NULL },
    warning = function(x) { NULL },
    finally = function(x) { NULL }
  )

  if (is.null(w)) {
    tryCatch(
      {
        w <- stats::model.frame(x)[["(weights)"]]
      },
      error = function(x) { NULL },
      warning = function(x) { NULL },
      finally = function(x) { NULL }
    )
  }

  if (is.null(w)) {
    tryCatch(
      {
        w <- .get_data_from_env(x)[[find_weights(x)]]
      },
      error = function(x) { NULL },
      warning = function(x) { NULL },
      finally = function(x) { NULL }
    )
  }

  w
}
