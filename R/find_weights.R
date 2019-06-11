#' @title Find names of model weights
#' @name find_weights
#'
#' @description Returns the name of the variable that describes the weights of a model.
#'
#' @param x A fitted model.
#'
#' @return The name of the weighting variable as character vector, or \code{NULL}
#'   if no weights were specified.
#'
#' @examples
#' data(mtcars)
#' mtcars$weight <- rnorm(nrow(mtcars), 1, .3)
#' m <- lm(mpg ~ wt + cyl + vs, data = mtcars, weights = weight)
#' find_weights(m)
#' @export
find_weights <- function(x) {
  tryCatch(
    {
      w <- as.character(parse(text = .safe_deparse(x$call))[[1]]$weights)
      if (.is_empty_object(w)) w <- NULL
      w
    },
    error = function(e) { NULL }
  )
}
