#' @title Tidy methods for easystats-objects
#' @name tidy
#'
#' @description Tidy methods for easystats-objects. \code{tidy()} usually returns
#'   the same data frame that was used as input, however, with broom-alike
#'   column names.
#'
#' @param x A data frame, as returned by functions from easystats-packages like \code{\link[parameters:model_parameters]{model_parameters()}} or \code{\link[effectsize:effectsize]{effectsize()}}.
#' @param ... Currently not used.
#'
#' @return \code{x}, with "standardized" column names (see \code{\link{standardize_names}}).
#'
#' @note Due to possible namespace conflicts with other packages that define
#'   a generic \code{tidy()}-method, the lifecycle of this function is still
#'   experimental, and there might be a chance that it will become defunct and
#'   removed in a future update. If possible, the preferred and stable alternative
#'   to \code{tidy()} for \emph{easystats}-objects is \code{\link{standardize_names}}.
#'
#' @examples
#' if (require("parameters")) {
#'   model <- lm(mpg ~ wt + cyl, data = mtcars)
#'   mp <- model_parameters(model)
#'
#'   as.data.frame(mp)
#'   tidy(mp)
#' }
#' @export
tidy <- function(x, ...) {
  UseMethod("tidy")
}

#' @export
tidy.parameters_model <- function(x, ...) {
  standardize_names(x, style = "broom")
}

#' @export
tidy.effectsize_table <- tidy.parameters_model
