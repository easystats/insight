#' @title Find model parameters from estimated marginal means objects
#' @name find_parameters.emmGrid
#'
#' @description Returns the parameter names from a model.
#'
#' @inheritParams find_parameters
#' @inheritParams find_predictors
#' @inheritParams get_parameters.BGGM
#' @inheritParams get_parameters.emmGrid
#'
#' @return A list of parameter names. For simple models, only one list-element,
#'    \code{conditional}, is returned.
#'
#' @examples
#' data(mtcars)
#' model <- lm(mpg ~ wt * factor(cyl), data = mtcars)
#' if (require("emmeans", quietly = TRUE)) {
#'   emm <- emmeans(model, c("wt", "cyl"))
#'   find_parameters(emm)
#' }
#' @export
find_parameters.emmGrid <- function(x, flatten = FALSE, merge_parameters = FALSE, ...) {
  out <- params <- get_parameters(x, summary = TRUE, merge_parameters = merge_parameters)
  if ("Component" %in% colnames(params)) {
    params$Component <- factor(params$Component, levels = unique(params$Component))
  }
  if (!.is_baysian_emmeans(x)) {
    if ("Component" %in% colnames(params)) {
      out <- lapply(split(params, params$Component), function(i) i[[1]])
    } else {
      out <- stats::setNames(list(params[[1]]), unique(.classify_emmeans(x)))
    }
  } else {
    col_names <- colnames(get_parameters(x, summary = FALSE, merge_parameters = merge_parameters))
    if ("Component" %in% colnames(params)) {
      params$Parameter <- col_names
      out <- lapply(split(params, params$Component), function(i) i[[1]])
    } else {
      out <- stats::setNames(list(col_names), unique(.classify_emmeans(x)))
    }
  }

  if (flatten) {
    unique(unlist(out))
  } else {
    out
  }
}


#' @export
find_parameters.emm_list <- find_parameters.emmGrid
