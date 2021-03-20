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
      s <- summary(x)
      estimate_pos <- which(colnames(s) == x@misc$estName)
      out <- list(conditional = colnames(s)[1:(estimate_pos - 1)])
    }
  } else {
    col_names <- colnames(get_parameters(x, summary = FALSE, merge_parameters = merge_parameters))
    if ("Component" %in% colnames(params)) {
      params$Parameter <- col_names
      out <- lapply(split(params, params$Component), function(i) i[[1]])
    } else {
      out <- list(conditional = col_names)
    }
  }

  if (flatten) {
    unique(unlist(out))
  } else {
    out
  }
}


#' @export
find_parameters.emm_list <- function(x, flatten = FALSE, merge_parameters = FALSE, ...) {
  out <- params <- get_parameters(x, summary = TRUE, merge_parameters = merge_parameters)
  if ("Component" %in% colnames(params)) {
    params$Component <- factor(params$Component, levels = unique(params$Component))
  }
  if (!.is_baysian_emmeans(x)) {
    if ("Component" %in% colnames(params)) {
      out <- lapply(split(params, params$Component), function(i) i[[1]])
    } else {
      s <- summary(x)[[1]]
      estimate_pos <- which(colnames(s) == x[[1]]@misc$estName)
      out <- list(conditional = colnames(s)[1:(estimate_pos - 1)])
    }
  } else {
    col_names <- colnames(get_parameters(x, summary = FALSE, merge_parameters = merge_parameters))
    if ("Component" %in% colnames(params)) {
      params$Parameter <- col_names
      out <- lapply(split(params, params$Component), function(i) i[[1]])
    } else {
      out <- list(conditional = col_names)
    }
  }

  if (flatten) {
    unique(unlist(out))
  } else {
    out
  }
}
