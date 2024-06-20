#' @title Checks whether a list of models are nested models
#' @name is_nested_models
#'
#' @description Checks whether a list of models are nested models, strictly
#'   following the order they were passed to the function.
#'
#' @param ... Multiple regression model objects.
#'
#' @return `TRUE` if models are nested, `FALSE` otherwise. If models
#' are nested, also returns two attributes that indicate whether nesting of
#' models is in decreasing or increasing order.
#'
#' @details The term "nested" here means that all the fixed predictors of a
#' model are contained within the fixed predictors of a larger model (sometimes
#' referred to as the encompassing model). Currently, `is_nested_models()` ignores
#' random effects parameters.
#'
#' @examples
#' m1 <- lm(Sepal.Length ~ Petal.Width + Species, data = iris)
#' m2 <- lm(Sepal.Length ~ Species, data = iris)
#' m3 <- lm(Sepal.Length ~ Petal.Width, data = iris)
#' m4 <- lm(Sepal.Length ~ 1, data = iris)
#'
#' is_nested_models(m1, m2, m4)
#' is_nested_models(m4, m2, m1)
#' is_nested_models(m1, m2, m3)
#' @export
is_nested_models <- function(...) {
  model_objects <- list(...)
  object_names <- match.call(expand.dots = FALSE)[["..."]]

  if (!all(vapply(model_objects, is_regression_model, TRUE))) {
    format_error("All models must be valid regression model objects.")
  }
  names(model_objects) <- object_names
  info <- ellipsis_info.ListRegressions(model_objects)

  out <- isTRUE(attributes(info)$is_nested)
  attr(out, "is_nested_increasing") <- attributes(info)$is_nested_increasing
  attr(out, "is_nested_decreasing") <- attributes(info)$is_nested_decreasing

  out
}
