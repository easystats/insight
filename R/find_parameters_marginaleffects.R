#' @export
find_parameters.predictions <- function(x, ...) {
  check_if_installed("marginaleffects")
  unique(c(
    names(marginaleffects::components(x, "variables")),
    marginaleffects::components(x, "by")
  ))
}

#' @export
find_parameters.comparisons <- function(x, ...) {
  unique(x$term)
}
