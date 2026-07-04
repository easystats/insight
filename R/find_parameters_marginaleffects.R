#' @export
find_parameters.predictions <- function(x, ...) {
  check_if_installed("marginaleffects")
  unique(c(
    names(marginaleffects::components(mod_ame, "variables")),
    marginaleffects::components(mod_ame, "by")
  ))
}

#' @export
find_parameters.comparisons <- function(x, ...) {
  unique(x$term)
}
