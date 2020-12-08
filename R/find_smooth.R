#' @title Find smooth terms from a model object
#' @name find_smooth
#'
#' @description Return the names of smooth terms from a model object.
#'
#' @param x A (gam) model.
#' @inheritParams find_predictors
#'
#' @return A character vector with the name(s) of the smooth terms.
#'
#' @examples
#' if (require("mgcv")) {
#'   data(iris)
#'   model <- gam(Petal.Length ~ Petal.Width + s(Sepal.Length), data = iris)
#'   find_smooth(model)
#' }
#' @export
find_smooth <- function(x, flatten = FALSE) {
  all_terms <- find_terms(x, flatten = TRUE)
  patterns <- "^(s|ti|te|t2|gam::s|VGAM::s|mgcv::s|mgcv::ti|mgcv::te|mgcv::t2|brms::s|brms::t2)\\("
  l <- .compact_list(list(conditional = all_terms[grepl(patterns, all_terms)]))

  if (.is_empty_object(l)) {
    return(NULL)
  }

  if (flatten) {
    unique(unlist(l))
  } else {
    l
  }
}
