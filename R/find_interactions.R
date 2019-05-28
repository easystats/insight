#' @title Find interaction terms from models
#' @name find_interactions
#'
#' @description Returns all lowest to highest order interaction terms from a model.
#'
#' @inheritParams find_predictors
#'
#' @return A list of character vectors that represent the interaction terms.
#'  Depending on \code{component}, the returned list has following
#'  elements (or \code{NULL}, if model has no interaction term):
#'  \itemize{
#'    \item \code{conditional}, interaction terms that belong to the "fixed effects" terms from the model
#'    \item \code{zero_inflated}, interaction terms that belong to the "fixed effects" terms from the zero-inflation component of the model
#'    \item \code{instruments}, for fixed-effects regressions like \code{ivreg}, \code{felm} or \code{plm}, interaction terms that belong to the instrumental variables
#'  }
#'
#' @examples
#' data(mtcars)
#'
#' m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
#' find_interactions(m)
#'
#' m <- lm(mpg ~ wt * cyl + vs * hp * gear + carb, data = mtcars)
#' find_interactions(m)
#'
#' @export
find_interactions <- function(x, component = c("all", "conditional", "zi", "zero_inflated", "dispersion", "instruments"), flatten = FALSE) {
  component <- match.arg(component)

  f <- find_formula(x)
  is_mv <- is_multivariate(f)
  elements <- .get_elements(effects = "fixed", component = component)

  if (is_mv) {
    l <- lapply(f, function(.x) compact_list(lapply(.x[elements], .get_interaction_terms)))
  } else {
    l <- compact_list(lapply(f[elements], .get_interaction_terms))
  }

  if (is_empty_object(l)) {
    return(NULL)
  }

  if (flatten) {
    unique(unlist(l))
  } else {
    l
  }
}



.get_interaction_terms <- function(f) {
  if (is.null(f)) return(NULL)
  terms <- labels(stats::terms(f))
  interaction_terms <- grepl(":", terms, fixed = TRUE)
  if (any(interaction_terms))
    terms[interaction_terms]
  else
    NULL
}