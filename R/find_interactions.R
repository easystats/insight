#' @title Find interaction terms from models
#' @name find_interactions
#'
#' @description Returns all lowest to highest order interaction terms from a model.
#'
#' @inheritParams find_predictors
#' @inheritSection find_predictors Model components
#'
#' @return A list of character vectors that represent the interaction terms.
#'  Depending on `component`, the returned list has following
#'  elements (or `NULL`, if model has no interaction term):
#'
#' - `conditional`, interaction terms that belong to the "fixed
#'   effects" terms from the model
#' - `zero_inflated`, interaction terms that belong to the "fixed
#'   effects" terms from the zero-inflation component of the model
#' - `instruments`, for fixed-effects regressions like `ivreg`,
#'   `felm` or `plm`, interaction terms that belong to the
#'    instrumental variables
#'
#' @examples
#' data(mtcars)
#'
#' m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
#' find_interactions(m)
#'
#' m <- lm(mpg ~ wt * cyl + vs * hp * gear + carb, data = mtcars)
#' find_interactions(m)
#' @export
find_interactions <- function(x, component = "all", flatten = FALSE) {
  component <- validate_argument(
    component,
    c("all", "conditional", "zi", "zero_inflated", "dispersion", "instruments")
  )

  .find_interactions(x,
    effects = "fixed",
    component,
    flatten,
    main_effects = FALSE
  )
}


.find_interactions <- function(x,
                               effects = "fixed",
                               component,
                               flatten,
                               main_effects = FALSE) {
  f <- find_formula(x, verbose = FALSE)
  is_mv <- is_multivariate(f)
  elements <- .get_elements(effects = effects, component = component)

  if (is_mv) {
    l <- lapply(f, function(.x) compact_list(lapply(.x[elements], .get_interaction_terms, main_effects)))
  } else {
    l <- compact_list(lapply(f[elements], .get_interaction_terms, main_effects))
  }

  if (is_empty_object(l)) {
    return(NULL)
  }

  if (flatten) {
    unique(unlist(l, use.names = FALSE))
  } else {
    l
  }
}


.get_interaction_terms <- function(f, main_effects = FALSE) {
  if (is.null(f)) {
    return(NULL)
  }
  model_terms <- labels(stats::terms(f))
  if (main_effects) {
    model_terms
  } else {
    interaction_terms <- grepl(":", model_terms, fixed = TRUE)
    if (any(interaction_terms)) {
      model_terms[interaction_terms]
    } else {
      NULL
    }
  }
}
