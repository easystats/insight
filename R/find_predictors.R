#' @title Find names of model predictors
#' @name find_predictors
#'
#' @description to do...
#'
#' @param x A fitted model.
#' @param effects Should variables for fixed effects, random effects
#'    or both be returned? Only applies to mixed models. May be abbreviated.
#' @param component Should all predictor variables, predictor variables for the
#'    conditional model, the zero-inflated part of the model or the dispersion
#'    model be returned? Only applies to models with zero-inflated and/or
#'    dispersion formula. May be abbreviated.
#' @param flatten Logical, if \code{TRUE}, the name of model terms are returned
#'    as a single character, not as list.
#'
#' @return A list of character vectors that represent the name(s) of the
#'    predictor variables. Depending on the combination of the arguments
#'    \code{effects} and \code{component}, the returned list has following
#'    elements:
#'    \itemize{
#'      \item \code{conditional}, the "fixed effects" terms from the model
#'      \item \code{random}, the "random effects" terms from the model
#'      \item \code{zero_inflated}, the "fixed effects" terms from the zero-inflation component of the model
#'      \item \code{zero_inflated_random}, the "random effects" terms from the zero-inflation component of the model
#'      \item \code{dispersion}, the dispersion terms
#'    }
#'
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
#' find_predictors(m)
#'
#' @export
find_predictors <- function(x, effects = c("fixed", "random", "all"), component = c("all", "conditional", "zi", "zero_inflated", "dispersion"), flatten = FALSE) {
  effects <- match.arg(effects)
  component <- match.arg(component)

  f <- find_formula(x)
  elements <- c("conditional", "random", "zero_inflated", "zero_inflated_random", "dispersion")

  elements <- switch(
    effects,
    all = elements,
    fixed = elements[elements %in% c("conditional", "zero_inflated", "disperion")],
    random = elements[elements %in% c("random", "zero_inflated_random")]
  )

  elements <- switch(
    component,
    all = elements,
    conditional = elements[elements %in% c("conditional", "random")],
    zi = ,
    zero_inflated = elements[elements %in% c("zero_inflated", "zero_inflated_random")],
    dispersion = elements[elements == "dispersion"]
  )

  # filter formulas, depending on requested effects and components
  f <- f[names(f) %in% elements]

  # from conditional model, remove response
  if (obj_has_name(f, "conditional"))
    f[["conditional"]] <- f[["conditional"]][[3]]

  # if we have random effects, just return grouping variable, not random slopes
  if (obj_has_name(f, "random"))
    f[["random"]] <- get_group_factor(x, f[["random"]])

  # same for zi-random effects
  if (obj_has_name(f, "zero_inflated_random"))
    f[["zero_inflated_random"]] <- get_group_factor(x, f[["zero_inflated_random"]])

  # random effects are returned as list, so we need to unlist here
  l <- compact_list(lapply(names(f), function(i) {
    if (i %in% c("random", "zero_inflated_random"))
      unique(paste(unlist(f[[i]])))
    else if (is.numeric(f[[i]]))
      f[[i]]
    else
      unique(all.vars(f[[i]]))
  }))

  names(l) <- names(f)

  if (flatten)
    unique(unlist(l))
  else
    l
}
