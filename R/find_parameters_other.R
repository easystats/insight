#' @title Find model parameters from models with special components
#' @name find_parameters.averaging
#'
#' @description Returns the names of model parameters, like they typically
#'     appear in the \code{summary()} output.
#'
#' @param ... Currently not used.
#' @inheritParams find_parameters
#' @inheritParams find_parameters.betamfx
#' @inheritParams find_predictors
#'
#' @return A list of parameter names. The returned list may have following
#'   elements:
#'    \itemize{
#'      \item \code{conditional}, the "fixed effects" part from the model.
#'      \item \code{full}, parameters from the full model.
#'    }
#'
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
#' find_parameters(m)
#' @export
find_parameters.averaging <- function(x, component = c("conditional", "full"), flatten = FALSE, ...) {
  component <- match.arg(component)
  cf <- stats::coef(x, full = component == "full")
  out <- list(conditional = .remove_backticks_from_string(names(cf)))

  if (flatten) {
    unique(unlist(out))
  } else {
    out
  }
}



#' @rdname find_parameters.averaging
#' @export
find_parameters.betareg <- function(x, component = c("all", "conditional", "precision", "location", "distributional", "auxiliary"), flatten = FALSE, ...) {
  component <- match.arg(component)
  pars <- list(
    conditional = names(x$coefficients$mean),
    precision = names(x$coefficients$precision)
  )

  pars$conditional <- .remove_backticks_from_string(pars$conditional)
  .filter_parameters(pars, effects = "all", component = component, flatten = flatten, recursive = FALSE)
}



#' @rdname find_parameters.averaging
#' @export
find_parameters.DirichletRegModel <- function(x, component = c("all", "conditional", "precision", "location", "distributional", "auxiliary"), flatten = FALSE, ...) {
  component <- match.arg(component)
  if (x$parametrization == "common") {
    pars <- list(conditional = names(unlist(stats::coef(x))))
  } else {
    pars <- .compact_list(list(
      conditional = names(unlist(stats::coef(x)[["beta"]])),
      precision = names(unlist(stats::coef(x)[["gamma"]]))
    ))
    pars$precision <- .remove_backticks_from_string(pars$precision)
  }

  pars$conditional <- .remove_backticks_from_string(pars$conditional)
  .filter_parameters(pars, effects = "all", component = component, flatten = flatten, recursive = FALSE)
}
