#' @title Find names of model parameters from marginal effects models
#' @name find_parameters.betamfx
#'
#' @description Returns the names of model parameters, like they typically
#' appear in the `summary()` output.
#'
#' @param ... Currently not used.
#' @inheritParams find_predictors
#' @inheritParams find_parameters
#'
#' @inheritSection find_predictors Model components
#'
#' @return A list of parameter names. The returned list may have following
#' elements:
#'
#' - `conditional`, the "fixed effects" part from the model.
#' - `marginal`, the marginal effects.
#' - `precision`, the precision parameter.
#'
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
#' find_parameters(m)
#' @export
find_parameters.betamfx <- function(x, component = "all", flatten = FALSE, ...) {
  pars <- list(
    marginal = text_remove_backticks(rownames(x$mfxest)),
    conditional = text_remove_backticks(names(x$fit$coefficients$mean)),
    precision = text_remove_backticks(names(x$fit$coefficients$precision))
  )

  component <- validate_argument(
    component,
    c(
      "all", "conditional", "precision", "marginal", "location",
      "distributional", "auxiliary"
    )
  )
  elements <- .get_elements(effects = "all", component = component)
  pars <- compact_list(pars[elements])

  if (flatten) {
    unique(unlist(pars, use.names = FALSE))
  } else {
    pars
  }
}


#' @export
find_parameters.betaor <- function(x, component = "all", flatten = FALSE, ...) {
  pars <- list(
    conditional = text_remove_backticks(names(x$fit$coefficients$mean)),
    precision = text_remove_backticks(names(x$fit$coefficients$precision))
  )

  component <- validate_argument(
    component,
    c("all", "conditional", "precision", "location", "distributional", "auxiliary")
  )
  elements <- .get_elements(effects = "all", component = component)
  pars <- compact_list(pars[elements])

  if (flatten) {
    unique(unlist(pars, use.names = FALSE))
  } else {
    pars
  }
}


#' @export
find_parameters.logitmfx <- function(x, component = "all", flatten = FALSE, ...) {
  p <- text_remove_backticks(names(stats::coef(x$fit)))
  pars <- list(marginal = text_remove_backticks(rownames(x$mfxest)), conditional = p)

  component <- validate_argument(component, c("all", "conditional", "marginal", "location"))
  elements <- .get_elements(effects = "all", component = component)
  pars <- compact_list(pars[elements])

  if (flatten) {
    unique(unlist(pars, use.names = FALSE))
  } else {
    pars
  }
}

#' @export
find_parameters.poissonmfx <- find_parameters.logitmfx

#' @export
find_parameters.negbinmfx <- find_parameters.logitmfx

#' @export
find_parameters.probitmfx <- find_parameters.logitmfx


#' @export
find_parameters.logitor <- function(x, flatten = FALSE, ...) {
  pars <- list(conditional = text_remove_backticks(names(stats::coef(x$fit))))

  if (flatten) {
    unique(unlist(pars, use.names = FALSE))
  } else {
    pars
  }
}

#' @export
find_parameters.poissonirr <- find_parameters.logitor

#' @export
find_parameters.negbinirr <- find_parameters.logitor
