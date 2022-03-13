#' @title Find names of model parameters from marginal effects models
#' @name find_parameters.betamfx
#'
#' @description Returns the names of model parameters, like they typically
#'     appear in the `summary()` output.
#'
#' @param component Which type of parameters to return, such as parameters for the
#'   conditional model, the zero-inflated part of the model, the dispersion
#'   term, the instrumental variables or marginal effects be returned? Applies
#'   to models with zero-inflated and/or dispersion formula, or to models with
#'   instrumental variables (so called fixed-effects regressions), or models
#'   with marginal effects from \pkg{mfx}. May be abbreviated. Note that the
#'   *conditional* component is also called *count* or *mean*
#'   component, depending on the model. There are three convenient shortcuts:
#'   `component = "all"` returns all possible parameters.
#'   If `component = "location"`, location parameters such as `conditional`,
#'   `zero_inflated`, `smooth_terms`, or `instruments` are returned
#'   (everything that are fixed or random effects - depending on the `effects`
#'   argument - but no auxiliary parameters). For `component = "distributional"`
#'   (or `"auxiliary"`), components like `sigma`, `dispersion`,
#'   `beta` or `precision` (and other auxiliary parameters) are returned.
#' @param ... Currently not used.
#' @inheritParams find_parameters
#' @inheritParams find_predictors
#'
#' @return A list of parameter names. The returned list may have following
#' elements:
#'    \itemize{
#'      \item `conditional`, the "fixed effects" part from the model.
#'      \item `marginal`, the marginal effects.
#'      \item `precision`, the precision parameter.
#'    }
#'
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
#' find_parameters(m)
#' @export
find_parameters.betamfx <- function(x, component = c("all", "conditional", "precision", "marginal", "location", "distributional", "auxiliary"), flatten = FALSE, ...) {
  pars <- list(
    marginal = text_remove_backticks(rownames(x$mfxest)),
    conditional = text_remove_backticks(names(x$fit$coefficients$mean)),
    precision = text_remove_backticks(names(x$fit$coefficients$precision))
  )

  component <- match.arg(component)
  elements <- .get_elements(effects = "all", component = component)
  pars <- compact_list(pars[elements])

  if (flatten) {
    unique(unlist(pars))
  } else {
    pars
  }
}



#' @export
find_parameters.betaor <- function(x, component = c("all", "conditional", "precision", "location", "distributional", "auxiliary"), flatten = FALSE, ...) {
  pars <- list(
    conditional = text_remove_backticks(names(x$fit$coefficients$mean)),
    precision = text_remove_backticks(names(x$fit$coefficients$precision))
  )

  component <- match.arg(component)
  elements <- .get_elements(effects = "all", component = component)
  pars <- compact_list(pars[elements])

  if (flatten) {
    unique(unlist(pars))
  } else {
    pars
  }
}


#' @rdname find_parameters.betamfx
#' @export
find_parameters.logitmfx <- function(x, component = c("all", "conditional", "marginal", "location"), flatten = FALSE, ...) {
  p <- text_remove_backticks(names(stats::coef(x$fit)))
  pars <- list(marginal = text_remove_backticks(rownames(x$mfxest)), conditional = p)

  component <- match.arg(component)
  elements <- .get_elements(effects = "all", component = component)
  pars <- compact_list(pars[elements])

  if (flatten) {
    unique(unlist(pars))
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
    unique(unlist(pars))
  } else {
    pars
  }
}

#' @export
find_parameters.poissonirr <- find_parameters.logitor

#' @export
find_parameters.negbinirr <- find_parameters.logitor
