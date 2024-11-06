#' @title Find names of model parameters from generalized additive models
#' @name find_parameters.gamlss
#'
#' @description Returns the names of model parameters, like they typically
#'     appear in the `summary()` output.
#'
#' @param ... Currently not used.
#' @inheritParams find_parameters
#' @inheritParams find_parameters.betamfx
#' @inheritParams find_predictors
#'
#' @return A list of parameter names. The returned list may have following
#' elements:
#'
#' - `conditional`, the "fixed effects" part from the model.
#' - `smooth_terms`, the smooth parameters.
#'
#' @examplesIf requireNamespace("mgcv", quietly = TRUE)
#' data(mtcars)
#' m <- mgcv::gam(mpg ~ s(hp) + gear, data = mtcars)
#' find_parameters(m)
#' @export
find_parameters.gamlss <- function(x, flatten = FALSE, ...) {
  pars <- lapply(x$parameters, function(i) {
    text_remove_backticks(names(stats::na.omit(stats::coef(x, what = i))))
  })

  names(pars) <- x$parameters
  if ("mu" %in% names(pars)) names(pars)[1] <- "conditional"

  pars <- compact_list(pars)

  if (flatten) {
    unique(unlist(pars, use.names = FALSE))
  } else {
    pars
  }
}


#' @rdname find_parameters.gamlss
#' @export
find_parameters.gam <- function(x, component = "all", flatten = FALSE, ...) {
  pars <- list(conditional = names(stats::coef(x)))
  pars$conditional <- text_remove_backticks(pars$conditional)

  st <- summary(x)$s.table

  pars$conditional <- pars$conditional[.grep_non_smoothers(pars$conditional)]
  pars$smooth_terms <- row.names(st)

  pars <- compact_list(pars)

  component <- validate_argument(component, c("all", "conditional", "smooth_terms", "location"))
  elements <- .get_elements(effects = "all", component = component)
  pars <- compact_list(pars[elements])

  if (flatten) {
    unique(unlist(pars, use.names = FALSE))
  } else {
    pars
  }
}

#' @export
find_parameters.scam <- find_parameters.gam


#' @export
find_parameters.Gam <- function(x, component = "all", flatten = FALSE, ...) {
  pars <- names(stats::coef(x))
  component <- validate_argument(component, c("all", "conditional", "smooth_terms", "location"))

  l <- compact_list(list(
    conditional = pars[.grep_non_smoothers(pars)],
    smooth_terms = pars[.grep_smoothers(pars)]
  ))

  .filter_parameters(
    l,
    effects = "all",
    component = component,
    flatten = flatten,
    recursive = TRUE
  )
}

#' @export
find_parameters.vgam <- find_parameters.Gam


#' @export
find_parameters.gamm <- function(x, component = "all", flatten = FALSE, ...) {
  x <- x$gam
  class(x) <- c(class(x), c("glm", "lm"))
  component <- validate_argument(component, c("all", "conditional", "smooth_terms", "location"))

  l <- find_parameters.gam(x, component = component)

  if (flatten) {
    unique(unlist(l, use.names = FALSE))
  } else {
    l
  }
}


#' @export
find_parameters.cgam <- function(x, component = "all", flatten = FALSE, ...) {
  component <- validate_argument(component, c("all", "conditional", "smooth_terms", "location"))
  sc <- summary(x)

  estimates <- sc$coefficients
  smooth_terms <- sc$coefficients2

  l <- compact_list(list(
    conditional = rownames(estimates),
    smooth_terms = rownames(smooth_terms)
  ))

  l <- lapply(l, text_remove_backticks)

  component <- match.arg(component)
  elements <- .get_elements(effects = "all", component = component)
  l <- compact_list(l[elements])

  if (flatten) {
    unique(unlist(l, use.names = FALSE))
  } else {
    l
  }
}


#' @export
find_parameters.SemiParBIV <- function(x, flatten = FALSE, ...) {
  pars <- get_parameters(x)
  # make sure we preserve order for split()
  pars$Component <- factor(pars$Component, levels = unique(pars$Component))
  l <- lapply(split(pars, pars$Component), function(i) {
    as.vector(i$Parameter)
  })
  if (flatten) {
    unique(unlist(l, use.names = FALSE))
  } else {
    l
  }
}

#' @export
find_parameters.selection <- find_parameters.SemiParBIV


#' @export
find_parameters.rqss <- function(x, component = "all", flatten = FALSE, ...) {
  sc <- summary(x)

  pars <- list(
    conditional = rownames(sc$coef),
    smooth_terms = rownames(sc$qsstab)
  )

  pars$conditional <- text_remove_backticks(pars$conditional)
  pars$smooth_terms <- text_remove_backticks(pars$smooth_terms)

  component <- validate_argument(component, c("all", "conditional", "smooth_terms", "location"))
  elements <- .get_elements(effects = "all", component)
  pars <- compact_list(pars[elements])

  if (flatten) {
    unique(unlist(pars))
  } else {
    pars
  }
}
