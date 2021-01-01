#' @title Get model parameters from zero-inflated and hurdle models
#' @name get_parameters.zeroinfl
#'
#' @description Returns the coefficients from a model.
#'
#' @param verbose Toggle messages and warnings.
#' @param ... Currently not used.
#'
#' @inheritParams find_parameters
#' @inheritParams find_predictors
#'
#' @return For models with smooth terms or zero-inflation component, a data
#'   frame with three columns: the parameter names, the related point estimates
#'   and the component.
#'
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
#' get_parameters(m)
#' @importFrom stats coef
#' @export
get_parameters.zeroinfl <- function(x, component = c("all", "conditional", "zi", "zero_inflated"), ...) {
  component <- match.arg(component)
  .return_zeroinf_parms(x, component)
}

#' @export
get_parameters.hurdle <- get_parameters.zeroinfl

#' @export
get_parameters.zerotrunc <- get_parameters.default

#' @rdname get_parameters
#' @export
get_parameters.zcpglm <- function(x, component = c("all", "conditional", "zi", "zero_inflated"), ...) {
  component <- match.arg(component)
  cf <- stats::coef(x)

  cond <- data.frame(
    Parameter = names(cf$tweedie),
    Estimate = unname(cf$tweedie),
    Component = "conditional",
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  zi <- data.frame(
    Parameter = names(cf$zero),
    Estimate = unname(cf$zero),
    Component = "zero_inflated",
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  pars <- switch(
    component,
    all = rbind(cond, zi),
    conditional = cond,
    zi = ,
    zero_inflated = zi
  )

  if (component != "all") {
    pars <- .remove_column(pars, "Component")
  }

  .remove_backticks_from_parameter_names(pars)
}





# helper -------------------

.return_zeroinf_parms <- function(x, component) {
  cf <- stats::coef(x)

  conditional <- grepl("^count_", names(cf), perl = TRUE)
  zero_inflated <- grepl("^zero_", names(cf), perl = TRUE)

  cond <- data.frame(
    Parameter = names(cf)[conditional],
    Estimate = unname(cf)[conditional],
    Component = "conditional",
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  zi <- data.frame(
    Parameter = names(cf)[zero_inflated],
    Estimate = unname(cf)[zero_inflated],
    Component = "zero_inflated",
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  pars <- switch(
    component,
    all = rbind(cond, zi),
    conditional = cond,
    zi = ,
    zero_inflated = zi
  )

  if (component != "all") {
    pars <- .remove_column(pars, "Component")
  }

  .remove_backticks_from_parameter_names(pars)
}
