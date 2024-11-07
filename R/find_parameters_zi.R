#' @title Find names of model parameters from zero-inflated models
#' @name find_parameters.zeroinfl
#'
#' @description Returns the names of model parameters, like they typically
#'     appear in the `summary()` output.
#'
#' @param ... Currently not used.
#' @inheritParams find_parameters
#' @inheritParams find_parameters.betamfx
#' @inheritParams find_predictors
#'
#' @inheritSection find_predictors Model components
#'
#' @return A list of parameter names. The returned list may have following
#' elements:
#'
#' - `conditional`, the "fixed effects" part from the model.
#' - `zero_inflated`, the "fixed effects" part from the zero-inflation
#'   component of the model.
#' - Special models are `mhurdle`, which also can have the components
#'   `infrequent_purchase`, `ip`, and `auxiliary`.
#'
#' @examplesIf requireNamespace("pscl", quietly = TRUE)
#' data(bioChemists, package = "pscl")
#' m <- pscl::zeroinfl(
#'   art ~ fem + mar + kid5 + ment | kid5 + phd,
#'   data = bioChemists
#' )
#' find_parameters(m)
#' @export
find_parameters.zeroinfl <- function(x, component = "all", flatten = FALSE, ...) {
  cf <- names(stats::coef(x))
  component <- validate_argument(component, c("all", "conditional", "zi", "zero_inflated"))

  l <- compact_list(list(
    conditional = cf[startsWith(cf, "count_")],
    zero_inflated = cf[startsWith(cf, "zero_")]
  ))

  .filter_parameters(
    l,
    effects = "all",
    component = component,
    flatten = flatten,
    recursive = FALSE
  )
}

#' @export
find_parameters.hurdle <- find_parameters.zeroinfl

#' @export
find_parameters.zerotrunc <- find_parameters.zeroinfl


#' @export
find_parameters.zcpglm <- function(x, component = "all", flatten = FALSE, ...) {
  cf <- stats::coef(x)
  component <- validate_argument(component, c("all", "conditional", "zi", "zero_inflated"))

  l <- compact_list(list(
    conditional = names(cf$tweedie),
    zero_inflated = names(cf$zero)
  ))

  .filter_parameters(
    l,
    effects = "all",
    component = component,
    flatten = flatten,
    recursive = FALSE
  )
}


#' @export
find_parameters.mhurdle <- function(x, component = "all", flatten = FALSE, ...) {
  component <- validate_argument(
    component,
    c("all", "conditional", "zi", "zero_inflated", "infrequent_purchase", "ip", "auxiliary", "distributional")
  )
  cf <- stats::coef(x)

  # handle alias
  if (component == "distributional") {
    component <- "auxiliary"
  }

  cond_pars <- which(startsWith(names(cf), "h2."))
  zi_pars <- which(startsWith(names(cf), "h1."))
  ip_pars <- which(startsWith(names(cf), "h3."))
  aux_pars <- (seq_along(names(cf)))[-c(cond_pars, zi_pars, ip_pars)]

  # names(cf) <- gsub("^(h1|h2|h3)\\.(.*)", "\\2", names(cf))

  l <- compact_list(list(
    conditional = names(cf)[cond_pars],
    zero_inflated = names(cf)[zi_pars],
    infrequent_purchase = names(cf)[ip_pars],
    auxiliary = names(cf)[aux_pars]
  ))

  .filter_parameters(
    l,
    effects = "all",
    component = component,
    flatten = flatten,
    recursive = FALSE
  )
}
