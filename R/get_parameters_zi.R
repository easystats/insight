#' @title Get model parameters from zero-inflated and hurdle models
#' @name get_parameters.zeroinfl
#'
#' @description Returns the coefficients from a model.
#'
#' @param ... Currently not used.
#'
#' @inheritParams find_parameters
#' @inheritParams find_predictors
#'
#' @inheritSection find_predictors Model components
#'
#' @return For models with smooth terms or zero-inflation component, a data
#'   frame with three columns: the parameter names, the related point estimates
#'   and the component.
#'
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
#' get_parameters(m)
#' @export
get_parameters.zeroinfl <- function(x, component = "all", ...) {
  component <- validate_argument(component, c("all", "conditional", "zi", "zero_inflated"))
  .return_zeroinf_parms(x, component)
}

#' @export
get_parameters.hurdle <- get_parameters.zeroinfl

#' @export
get_parameters.zerotrunc <- get_parameters.default


#' @export
get_parameters.zcpglm <- function(x, component = "all", ...) {
  component <- validate_argument(component, c("all", "conditional", "zi", "zero_inflated"))
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

  pars <- switch(component,
    all = rbind(cond, zi),
    conditional = cond,
    zi = ,
    zero_inflated = zi
  )

  if (component != "all") {
    pars <- .remove_column(pars, "Component")
  }

  text_remove_backticks(pars)
}


# helper -------------------

.return_zeroinf_parms <- function(x, component) {
  cf <- stats::coef(x)

  conditional <- startsWith(names(cf), "count_")
  zero_inflated <- startsWith(names(cf), "zero_")

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

  pars <- switch(component,
    all = rbind(cond, zi),
    conditional = cond,
    zi = ,
    zero_inflated = zi
  )

  if (component != "all") {
    pars <- .remove_column(pars, "Component")
  }

  text_remove_backticks(pars)
}


#' @export
get_parameters.mhurdle <- function(x, component = "all", ...) {
  component <- validate_argument(
    component,
    c(
      "all", "conditional", "zi", "zero_inflated",
      "infrequent_purchase", "ip", "auxiliary"
    )
  )
  cf <- stats::coef(x)

  cond_pars <- which(startsWith(names(cf), "h2."))
  zi_pars <- which(startsWith(names(cf), "h1."))
  ip_pars <- which(startsWith(names(cf), "h3."))
  aux_pars <- (seq_along(names(cf)))[-c(cond_pars, zi_pars, ip_pars)]

  if (length(cond_pars)) {
    cond_dat <- data.frame(
      Parameter = names(cf)[cond_pars],
      Estimate = unname(cf[cond_pars]),
      Component = "conditional",
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  } else {
    cond_dat <- NULL
  }

  if (length(zi_pars)) {
    zi_dat <- data.frame(
      Parameter = names(cf)[zi_pars],
      Estimate = unname(cf[zi_pars]),
      Component = "zero_inflated",
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  } else {
    zi_dat <- NULL
  }

  if (length(ip_pars)) {
    ip_dat <- data.frame(
      Parameter = names(cf)[ip_pars],
      Estimate = unname(cf[ip_pars]),
      Component = "infrequent_purchase",
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  } else {
    ip_dat <- NULL
  }

  if (length(aux_pars)) {
    aux_dat <- data.frame(
      Parameter = names(cf)[aux_pars],
      Estimate = unname(cf[aux_pars]),
      Component = "auxiliary",
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  } else {
    aux_dat <- NULL
  }

  pars <- rbind(cond_dat, zi_dat, ip_dat, aux_dat)
  pars <- .filter_component(pars, component)

  if (component != "all") {
    pars <- .remove_column(pars, "Component")
  }

  # pars$Parameter <- gsub("^(h1|h2|h3)\\.(.*)", "\\2", pars$Parameter)
  text_remove_backticks(pars)
}
