#' @title Find model parameters from models with special components
#' @name find_parameters.averaging
#'
#' @description Returns the names of model parameters, like they typically
#' appear in the `summary()` output.
#'
#' @param ... Currently not used.
#' @inheritParams find_parameters
#' @inheritParams find_parameters.betamfx
#' @inheritParams find_predictors
#'
#' @inheritSection find_predictors Model components
#'
#' @return A list of parameter names. The returned list may have following
#' elements, usually requested via the `component` argument:
#'
#' - `conditional`, the "fixed effects" part from the model.
#' - `full`, parameters from the full model.
#' - `precision` for models of class `betareg`.
#' - `survival` for model of class `mjoint`.
#' - `extra` for models of class `glmx`.
#'
#' @examplesIf requireNamespace("betareg", quietly = TRUE)
#' data("GasolineYield", package = "betareg")
#' m <- betareg::betareg(yield ~ batch + temp, data = GasolineYield)
#' find_parameters(m)
#' find_parameters(m, component = "precision")
#' @export
find_parameters.averaging <- function(x, component = "conditional", flatten = FALSE, ...) {
  component <- validate_argument(component, c("conditional", "full"))
  cf <- stats::coef(x, full = component == "full")
  out <- list(conditional = text_remove_backticks(names(cf)))

  if (flatten) {
    unique(unlist(out, use.names = FALSE))
  } else {
    out
  }
}


#' @export
find_parameters.glmgee <- function(x, component = "all", flatten = FALSE, ...) {
  component <- validate_argument(component, c("all", "conditional", "dispersion"))

  junk <- utils::capture.output({
    cs <- suppressWarnings(stats::coef(summary(x, corr = FALSE)))
  })
  params <- compact_character(rownames(cs))

  out <- list(
    conditional = text_remove_backticks(params[params != "Dispersion"]),
    dispersion = text_remove_backticks(params[params == "Dispersion"])
  )

  .filter_parameters(
    out,
    effects = "all",
    component = component,
    flatten = flatten,
    recursive = FALSE
  )
}


#' @export
find_parameters.betareg <- function(x, component = "all", flatten = FALSE, ...) {
  component <- validate_argument(
    component,
    c("all", "conditional", "precision", "location", "distributional", "auxiliary")
  )

  pars <- list(
    conditional = names(x$coefficients$mean),
    precision = names(x$coefficients$precision)
  )

  pars$conditional <- text_remove_backticks(pars$conditional)
  .filter_parameters(
    pars,
    effects = "all",
    component = component,
    flatten = flatten,
    recursive = FALSE
  )
}


#' @export
find_parameters.DirichletRegModel <- function(x, component = "all", flatten = FALSE, ...) {
  component <- validate_argument(
    component,
    c("all", "conditional", "precision", "location", "distributional", "auxiliary")
  )
  if (x$parametrization == "common") {
    pars <- list(conditional = names(unlist(stats::coef(x))))
  } else {
    pars <- compact_list(list(
      conditional = names(unlist(stats::coef(x)[["beta"]])),
      precision = names(unlist(stats::coef(x)[["gamma"]]))
    ))
    pars$precision <- text_remove_backticks(pars$precision)
  }

  pars$conditional <- text_remove_backticks(pars$conditional)

  .filter_parameters(
    pars,
    effects = "all",
    component = component,
    flatten = flatten,
    recursive = FALSE
  )
}


#' @export
find_parameters.mjoint <- function(x, component = "all", flatten = FALSE, ...) {
  component <- validate_argument(component, c("all", "conditional", "survival"))
  s <- summary(x)

  out <- list(
    conditional = text_remove_backticks(rownames(s$coefs.long)),
    survival = text_remove_backticks(rownames(s$coefs.surv))
  )

  .filter_parameters(
    out,
    effects = "all",
    component = component,
    flatten = flatten,
    recursive = FALSE
  )
}


#' @export
find_parameters.glmx <- function(x, component = "all", flatten = FALSE, ...) {
  component <- validate_argument(component, c("all", "conditional", "extra"))
  cf <- stats::coef(summary(x))

  out <- list(
    conditional = text_remove_backticks(names(cf$glm[, 1])),
    extra = text_remove_backticks(rownames(cf$extra))
  )

  .filter_parameters(
    out,
    effects = "all",
    component = component,
    flatten = flatten,
    recursive = FALSE
  )
}


#' @export
find_parameters.model_fit <- function(x, flatten = FALSE, ...) {
  find_parameters(x$fit, flatten = flatten, ...)
}


#' @export
find_parameters.systemfit <- function(x, flatten = FALSE, ...) {
  cf <- stats::coef(x)
  f <- find_formula(x, verbose = FALSE)

  system_names <- names(f)

  out <- lapply(system_names, function(i) {
    pattern <- paste0("^", i, "_(.*)")
    params <- grepl(pattern, names(cf))
    gsub(pattern, "\\1", names(cf)[params])
  })

  names(out) <- system_names

  if (flatten) {
    unique(unlist(out, use.names = FALSE))
  } else {
    out
  }
}


#' @export
find_parameters.bfsl <- function(x, flatten = FALSE, ...) {
  cf <- stats::coef(x)
  out <- list(conditional = rownames(cf))

  if (flatten) {
    unique(unlist(out, use.names = FALSE))
  } else {
    out
  }
}


#' @export
find_parameters.marginaleffects <- function(x, flatten = FALSE, ...) {
  # Recover dataframe
  excl <- c(
    "rowid", "type", "estimate", "std.error", "contrast", "term", "dydx",
    "statistic", "p.value", "s.value", "conf.low", "conf.high", "predicted_hi",
    "predicted_lo", "predicted", "eps", "marginaleffects_eps"
  )

  params <- x[!names(x) %in% excl]

  # Remove fixed variables
  params <- params[vapply(params, function(x) length(unique(x)) > 1, TRUE)]
  # Transform to list
  out <- list(marginaleffects = names(params))

  if (flatten) {
    unique(unlist(out, use.names = FALSE))
  } else {
    out
  }
}


#' @export
find_parameters.marginaleffects.summary <- function(x, flatten = FALSE, ...) {
  out <- list(marginaleffects = x$term)

  if (flatten) {
    unique(unlist(out, use.names = FALSE))
  } else {
    out
  }
}


#' @export
find_parameters.deltaMethod <- function(x, flatten = FALSE, ...) {
  params <- standardize_names(x)
  out <- list(conditional = rownames(params))

  if (flatten) {
    unique(unlist(out, use.names = FALSE))
  } else {
    out
  }
}


#' @export
find_parameters.coxph <- function(x, flatten = FALSE, ...) {
  cf <- stats::coef(summary(x))
  out <- list(conditional = rownames(cf))

  if (flatten) {
    unique(unlist(out, use.names = FALSE))
  } else {
    out
  }
}


#' @export
find_parameters.asym <- function(x, flatten = FALSE, ...) {
  cf <- stats::coef(x)

  params <- names(cf)
  params <- gsub("^plus__", "+", params)
  params <- gsub("^minus__", "-", params)

  out <- list(conditional = params)

  if (flatten) {
    unique(unlist(out, use.names = FALSE))
  } else {
    out
  }
}


#' @export
find_parameters.oohbchoice <- function(x, flatten = FALSE, ...) {
  out <- list(conditional = names(stats::coef(x)))

  if (flatten) {
    unique(unlist(out, use.names = FALSE))
  } else {
    out
  }
}
