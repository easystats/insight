#' @title Find model parameters from models with special components
#' @name find_parameters.averaging
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
#'   elements:
#'    \itemize{
#'      \item `conditional`, the "fixed effects" part from the model.
#'      \item `full`, parameters from the full model.
#'    }
#'
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
#' find_parameters(m)
#' @export
find_parameters.averaging <- function(x,
                                      component = c("conditional", "full"),
                                      flatten = FALSE,
                                      ...) {
  component <- match.arg(component)
  cf <- stats::coef(x, full = component == "full")
  out <- list(conditional = text_remove_backticks(names(cf)))

  if (flatten) {
    unique(unlist(out))
  } else {
    out
  }
}



#' @rdname find_parameters.averaging
#' @export
find_parameters.betareg <- function(x,
                                    component = c("all", "conditional", "precision", "location", "distributional", "auxiliary"),
                                    flatten = FALSE,
                                    ...) {
  component <- match.arg(component)
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



#' @rdname find_parameters.averaging
#' @export
find_parameters.DirichletRegModel <- function(x,
                                              component = c("all", "conditional", "precision", "location", "distributional", "auxiliary"),
                                              flatten = FALSE,
                                              ...) {
  component <- match.arg(component)
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



#' @rdname find_parameters.averaging
#' @export
find_parameters.mjoint <- function(x,
                                   component = c("all", "conditional", "survival"),
                                   flatten = FALSE,
                                   ...) {
  component <- match.arg(component)
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



#' @rdname find_parameters.averaging
#' @export
find_parameters.glmx <- function(x,
                                 component = c("all", "conditional", "extra"),
                                 flatten = FALSE,
                                 ...) {
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
  f <- find_formula(x)

  system_names <- names(f)

  out <- lapply(system_names, function(i) {
    pattern <- paste0("^", i, "_(.*)")
    params <- grepl(pattern, names(cf))
    gsub(pattern, "\\1", names(cf)[params])
  })

  names(out) <- system_names

  if (flatten) {
    unique(unlist(out))
  } else {
    out
  }
}



#' @export
find_parameters.bfsl <- function(x, flatten = FALSE, ...) {
  cf <- stats::coef(x)
  out <- list(conditional = rownames(cf))

  if (flatten) {
    unique(unlist(out))
  } else {
    out
  }
}


#' @export
find_parameters.marginaleffects <- function(x, flatten = FALSE, ...) {
  # Recover dataframe
  params <- x[!names(x) %in% c("rowid", "type", "std.error", "contrast", "term", "dydx")]
  # Remove fixed variables
  params <- params[sapply(params, function(x) length(unique(x)) > 1)]
  # Transform to list
  out <- list(marginaleffects = names(params))

  if (flatten) {
    unique(unlist(out))
  } else {
    out
  }
}


#' @export
find_parameters.deltaMethod <- function(x, flatten = FALSE, ...) {
  params <- standardize_names(x)
  out <- list(conditional = rownames(params))

  if (flatten) {
    unique(unlist(out))
  } else {
    out
  }
}
