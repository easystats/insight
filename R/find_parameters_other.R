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
  out <- list(conditional = .remove_backticks_from_string(names(cf)))

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

  pars$conditional <- .remove_backticks_from_string(pars$conditional)
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
    pars <- .compact_list(list(
      conditional = names(unlist(stats::coef(x)[["beta"]])),
      precision = names(unlist(stats::coef(x)[["gamma"]]))
    ))
    pars$precision <- .remove_backticks_from_string(pars$precision)
  }

  pars$conditional <- .remove_backticks_from_string(pars$conditional)

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
    conditional = .remove_backticks_from_string(rownames(s$coefs.long)),
    survival = .remove_backticks_from_string(rownames(s$coefs.surv))
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
    conditional = .remove_backticks_from_string(names(cf$glm[, 1])),
    extra = .remove_backticks_from_string(rownames(cf$extra))
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
find_parameters.bsfl <- function(x, flatten = FALSE, ...) {
  cf <- stats::coef(x)
  out <- list(conditional = rownames(cf))

  if (flatten) {
    unique(unlist(out))
  } else {
    out
  }
}
