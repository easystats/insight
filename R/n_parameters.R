#' Count number of parameters in a model
#'
#' Returns the number of parameters (coefficients) of a model.
#'
#' @param x A statistical model.
#' @param remove_nonestimable Logical, if `TRUE`, removes (i.e. does not
#'   count) non-estimable parameters (which may occur for models with
#'   rank-deficient model matrix).
#' @param ... Arguments passed to or from other methods.
#' @inheritParams find_predictors
#'
#' @return The number of parameters in the model.
#'
#' @note
#' This function returns the number of parameters for the fixed effects by
#' default, as returned by `find_parameters(x, effects = "fixed")`. It does not
#' include *all* estimated model parameters, i.e. auxiliary parameters like
#' sigma or dispersion are not counted. To get the number of *all estimated*
#' parameters, use `get_df(x, type = "model")`.
#'
#' @examples
#' data(iris)
#' model <- lm(Sepal.Length ~ Sepal.Width * Species, data = iris)
#' n_parameters(model)
#' @export
n_parameters <- function(x, ...) {
  UseMethod("n_parameters")
}


# Default models -------------------------------------

#' @rdname n_parameters
#' @export
n_parameters.default <- function(x, remove_nonestimable = FALSE, ...) {
  .n_parameters_effects(x,
    effects = "fixed",
    remove_nonestimable = remove_nonestimable,
    ...
  )
}


# helper
.process_estimable <- function(params, remove_nonestimable) {
  if (isTRUE(remove_nonestimable)) {
    params <- params[!is.na(params$Estimate), ]
  }

  nrow(params)
}


# Models with random effects -------------------------------------

#' @rdname n_parameters
#' @export
n_parameters.merMod <- function(x,
                                effects = "fixed",
                                remove_nonestimable = FALSE,
                                ...) {
  effects <- validate_argument(effects, c("fixed", "random"))

  .n_parameters_effects(x,
    effects = effects,
    remove_nonestimable = remove_nonestimable,
    ...
  )
}

#' @export
n_parameters.BBmm <- n_parameters.merMod

#' @export
n_parameters.glimML <- n_parameters.merMod

#' @export
n_parameters.cpglmm <- n_parameters.merMod

#' @export
n_parameters.rlmerMod <- n_parameters.merMod

#' @export
n_parameters.mixed <- n_parameters.merMod

#' @export
n_parameters.coxme <- n_parameters.merMod

#' @export
n_parameters.lme <- n_parameters.merMod

#' @export
n_parameters.MCMCglmm <- n_parameters.merMod

#' @export
n_parameters.sim.merMod <- n_parameters.merMod

#' @export
n_parameters.wbm <- n_parameters.merMod

#' @export
n_parameters.svy2lme <- n_parameters.merMod


# Models with random effects and other components ----------------------------

#' @export
n_parameters.MixMod <- function(x,
                                effects = "fixed",
                                component = "all",
                                remove_nonestimable = FALSE,
                                ...) {
  effects <- validate_argument(effects, c("fixed", "random"))
  component <- validate_argument(component, c("all", "conditional", "zi", "zero_inflated"))

  if (effects == "random" || isFALSE(remove_nonestimable)) {
    length(unlist(
      find_parameters(
        x,
        effects = effects,
        component = component,
        flatten = FALSE,
        verbose = FALSE,
        ...
      ),
      use.names = FALSE
    ))
  } else {
    params <- get_parameters(x, effects = effects, component = component, ...)
    .process_estimable(params, remove_nonestimable)
  }
}

#' @rdname n_parameters
#' @export
n_parameters.glmmTMB <- n_parameters.MixMod


# Models with (zero-inflation) components ----------------------------

#' @export
n_parameters.zeroinfl <- function(x,
                                  component = "all",
                                  remove_nonestimable = FALSE,
                                  ...) {
  component <- validate_argument(component, c("all", "conditional", "zi", "zero_inflated"))
  .n_parameters_component(x, component, remove_nonestimable, ...)
}

#' @export
n_parameters.hurdle <- n_parameters.zeroinfl

#' @export
n_parameters.zerotrunc <- n_parameters.default


# GAMs ----------------------------

#' @export
n_parameters.gam <- function(x,
                             component = "all",
                             remove_nonestimable = FALSE,
                             ...) {
  component <- validate_argument(component, c("all", "conditional", "smooth_terms"))
  .n_parameters_component(x, component, remove_nonestimable, ...)
}

#' @export
n_parameters.Gam <- n_parameters.gam

#' @export
n_parameters.vgam <- n_parameters.gam


# Bayesian Models ----------------------------

#' @export
n_parameters.brmsfit <- function(x,
                                 effects = "all",
                                 component = "all",
                                 ...) {
  effects <- validate_argument(effects, c("all", "fixed", "random"))
  component <- validate_argument(component, c("all", .all_elements()))

  length(unlist(
    find_parameters(
      x,
      effects = effects,
      component = component,
      flatten = FALSE,
      verbose = FALSE,
      ...
    ),
    use.names = FALSE
  ))
}


#' @export
n_parameters.stanreg <- function(x,
                                 effects = "all",
                                 component = "all",
                                 ...) {
  effects <- validate_argument(effects, c("all", "fixed", "random"))
  component <- validate_argument(component, c("all", "conditional", "smooth_terms"))

  length(unlist(
    find_parameters(
      x,
      effects = effects,
      component = component,
      flatten = FALSE,
      verbose = FALSE,
      ...
    ),
    use.names = FALSE
  ))
}

#' @export
n_parameters.stanmvreg <- n_parameters.stanreg


# Other models -------------------------------------


#' @export
n_parameters.gls <- function(x, ...) {
  x$dims[["p"]]
}


#' @export
n_parameters.logitr <- function(x, effects = "all", ...) {
  effects <- validate_argument(effects, c("all", "fixed", "random"))
  switch(effects,
    fixed = x$n$parsFixed,
    random = x$n$parsRandom,
    x$n$parsFixed + x$n$parsRandom
  )
}


#' @export
n_parameters.lavaan <- function(x, ...) {
  # TODO

  # check_if_installed("lavaan")
  # lavaan::fitmeasures(x)[["npar"]]
}


#' @export
n_parameters.blavaan <- n_parameters.lavaan


#' @export
n_parameters.multinom <- function(x, ...) {
  nrow(get_parameters(x))
}


#' @export
n_parameters.bayesx <- function(x, ...) {
  length(unlist(
    find_parameters(
      x,
      component = "conditional",
      flatten = FALSE,
      verbose = FALSE,
      ...
    ),
    use.names = FALSE
  ))
}


# helper ---------------------

.n_parameters_component <- function(x, component, remove_nonestimable, ...) {
  if (isTRUE(remove_nonestimable)) {
    params <- get_parameters(x, component = component, ...)
    .process_estimable(params, remove_nonestimable)
  } else {
    length(unlist(
      find_parameters(
        x,
        component = component,
        flatten = FALSE,
        verbose = FALSE,
        ...
      ),
      use.names = FALSE
    ))
  }
}


.n_parameters_effects <- function(x, effects, remove_nonestimable, ...) {
  if (effects == "random" || isFALSE(remove_nonestimable)) {
    length(unlist(
      find_parameters(
        x,
        effects = effects,
        flatten = FALSE,
        verbose = FALSE,
        ...
      ),
      use.names = FALSE
    ))
  } else {
    params <- get_parameters(x, effects = effects, ...)
    .process_estimable(params, remove_nonestimable)
  }
}
