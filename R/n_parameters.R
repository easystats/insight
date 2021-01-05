#' Count number of parameters in a model
#'
#' Returns the number of parameters (coefficients) of a model.
#'
#' @param x A statistical model.
#' @param effects Should number of parameters for fixed effects, random effects
#'    or both be returned? Only applies to mixed models. May be abbreviated.
#' @param component Should total number of parameters, number parameters for the
#'    conditional model, the zero-inflated part of the model, the dispersion
#'    term or the instrumental variables be returned? Applies to models
#'    with zero-inflated and/or dispersion formula, or to models with instrumental
#'    variable (so called fixed-effects regressions). May be abbreviated.
#' @param ... Arguments passed to or from other methods.
#'
#' @return The number of parameters in the model.
#'
#' @note This function returns the number of parameters for the fixed effects
#' by default, as returned by \code{find_parameters(x, effects = "fixed")}.
#' It does not include \emph{all} estimated model parameters, i.e. auxiliary
#' parameters like sigma or dispersion are not counted. To get the number of
#' \emph{all estimated} parameters, use \code{get_df(x, type = "model")}.
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
n_parameters.default <- function(x, ...) {
  length(find_parameters(x, effects = "fixed", flatten = TRUE, ...))
}




# Models with random effects -------------------------------------

#' @rdname n_parameters
#' @export
n_parameters.merMod <- function(x, effects = c("fixed", "random"), ...) {
  effects <- match.arg(effects)
  length(find_parameters(x, effects = effects, flatten = TRUE, ...))
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




# Models with random effects and other components ----------------------------

#' @export
n_parameters.MixMod <- function(x,
                                effects = c("fixed", "random"),
                                component = c("all", "conditional", "zi", "zero_inflated"),
                                ...) {
  effects <- match.arg(effects)
  component <- match.arg(component)
  length(find_parameters(x, effects = effects, component = component, flatten = TRUE, ...))
}

#' @rdname n_parameters
#' @export
n_parameters.glmmTMB <- n_parameters.MixMod





# Models with (zero-inflation) components ----------------------------

#' @rdname n_parameters
#' @export
n_parameters.zeroinfl <- function(x, component = c("all", "conditional", "zi", "zero_inflated"), ...) {
  component <- match.arg(component)
  length(find_parameters(x, component = component, flatten = TRUE, ...))
}

#' @export
n_parameters.hurdle <- n_parameters.zeroinfl

#' @export
n_parameters.zerotrunc <- n_parameters.default






# GAMs ----------------------------

#' @rdname n_parameters
#' @export
n_parameters.gam <- function(x, component = c("all", "conditional", "smooth_terms"), ...) {
  component <- match.arg(component)
  length(find_parameters(x, component = component, flatten = TRUE, ...))
}

#' @export
n_parameters.Gam <- n_parameters.gam

#' @export
n_parameters.vgam <- n_parameters.gam






# Bayesian Models ----------------------------

#' @rdname n_parameters
#' @export
n_parameters.brmsfit <- function(x,
                                 effects = c("all", "fixed", "random"),
                                 component = c("all", "conditional", "zi", "zero_inflated", "dispersion", "simplex", "sigma", "smooth_terms"),
                                 ...) {
  effects <- match.arg(effects)
  component <- match.arg(component)
  length(find_parameters(x, effects = effects, component = component, flatten = TRUE, ...))
}


#' @export
n_parameters.stanreg <- function(x,
                                 effects = c("all", "fixed", "random"),
                                 component = c("all", "conditional", "smooth_terms"),
                                 ...) {
  effects <- match.arg(effects)
  component <- match.arg(component)
  length(find_parameters(x, effects = effects, component = component, flatten = TRUE, ...))
}

#' @export
n_parameters.stanmvreg <- n_parameters.stanreg






# Other models -------------------------------------


#' @export
n_parameters.lavaan <- function(x, ...) {
  length(stats::coef(x))
  # if (!requireNamespace("lavaan", quietly = TRUE)) {
  #   stop("Package 'lavaan' required for this function to work. Please install it by running `install.packages('lavaan')`.")
  # }
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
  length(find_parameters(x, component = "conditional", flatten = TRUE, ...))
}
