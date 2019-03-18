#' @title Get variance components from random effects models
#' @name get_variance
#'
#' @description This function extracts the different variance components of a
#'   mixed model and returns the result as list. Functions like
#'   \code{get_variance_residual(x)} or \code{get_variance_fixed(x)} are shortcuts
#'   for \code{get_variance(x, component = "residual")} etc.
#'
#' @param x A mixed effects model of class \code{merMod}, \code{glmmTMB},
#'   \code{MixMod}, \code{lme}, or \code{stanreg}.
#' @param component Character value, indicating the variance component that should
#'   be returned. By default, all variance components are returned. The
#'   distribution-specific (\code{"distribution"}) and residual (\code{"residual"}
#'   variance are the most computational intensive components, and hence may
#'   take a few seconds to calculate.
#' @param null_model The null-model for \code{x}. For \code{MixMod}-objects,
#'   the null-model can't be calculated in a stable way using \code{update()},
#'   so in certain cases, it might be better to manually fit the null-model
#'   and pass it as argument.
#' @param ... Currently not used.
#'
#' @return A list with following elements:
#'    \itemize{
#'      \item \code{var.fixed}, variance attributable to the fixed effects
#'      \item \code{var.random}, (mean) variance of random effects
#'      \item \code{var.residual}, residual variance (sum of dispersion and distribution)
#'      \item \code{var.distribution}, distribution-specific variance
#'      \item \code{var.dispersion}, variance due to additive dispersion
#'      \item \code{var.intercept}, the random-intercept-variance, or between-subject-variance (tau 00)
#'      \item \code{var.slope}, the random-slope-variance (tau 11)
#'      \item \code{cor.slope_intercept}, the random-slope-intercept-correlation (rho 01)
#'    }
#'
#' @details This function returns different variance components from mixed models,
#'   which are needed, for instance, to calculate r-squared measures or the
#'   intraclass-correlation coefficient (ICC). The distributional variance (or
#'   observation-level variance) is based on lognormal approximation,
#'   \code{log(1+var(x)/mu^2)} (see \cite{Nakagawa et al. 2017}. The random effect
#'   variances are actually \emph{mean} random effect variances, thus they
#'   reflect the "average" random effects variance for mixed models with random
#'   slopes or nested random effects (see \cite{Johnson et al. 2014}). Details
#'   about the calculation of the different variance components can be found
#'   in \cite{Johnson et al. 2014} and \cite{Nakagawa et al. 2017}.
#'
#' @note Support for objects of class \code{MixMod} (\pkg{GLMMadaptiv}) or
#'   \code{lme} (\pkg{nlme}) is experimental and may not work for every
#'   model.
#'
#' @references \itemize{
#'  \item Johnson, P. C. D. (2014). Extension of Nakagawa & Schielzeth’s R2 GLMM to random slopes models. Methods in Ecology and Evolution, 5(9), 944–946. \doi{10.1111/2041-210X.12225}
#'  \item Nakagawa, S., Johnson, P. C. D., & Schielzeth, H. (2017). The coefficient of determination R2 and intra-class correlation coefficient from generalized linear mixed-effects models revisited and expanded. Journal of The Royal Society Interface, 14(134), 20170213. \doi{10.1098/rsif.2017.0213}
#'  }
#'
#' @examples
#' \dontrun{
#' library(lme4)
#' data(sleepstudy)
#' m <- lmer(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)
#'
#' get_variance(m)
#' get_variance_fixed(m)
#' get_variance_residual(m)}
#'
#' @export
get_variance <- function(x, component = c("all", "fixed", "random", "residual", "distribution", "dispersion", "intercept", "slope", "rho01"), ...) {
  UseMethod("get_variance")
}


#' @export
get_variance.default <- function(x, component = c("all", "fixed", "random", "residual", "distribution", "dispersion", "intercept", "slope", "rho01"), ...) {
  warning(sprintf("Objects of class `%s` are not supported.", class(x)[1]))
  NULL
}


#' @export
get_variance.merMod <- function(x, component = c("all", "fixed", "random", "residual", "distribution", "dispersion", "intercept", "slope", "rho01"), ...) {
  component <- match.arg(component)
  .compute_variances(x, component = component, name_fun = "get_variance", name_full = "random effect variances")
}


#' @export
get_variance.glmmTMB <- function(x, component = c("all", "fixed", "random", "residual", "distribution", "dispersion", "intercept", "slope", "rho01"), ...) {
  component <- match.arg(component)
  .compute_variances(x, component = component, name_fun = "get_variance", name_full = "random effect variances")
}


#' @export
get_variance.stanreg <- function(x, component = c("all", "fixed", "random", "residual", "distribution", "dispersion", "intercept", "slope", "rho01"), ...) {
  component <- match.arg(component)
  .compute_variances(x, component = component, name_fun = "get_variance", name_full = "random effect variances")
}


#' @export
get_variance.MixMod <- function(x, component = c("all", "fixed", "random", "residual", "distribution", "dispersion", "intercept", "slope", "rho01"), null_model = NULL, ...) {
  component <- match.arg(component)
  .compute_variances(x, component = component, name_fun = "get_variance", name_full = "random effect variances", null_model = null_model)
}


#' @export
get_variance.lme <- function(x, component = c("all", "fixed", "random", "residual", "distribution", "dispersion", "intercept", "slope", "rho01"), ...) {
  component <- match.arg(component)
  .compute_variances(x, component = component, name_fun = "get_variance", name_full = "random effect variances")
}


#' @rdname get_variance
#' @export
get_variance_residual <- function(x, ...) {
  unlist(get_variance(x, component = "residual", ...))
}

#' @rdname get_variance
#' @export
get_variance_fixed <- function(x, ...) {
  unlist(get_variance(x, component = "fixed", ...))
}

#' @rdname get_variance
#' @export
get_variance_random <- function(x, ...) {
  unlist(get_variance(x, component = "random", ...))
}

#' @rdname get_variance
#' @export
get_variance_distribution <- function(x, ...) {
  unlist(get_variance(x, component = "distribution", ...))
}

#' @rdname get_variance
#' @export
get_variance_dispersion <- function(x, ...) {
  unlist(get_variance(x, component = "dispersion", ...))
}

#' @rdname get_variance
#' @export
get_variance_intercept <- function(x, ...) {
  unlist(get_variance(x, component = "intercept", ...))
}

#' @rdname get_variance
#' @export
get_variance_slope <- function(x, ...) {
  unlist(get_variance(x, component = "slope", ...))
}

#' @rdname get_variance
#' @export
get_correlation_slope_intercept <- function(x, ...) {
  unlist(get_variance(x, component = "rho01", ...))
}
