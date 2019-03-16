#' @title Get variance components from random effects models
#' @name get_variances
#'
#' @description This function extracts the different variance components of a
#'   mixed model and returns the result as list. Functions like
#'   \code{get_resid_variance(x)} or \code{get_fixef_variance(x)} are shortcuts
#'   for \code{get_variances(x, component = "resid")} etc.
#'
#' @param x A mixed effects model of class \code{merMod}, \code{glmmTMB} or
#'   \code{stanreg}.
#' @param component Character value, indicating the variance component that should
#'   be returned. May be one of \code{"all"}, \code{"fixef"}, \code{"ranef"},
#'   \code{"resid"}, \code{"dist"} or \code{"disp"}. By default, all variance
#'   components are returned. The distribution-specific (\code{"dist"}) and
#'   residual (\code{"resid"} variance are the most computational intensive
#'   components, and hence may take a few seconds to calculate.
#' @param ... Currently not used.
#'
#' @return A list with following elements:
#'    \itemize{
#'      \item \code{var.fixef}, variance attributable to the fixed effects
#'      \item \code{var.ranef}, (mean) variance of random effects
#'      \item \code{var.resid}, residual variance (sum of dispersion and distribution)
#'      \item \code{var.dist}, distribution-specific variance
#'      \item \code{var.disp}, variance due to additive dispersion
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
#' get_variances(m)
#' get_fixef_variance(m)
#' get_resid_variance(m)}
#'
#' @export
get_variances <- function(x, component = c("all", "fixef", "ranef", "resid", "dist", "disp"), ...) {
  UseMethod("get_variances")
}


#' @export
get_variances.default <- function(x, component = c("all", "fixef", "ranef", "resid", "dist", "disp"), ...) {
  warning(sprintf("Objects of class `%s` are not supported.", class(x)[1]))
  NULL
}


#' @export
get_variances.merMod <- function(x, component = c("all", "fixef", "ranef", "resid", "dist", "disp"), ...) {
  component <- match.arg(component)
  .compute_variances(x, component = component, name_fun = "get_variances", name_full = "random effect variances")
}


#' @export
get_variances.glmmTMB <- function(x, component = c("all", "fixef", "ranef", "resid", "dist", "disp"), ...) {
  component <- match.arg(component)
  .compute_variances(x, component = component, name_fun = "get_variances", name_full = "random effect variances")
}


#' @export
get_variances.stanreg <- function(x, component = c("all", "fixef", "ranef", "resid", "dist", "disp"), ...) {
  component <- match.arg(component)
  .compute_variances(x, component = component, name_fun = "get_variances", name_full = "random effect variances")
}


#' @rdname get_variances
#' @export
get_resid_variance <- function(x, ...) {
  unlist(get_variances(x, component = "resid", ...))
}

#' @rdname get_variances
#' @export
get_fixef_variance <- function(x, ...) {
  unlist(get_variances(x, component = "fixef", ...))
}

#' @rdname get_variances
#' @export
get_ranef_variance <- function(x, ...) {
  unlist(get_variances(x, component = "ranef", ...))
}

#' @rdname get_variances
#' @export
get_dist_variance <- function(x, ...) {
  unlist(get_variances(x, component = "dist", ...))
}

#' @rdname get_variances
#' @export
get_disp_variance <- function(x, ...) {
  unlist(get_variances(x, component = "disp", ...))
}
