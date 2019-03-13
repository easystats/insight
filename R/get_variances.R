#' @title Get variance components from random effects models
#' @name get_variances
#'
#' @description This function extracts the different variance components of a
#'   mixed model and returns the result as list.
#'
#' @param x A mixed effects model of class \code{merMod}, \code{glmmTMB} or
#'   \code{stanreg}.
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
#'  \item Johnson PC, O'Hara RB. 2014. Extension of Nakagawa & Schielzeth's R2GLMM to random slopes models. Methods Ecol Evol, 5: 944-946. (\doi{10.1111/2041-210X.12225})
#'  \item Nakagawa S, Johnson P, Schielzeth H (2017) The coefficient of determination R2 and intra-class correlation coefficient from generalized linear mixed-effects models revisted and expanded. J. R. Soc. Interface 14. \doi{10.1098/rsif.2017.0213}
#'  }
#'
#' @examples
#' \dontrun{
#' library(lme4)
#' data(sleepstudy)
#' m <- lmer(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)
#' get_variances(m)}
#'
#' @export
get_variances <- function(x, ...) {
  UseMethod("get_variances")
}


#' @export
get_variances.default <- function(x, ...) {
  warning(sprintf("Objects of class `%s` are not supported.", class(x)[1]))
  NULL
}


#' @export
get_variances.merMod <- function(x, ...) {
  .compute_variances(x, name_fun = "get_variances", name_full = "random effect variances")
}


#' @export
get_variances.glmmTMB <- function(x, ...) {
  .compute_variances(x, name_fun = "get_variances", name_full = "random effect variances")
}


#' @export
get_variances.stanreg <- function(x, ...) {
  .compute_variances(x, name_fun = "get_variances", name_full = "random effect variances")
}
