#' @title Get variance components from random effects models
#' @name get_variance
#'
#' @description
#'
#' This function extracts the different variance components of a mixed model and
#' returns the result as list. Functions like `get_variance_residual(x)` or
#' `get_variance_fixed(x)` are shortcuts for `get_variance(x, component = "residual")`
#' etc.
#'
#' @param x A mixed effects model.
#' @param component Character value, indicating the variance component that
#' should be returned. By default, all variance components are returned. Valid
#' options are `"all"`, `"fixed"`, `"random"`, `"residual"`, `"distribution"`,
#' `"dispersion"`, `"intercept"`, `"slope"`, `"rho01"`, and `"rho00"`, which are
#' equivalent to calling the dedicated functions like `get_variance_residual()`
#' etc. The distribution-specific (`"distribution"`) and residual (`"residual"`)
#' variance are the most computational intensive components, and hence may take
#' a few seconds to calculate.
#' @param verbose Toggle off warnings.
#' @param tolerance Tolerance for singularity check of random effects, to decide
#' whether to compute random effect variances or not. Indicates up to which
#' value the convergence result is accepted. The larger tolerance is, the
#' stricter the test will be. See [`performance::check_singularity()`].
#' @param null_model Optional, a null-model to be used for the calculation of
#' random effect variances. If `NULL`, the null-model is computed internally.
#' @param approximation Character string, indicating the approximation method
#' for the distribution-specific (observation level, or residual) variance. Only
#' applies to non-Gaussian models. Can be `"lognormal"` (default), `"delta"` or
#' `"trigamma"`. For binomial models, the default is the _theoretical_
#' distribution specific variance, however, it can also be
#' `"observation_level"`. See _Nakagawa et al. 2017_, in particular supplement
#' 2, for details.
#' @param model_component For models that can have a zero-inflation component,
#' specify for which component variances should be returned. If `NULL` or
#' `"full"` (the default), both the conditional and the zero-inflation component
#' are taken into account. If `"conditional"`, only the conditional component is
#' considered.
#' @param ... Currently not used.
#'
#' @return A list with following elements:
#'
#' - `var.fixed`, variance attributable to the fixed effects
#' - `var.random`, (mean) variance of random effects
#' - `var.residual`, residual variance (sum of dispersion and distribution-specific/observation level variance)
#' - `var.distribution`, distribution-specific (or observation level) variance
#' - `var.dispersion`, variance due to additive dispersion
#' - `var.intercept`, the random-intercept-variance, or between-subject-variance (\ifelse{html}{\out{&tau;<sub>00</sub>}}{\eqn{\tau_{00}}})
#' - `var.slope`, the random-slope-variance (\ifelse{html}{\out{&tau;<sub>11</sub>}}{\eqn{\tau_{11}}})
#' - `cor.slope_intercept`, the random-slope-intercept-correlation (\ifelse{html}{\out{&rho;<sub>01</sub>}}{\eqn{\rho_{01}}})
#' - `cor.slopes`, the correlation between random slopes (\ifelse{html}{\out{&rho;<sub>00</sub>}}{\eqn{\rho_{00}}})
#'
#' @details This function returns different variance components from mixed models,
#' which are needed, for instance, to calculate r-squared measures or the
#' intraclass-correlation coefficient (ICC).
#'
#' @section Fixed effects variance:
#' The fixed effects variance, \ifelse{html}{\out{&sigma;<sup>2</sup><sub>f</sub>}}{\eqn{\sigma^2_f}},
#' is the variance of the matrix-multiplication \ifelse{html}{\out{&beta;&lowast;X}}{\eqn{\beta*X}}
#' (parameter vector by model matrix).
#'
#' @section Random effects variance:
#' The random effect variance, \ifelse{html}{\out{&sigma;<sup>2</sup><sub>i</sub>}}{\eqn{\sigma^2_i}},
#' represents the *mean* random effect variance of the model. Since this
#' variance reflects the "average" random effects variance for mixed models, it
#' is also appropriate for models with more complex random effects structures,
#' like random slopes or nested random effects. Details can be found in
#' _Johnson 2014_, in particular equation 10. For simple random-intercept models,
#' the random effects variance equals the random-intercept variance.
#'
#' @section Distribution-specific (observation level) variance:
#' The distribution-specific variance,
#' \ifelse{html}{\out{&sigma;<sup>2</sup><sub>d</sub>}}{\eqn{\sigma^2_d}},
#' is the conditional variance of the response given the predictors , `Var[y|x]`,
#' which depends on the model family.
#' - **Gaussian:** For Gaussian models, it is
#'   \ifelse{html}{\out{&sigma;<sup>2</sup>}}{\eqn{\sigma^2}} (i.e. `sigma(model)^2`).
#' - **Bernoulli:** For models with binary outcome, it is
#'   \ifelse{html}{\out{&pi;<sup>2</sup>/3}}{\eqn{\pi^2 / 3}} for logit-link,
#'   `1` for probit-link, and \ifelse{html}{\out{&pi;<sup>2</sup>/6}}{\eqn{\pi^2 / 6}}
#'   for cloglog-links.
#' - **Binomial:** For other binomial models, the distribution-specific variance
#'   for Bernoulli models is used, divided by a weighting factor based on the
#'   number of trials and successes.
#' - **Gamma:** Models from Gamma-families use \ifelse{html}{\out{&mu;<sup>2</sup>}}{\eqn{\mu^2}}
#'   (as obtained from `family$variance()`).
#' - For all other models, the distribution-specific variance is by default
#'   based on lognormal approximation,
#'   \ifelse{html}{\out{log(1 + var(x) / &mu;<sup>2</sup>)}}{\eqn{log(1 + var(x) / \mu^2)}}
#'   (see _Nakagawa et al. 2017_). Other approximation methods can be specified
#'   with the `approximation` argument.
#' - **Zero-inflation models:** The expected variance of a zero-inflated model
#'   is computed according to _Zuur et al. 2012, p277_.
#'
#' @section Variance for the additive overdispersion term:
#' The variance for the additive overdispersion term,
#' \ifelse{html}{\out{&sigma;<sup>2</sup><sub><em>e</em></sub>}}{\eqn{\sigma^2_e}},
#' represents "the excess variation relative to what is expected from a certain
#' distribution" (_Nakagawa et al. 2017_). In (most? many?) cases, this will be
#' `0`.
#'
#' @section Residual variance:
#' The residual variance, \ifelse{html}{\out{&sigma;<sup>2</sup><sub>&epsilon;</sub>}}{\eqn{\sigma^2_\epsilon}},
#' is simply \ifelse{html}{\out{&sigma;<sup>2</sup><sub>d</sub> + &sigma;<sup>2</sup><sub><em>e</em></sub>}}{\eqn{\sigma^2_d + \sigma^2_e}}.
#' It is also called *within-subject variance*.
#'
#' @section Random intercept variance:
#' The random intercept variance, or *between-subject* variance
#' (\ifelse{html}{\out{&tau;<sub>00</sub>}}{\eqn{\tau_{00}}}), is obtained from
#' `VarCorr()`. It indicates how much groups or subjects differ from each other,
#' while the residual variance \ifelse{html}{\out{&sigma;<sup>2</sup><sub>&epsilon;</sub>}}{\eqn{\sigma^2_\epsilon}}
#' indicates the *within-subject variance*.
#'
#' @section Random slope variance:
#' The random slope variance (\ifelse{html}{\out{&tau;<sub>11</sub>}}{\eqn{\tau_{11}}})
#' is obtained from `VarCorr()`. This measure is only available for mixed models
#' with random slopes.
#'
#' @section Random slope-intercept correlation:
#' The random slope-intercept correlation
#' (\ifelse{html}{\out{&rho;<sub>01</sub>}}{\eqn{\rho_{01}}}) is obtained from
#' `VarCorr()`. This measure is only available for mixed models with random
#' intercepts and slopes.
#'
#' @section Supported models and model families:
#' This function supports models of class `merMod` (including models from
#' **blme**), `clmm`, `cpglmm`, `glmmadmb`, `glmmTMB`, `MixMod`, `lme`, `mixed`,
#' `rlmerMod`, `stanreg`, `brmsfit` or `wbm`. Support for objects of class
#' `MixMod` (**GLMMadaptive**), `lme` (**nlme**) or `brmsfit` (**brms**) is
#' not fully implemented or tested, and therefore may not work for all models
#' of the aforementioned classes.
#'
#' The results are validated against the solutions provided by _Nakagawa et al. (2017)_,
#' in particular examples shown in the Supplement 2 of the paper. Other model
#' families are validated against results from the **MuMIn** package. This means
#' that the returned variance components should be accurate and reliable for
#' following mixed models or model families:
#'
#' - Bernoulli (logistic) regression
#' - Binomial regression (with other than binary outcomes)
#' - Poisson and Quasi-Poisson regression
#' - Negative binomial regression (including nbinom1, nbinom2 and nbinom12 families)
#' - Gaussian regression (linear models)
#' - Gamma regression
#' - Tweedie regression
#' - Beta regression
#' - Ordered beta regression
#'
#' Following model families are not yet validated, but should work:
#'
#' - Zero-inflated and hurdle models
#' - Beta-binomial regression
#' - Compound Poisson regression
#' - Generalized Poisson regression
#' - Log-normal regression
#' - Skew-normal regression
#'
#' Extracting variance components for models with zero-inflation part is not
#' straightforward, because it is not definitely clear how the distribution-specific
#' variance should be calculated. Therefore, it is
#' recommended to carefully inspect the results, and probably validate against
#' other models, e.g. Bayesian models (although results may be only roughly
#' comparable).
#'
#' Log-normal regressions (e.g. `lognormal()` family in **glmmTMB** or
#' `gaussian("log")`) often have a very low fixed effects variance (if they were
#' calculated as suggested by _Nakagawa et al. 2017_). This results in very low
#' ICC or r-squared values, which may not be meaningful (see
#' [`performance::icc()`] or [`performance::r2_nakagawa()`]).
#'
#' @references
#'  - Johnson, P. C. D. (2014). Extension of Nakagawa & Schielzeth’s R2 GLMM to
#'  random slopes models. Methods in Ecology and Evolution, 5(9), 944–946.
#'  \doi{10.1111/2041-210X.12225}
#'
#'  - Nakagawa, S., Johnson, P. C. D., & Schielzeth, H. (2017). The coefficient
#'  of determination R2 and intra-class correlation coefficient from generalized
#'  linear mixed-effects models revisited and expanded. Journal of The Royal
#'  Society Interface, 14(134), 20170213. \doi{10.1098/rsif.2017.0213}
#'
#'  - Zuur, A. F., Savel'ev, A. A., & Ieno, E. N. (2012). Zero inflated models
#'  and generalized linear mixed models with R. Newburgh, United Kingdom:
#'  Highland Statistics.
#'
#' @examplesIf require("lme4", quietly = TRUE)
#' \donttest{
#' library(lme4)
#' data(sleepstudy)
#' m <- lmer(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)
#'
#' get_variance(m)
#' get_variance_fixed(m)
#' get_variance_residual(m)
#' }
#' @export
get_variance <- function(x, ...) {
  UseMethod("get_variance")
}

#' @export
get_variance.default <- function(x,
                                 component = "all",
                                 verbose = TRUE,
                                 ...) {
  if (isTRUE(verbose)) {
    format_warning(sprintf("Objects of class `%s` are not supported.", class(x)[1]))
  }
  NULL
}


#' @rdname get_variance
#' @export
get_variance.merMod <- function(x,
                                component = "all",
                                tolerance = 1e-8,
                                null_model = NULL,
                                approximation = "lognormal",
                                verbose = TRUE,
                                ...) {
  component <- validate_argument(
    component,
    c(
      "all", "fixed", "random", "residual", "distribution", "dispersion",
      "intercept", "slope", "rho01", "rho00"
    )
  )
  .safe(.compute_variances(
    model = x,
    component = component,
    name_fun = "get_variance",
    name_full = "random effect variances",
    verbose = verbose,
    tolerance = tolerance,
    model_null = null_model,
    approximation = approximation
  ))
}


#' @export
get_variance.rlmerMod <- get_variance.merMod

#' @export
get_variance.mjoint <- get_variance.merMod

#' @export
get_variance.cpglmm <- get_variance.merMod

#' @export
get_variance.glmmadmb <- get_variance.merMod

#' @export
get_variance.stanreg <- get_variance.merMod

#' @export
get_variance.clmm <- get_variance.merMod

#' @export
get_variance.wbm <- get_variance.merMod

#' @export
get_variance.wblm <- get_variance.merMod

#' @export
get_variance.lme <- get_variance.merMod

#' @export
get_variance.brmsfit <- get_variance.merMod


#' @rdname get_variance
#' @export
get_variance.glmmTMB <- function(x,
                                 component = "all",
                                 model_component = NULL,
                                 tolerance = 1e-8,
                                 null_model = NULL,
                                 approximation = "lognormal",
                                 verbose = TRUE,
                                 ...) {
  component <- validate_argument(
    component,
    c(
      "all", "fixed", "random", "residual", "distribution", "dispersion",
      "intercept", "slope", "rho01", "rho00"
    )
  )
  .safe(.compute_variances(
    model = x,
    component = component,
    name_fun = "get_variance",
    name_full = "random effect variances",
    verbose = verbose,
    tolerance = tolerance,
    model_component = model_component,
    model_null = null_model,
    approximation = approximation
  ))
}

#' @export
get_variance.MixMod <- get_variance.glmmTMB


#' @export
get_variance.mixed <- function(x,
                               component = "all",
                               tolerance = 1e-8,
                               null_model = NULL,
                               approximation = "lognormal",
                               verbose = TRUE,
                               ...) {
  component <- validate_argument(
    component,
    c(
      "all", "fixed", "random", "residual", "distribution", "dispersion",
      "intercept", "slope", "rho01", "rho00"
    )
  )
  .safe(.compute_variances(
    model = x$full_model,
    component = component,
    name_fun = "get_variance",
    name_full = "random effect variances",
    verbose = verbose,
    tolerance = tolerance,
    model_null = null_model,
    approximation = approximation
  ))
}


#' @rdname get_variance
#' @export
get_variance_residual <- function(x, verbose = TRUE, ...) {
  unlist(get_variance(x, component = "residual", verbose = verbose, ...))
}

#' @rdname get_variance
#' @export
get_variance_fixed <- function(x, verbose = TRUE, ...) {
  unlist(get_variance(x, component = "fixed", verbose = verbose, ...))
}

#' @rdname get_variance
#' @export
get_variance_random <- function(x, verbose = TRUE, tolerance = 1e-8, ...) {
  unlist(get_variance(x, component = "random", verbose = verbose, tolerance = tolerance, ...))
}

#' @rdname get_variance
#' @export
get_variance_distribution <- function(x, verbose = TRUE, ...) {
  unlist(get_variance(x, component = "distribution", verbose = verbose, ...))
}

#' @rdname get_variance
#' @export
get_variance_dispersion <- function(x, verbose = TRUE, ...) {
  unlist(get_variance(x, component = "dispersion", verbose = verbose, ...))
}

#' @rdname get_variance
#' @export
get_variance_intercept <- function(x, verbose = TRUE, ...) {
  unlist(get_variance(x, component = "intercept", verbose = verbose, ...))
}

#' @rdname get_variance
#' @export
get_variance_slope <- function(x, verbose = TRUE, ...) {
  unlist(get_variance(x, component = "slope", verbose = verbose, ...))
}

#' @rdname get_variance
#' @export
get_correlation_slope_intercept <- function(x, verbose = TRUE, ...) {
  unlist(get_variance(x, component = "rho01", verbose = verbose, ...))
}

#' @rdname get_variance
#' @export
get_correlation_slopes <- function(x, verbose = TRUE, ...) {
  unlist(get_variance(x, component = "rho00", verbose = verbose, ...))
}
