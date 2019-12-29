#' @title Find statistic for model
#' @description Returns the statistic for a regression model (\emph{t}-statistic,
#'   \emph{z}-statistic, etc.).
#' @name find_statistic
#'
#' @description Small helper that checks if a model is a regression model
#'   object and return the statistic used.
#'
#' @param x An object.
#' @param ... Currently not used.
#'
#' @return A character describing the type of statistic. If there is no
#'   statistic available with a distribution, \code{NULL} will be returned.
#'
#' @examples
#' # regression model object
#' data(mtcars)
#' m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
#' find_statistic(m)
#' @export
find_statistic <- function(x, ...) {

  # model object check --------------------------------------------------------

  # check if the object is a model object; if so, quit early
  if (!isTRUE(is_model(x))) {
    stop(message("The entered object is not a model object."), call. = FALSE)
  }

  # t-value objects ----------------------------------------------------------

  t.mods <-
    c(
      "BBreg",
      "BBmm",
      "bcplm",
      "biglm",
      "bglmerMod",
      "blmerMod",
      "cch",
      "censReg",
      "cgam",
      "cgamm",
      "coeftest",
      "complmrob",
      "cpglm",
      "cpglmm",
      "crq",
      "drc",
      "feis",
      "felm",
      "gamlss",
      "garch",
      "glmmPQL",
      "gls",
      "gmm",
      "ivreg",
      "iv_robust",
      "lm",
      "lm_robust",
      "lm.beta",
      "lme",
      "lmerMod",
      "lmRob",
      "lmrob",
      "mixed",
      "mlm",
      "multinom",
      "nlmerMod",
      "nlrq",
      "nls",
      "orcutt",
      "polr",
      "rlm",
      "rlmerMod",
      "rq",
      "rqss",
      "speedlm",
      "svyglm",
      "svyolr",
      "truncreg",
      "wbm",
      "wblm",
      "zcpglm"
    )

  # z-value objects ----------------------------------------------------------

  z.mods <-
    c(
      "aareg",
      "betareg",
      "bracl",
      "brglm",
      "brglmFit",
      "brmultinom",
      "cglm",
      "clm",
      "clm2",
      "clmm",
      "clmm2",
      "coxme",
      "coxph",
      "crch",
      "ergm",
      "feglm",
      "fixest",
      "flexsurvreg",
      "gee",
      "glimML",
      "glmmadmb",
      "glmmLasso",
      "glmmTMB",
      "glmx",
      "gmnl",
      "hurdle",
      "lavaan",
      "loggammacenslmrob",
      "LORgee",
      "lrm",
      "mixor",
      "MixMod",
      "mjoint",
      "mle2",
      "mlogit",
      "mclogit",
      "mmclogit",
      "mvmeta",
      "negbin",
      "nlreg",
      "objectiveML",
      "psm",
      "rma",
      "rma.uni",
      "sem",
      "slm",
      "survreg",
      "tobit",
      "vglm",
      "wbgee",
      "zeroinfl",
      "zerotrunc"
    )

  # F-value objects ----------------------------------------------------------

  f.mods <-
    c(
      "Anova.mlm",
      "aov",
      "aovlist",
      "anova",
      "Gam",
      "manova"
    )

  # chi-squared value objects ------------------------------------------------

  chi.mods <-
    c(
      "geeglm",
      "logistf",
      "vgam"
    )

  # mixed bag ----------------------------------------------------------------

  # models for which there is no clear t-or z-statistic
  # which statistic to use will be decided based on the family used
  g.mods <-
    c(
      "bigglm",
      "gam",
      "glm",
      "glmc",
      "glmerMod",
      "glmRob",
      "glmrob",
      "speedglm"
    )

  # t-statistic
  g.t.mods <-
    c(
      "quasi",
      "gaussian",
      "quasibinomial",
      "quasipoisson",
      "Gamma",
      "inverse.gaussian"
    )

  # z-statistic
  g.z.mods <-
    c(
      "binomial",
      "poisson"
    )

  # pattern finding ----------------------------------------------------------

  unclear.mods <-
    c("plm")

  # no statistic -------------------------------------------------------------

  unsupported.mods <-
    c(
      "BFBayesFactor",
      "brmsfit",
      "stanreg",
      "stanmvreg",
      "gbm",
      "list",
      "MCMCglmm",
      "survfit"
    )

  # edge cases ---------------------------------------------------------------

  # tweedie-check needs to come first, because glm can also have tweedie
  # family, so this exception needs to be caught before checking for g.mods

  tryCatch({
    suppressWarnings(
      if (!is_multivariate(x) && model_info(x)$is_tweedie) {
        return("t-statistic")
      }
    )
  },
  error = function(e) {}
  )

  # statistic check -----------------------------------------------------------

  if (class(x)[[1]] %in% unsupported.mods) {
    return(NULL)
  }

  if (class(x)[[1]] %in% t.mods) {
    return("t-statistic")
  }

  if (class(x)[[1]] %in% z.mods) {
    return("z-statistic")
  }

  if (class(x)[[1]] %in% f.mods) {
    return("F-statistic")
  }

  if (class(x)[[1]] %in% chi.mods) {
    return("chi-squared statistic")
  }

  if (class(x)[[1]] %in% g.mods) {
    if (model_info(x)$family %in% g.t.mods) {
      return("t-statistic")
    } else {
      return("z-statistic")
    }
  }

  # ambiguous cases -----------------------------------------------------------

  if (class(x)[[1]] %in% unclear.mods) {
    col_names <- colnames(as.data.frame(summary(x)$coefficients))
    t_names <-
      c(
        "t",
        "t-value",
        "t value",
        "t.value",
        "Pr(>|t|)"
      )
    z_names <-
      c(
        "z",
        "z-value",
        "z value",
        "z.value",
        "Pr(>|z|)",
        "Pr(>|Z|)",
        "Naive z",
        "Robust z",
        "san.z",
        "Wald Z"
      )
    f_names <- c("F", "F-value", "F value", "F.value")
    chi_names <- c("Chisq", "chi-sq", "chi.sq", "Wald", "W", "Pr(>|W|)")

    if (length(colnames(as.data.frame(summary(x)$coefficients))) == 0L) {
      return(NULL)
    }
    if (any(t_names %in% col_names)) {
      return("t-statistic")
    }
    if (any(z_names %in% col_names)) {
      return("z-statistic")
    }
    if (any(f_names %in% col_names)) {
      return("F-statistic")
    }
    if (any(chi_names %in% col_names)) {
      return("chi-squared statistic")
    }
  }
}
