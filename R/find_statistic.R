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

  if (inherits(x, "mipo")) {
    models <- eval(x$call$object)
    x <- models$analyses[[1]]
  }

  # t-value objects ----------------------------------------------------------

  t.mods <-
    c(
      "bayesx",
      "BBreg",
      "BBmm",
      "bcplm",
      "biglm",
      "bglmerMod",
      "blmerMod",
      "cch",
      "censReg",
      "coeftest",
      "complmrob",
      "cpglm",
      "cpglmm",
      "crq",
      "crqs",
      "drc",
      "emmGrid",
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
      "lmerModLmerTest",
      "lmRob",
      "lmrob",
      "lqm",
      "lqmm",
      "maxLik",
      "mixed",
      "mlm",
      "multinom",
      "nlmerMod",
      "nlrq",
      "nls",
      "ols",
      "orcutt",
      "polr",
      "rlm",
      "rms",
      "rlmerMod",
      "rq",
      "rqss",
      "speedlm",
      "spml",
      "svyglm",
      "svyolr",
      "truncreg",
      "wbm",
      "wblm"
    )

  # z-value objects ----------------------------------------------------------

  z.mods <-
    c(
      "aareg",
      "Arima",
      "averaging",
      "betamfx",
      "betaor",
      "betareg",
      "bife",
      "boot_test_mediation",
      "bracl",
      "brglm",
      "brglmFit",
      "brmultinom",
      "cglm",
      "cph",
      "clm",
      "clm2",
      "clmm",
      "clmm2",
      "clogit",
      "coxme",
      "coxph",
      "crch",
      "DirichletRegModel",
      "ergm",
      "feglm",
      "fixest",
      "flexsurvreg",
      "gee",
      "glimML",
      "glmm",
      "glmmadmb",
      "glmmLasso",
      "glmmTMB",
      "glmx",
      "gmnl",
      "hurdle",
      "lavaan",
      "loggammacenslmrob",
      "logitmfx",
      "logitor",
      "LORgee",
      "lrm",
      "margins",
      "metaplus",
      "mixor",
      "MixMod",
      "mjoint",
      "mle",
      "mle2",
      "mlogit",
      "mclogit",
      "mmclogit",
      "mvmeta",
      "negbin",
      "negbinmfx",
      "negbinirr",
      "nlreg",
      "objectiveML",
      "orm",
      "poissonmfx",
      "poissonirr",
      "psm",
      "probitmfx",
      "robmixglm",
      "rma",
      "rma.mv",
      "rma.uni",
      "sem",
      "slm",
      "survreg",
      "test_mediation",
      "tobit",
      "vglm",
      "wbgee",
      "zcpglm",
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
      "coxph.penal",
      "geeglm",
      "logistf",
      "MANOVA",
      "RM",
      "vgam"
    )

  # mixed bag ----------------------------------------------------------------

  # models for which there is no clear t-or z-statistic
  # which statistic to use will be decided based on the family used
  g.mods <-
    c(
      "bam",
      "bigglm",
      "cgam",
      "cgamm",
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

  unclear.mods <- c("plm")

  if (inherits(x, "glht")) {
    if (x$df == 0) {
      z.mods <- c(z.mods, "glht")
    } else {
      t.mods <- c(t.mods, "glht")
    }
  }

  # no statistic -------------------------------------------------------------

  unsupported.mods <-
    c(
      "bcplm",
      "BFBayesFactor",
      "brmsfit",
      "gbm",
      "list",
      "MCMCglmm",
      "pairwise.htest",
      "splmm",
      "stanreg",
      "stanmvreg",
      "survfit"
    )

  # edge cases ---------------------------------------------------------------

  # tweedie-check needs to come first, because glm can also have tweedie
  # family, so this exception needs to be caught before checking for g.mods

  tryCatch(
    {
      suppressWarnings(
        if (!is_multivariate(x) && model_info(x)$is_tweedie) {
          return("t-statistic")
        }
      )
    },
    error = function(e) {}
  )

  # statistic check -----------------------------------------------------------

  model_class <- class(x)[[1]]

  if (model_class %in% unsupported.mods) {
    return(NULL)
  }

  if (model_class %in% t.mods) {
    return("t-statistic")
  }

  if (model_class %in% z.mods) {
    return("z-statistic")
  }

  if (model_class %in% f.mods) {
    return("F-statistic")
  }

  if (model_class %in% chi.mods) {
    return("chi-squared statistic")
  }

  if (model_class %in% g.mods) {
    if (model_info(x)$family %in% g.t.mods) {
      return("t-statistic")
    } else {
      return("z-statistic")
    }
  }

  # ambiguous cases -----------------------------------------------------------

  if (model_class %in% unclear.mods) {
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
