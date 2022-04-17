#' @title Checks if an object is a regression model or statistical test object
#' @name is_model
#'
#' @description Small helper that checks if a model is a regression model or
#'   a statistical object. `is_regression_model()` is stricter and only
#'   returns `TRUE` for regression models, but not for, e.g., `htest`
#'   objects.
#'
#' @param x An object.
#'
#' @return A logical, `TRUE` if `x` is a (supported) model object.
#'
#' @details This function returns `TRUE` if `x` is a model object.
#'
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
#'
#' is_model(m)
#' is_model(mtcars)
#'
#' test <- t.test(1:10, y = c(7:20))
#' is_model(test)
#' is_regression_model(test)
#' @export
is_model <- function(x) {
  inherits(.get_class_list(x), .get_model_classes())
}


# Is regression model -----------------------------------------------------

#' @rdname is_model
#' @export
is_regression_model <- function(x) {
  inherits(.get_class_list(x), .get_model_classes(regression_only = TRUE))
}


# Helpers -----------------------------------------------------------------

.get_class_list <- function(x) {
  if (length(class(x)) > 1 || class(x) != "list") {
    return(x)
  }

  if (all(c("mer", "gam") %in% names(x))) {
    class(x) <- c("gamm4", "list")
  }
  x
}


.get_model_classes <- function(regression_only = FALSE) {
  out <- c(
    "_ranger",

    # a --------------------
    "aareg", "afex_aov", "AKP", "ancova", "anova", "Anova.mlm",
    "aov", "aovlist", "Arima", "averaging",

    # b --------------------
    "bamlss", "bamlss.frame", "bayesGAM", "bayesmeta", "bayesx",
    "bayesQR", "BBmm", "BBreg", "bcplm", "betamfx", "betaor", "betareg",
    "bfsl", "BFBayesFactor", "BGGM", "bglmerMod", "bife", "bifeAPEs",
    "biglm", "bigglm", "blrm", "blavaan", "blmerMod",
    "boot_test_mediation", "bracl", "brglm", "brglmFit", "brmsfit",
    "brmultinom", "bsem", "btergm", "buildmer",

    # c --------------------
    "cch", "censReg", "cgam", "cgamm", "cglm", "clm", "clm2",
    "clmm", "clmm2", "clogit", "coeftest", "complmrob", "comprisk",
    "confusionMatrix", "coxme", "coxph", "coxph.penal", "coxr",
    "cpglm", "cpglmm", "crch", "crq", "crqs", "crr", "dglm",

    # d --------------------
    "dep.effect", "deltaMethod", "DirichletRegModel", "drc",

    # e --------------------
    "eglm", "elm", "emmGrid", "emm_list", "epi.2by2", "ergm",

    # f --------------------
    "feglm", "feis", "felm", "fitdistr", "fixest", "flexmix",
    "flexsurvreg",

    # g --------------------
    "gam", "Gam", "GAMBoost", "gamlr", "gamlss", "gamm", "gamm4",
    "garch", "gbm", "gee", "geeglm", "gjrm", "glht", "glimML", "Glm", "glm",
    "glmaag", "glmbb", "glmboostLSS", "glmc", "glmdm", "glmdisc",
    "glmerMod", "glmlep", "glmm", "glmmadmb", "glmmEP", "glmmFit",
    "glmmfields", "glmmLasso", "glmmPQL", "glmmTMB", "glmnet", "glmrob",
    "glmRob", "glmx", "gls", "gmnl", "gmm", "gnls", "gsm",

    # h --------------------
    "heavyLme", "HLfit", "htest", "hurdle",

    # i --------------------
    "ivFixed", "iv_robust", "ivreg", "ivprobit",

    # j --------------------
    "joint",

    # k --------------------
    "kmeans",

    # l --------------------
    "lavaan", "lm", "lm_robust", "lme", "lmrob", "lmRob",
    "loggammacenslmrob", "logistf", "LogitBoost", "loo",
    "LORgee", "lmodel2", "lmerMod", "lmerModLmerTest",
    "logitmfx", "logitor", "lqm", "lqmm", "lrm",

    # m --------------------
    "maov", "manova", "MANOVA", "margins", "maxLik", "mboostLSS",
    "mclogit", "mcp1", "mcp2", "mmclogit", "mcmc", "mcmc.list",
    "MCMCglmm", "mediate", "merMod", "merModList", "meta_bma",
    "meta_fixed", "meta_random", "meta_ordered", "metaplus",
    "mhurdle", "mipo", "mira", "mixed", "mixor", "MixMod", "mjoint",
    "mle", "mle2", "mlergm", "mlm", "mlma", "mlogit", "model_fit",
    "multinom", "mvmeta", "mvord", "mvr", "marginaleffects",
    "marginaleffects.summary",

    # n --------------------
    "negbin", "negbinmfx", "negbinirr", "nlreg", "nlrq", "nls",
    "nparLD",

    # o --------------------
    "objectiveML", "ols", "osrt", "orcutt",

    # p --------------------
    "pairwise.htest", "pb1", "pb2", "pgmm", "plm", "plmm", "PMCMR",
    "poissonmfx", "poissonirr", "polr", "pseudoglm", "psm", "probitmfx",

    # q --------------------
    "qr", "QRNLMM", "QRLMM",

    # r --------------------
    "rankFD", "Rchoice", "rdrobust", "ridgelm", "riskRegression",
    "rjags", "rlm", "rlme", "rlmerMod", "RM", "rma", "rmanovab",
    "rma.uni", "rms", "robmixglm", "robtab", "rq", "rqs", "rqss",

    # s --------------------
    "Sarlm", "scam", "selection", "sem", "SemiParBIV", "slm", "speedlm",
    "speedglm", "splmm", "spml", "stanmvreg", "stanreg", "summary.lm",
    "survfit", "survreg", "survPresmooth", "svychisq", "svyglm", "svy_vglm",
    "svyolr", "svytable", "systemfit",

    # t --------------------
    "t1way", "t2way", "t3way", "test_mediation", "tobit", "trendPMCMR",
    "trimcibt", "truncreg",

    # v --------------------
    "varest", "vgam", "vglm",

    # w --------------------
    "wbm", "wblm", "wbgee", "wmcpAKP",

    # y --------------------
    "yuen", "yuend",

    # z --------------------
    "zcpglm", "zeroinfl", "zerotrunc"
  )

  if (isTRUE(regression_only)) {
    out <- setdiff(out, c(
      "emmGrid", "emm_list", "htest", "pairwise.htest", "summary.lm",
      "marginaleffects", "marginaleffects.summary"
    ))
  }

  out
}





.get_gam_classes <- function() {
  out <- c(
    "bamlss", "bamlss.frame", "brmsfit",
    "cgam", "cgamm",
    "gam", "Gam", "GAMBoost", "gamlr", "gamlss", "gamm", "gamm4",
    "stanmvreg", "stanreg"
  )
  out
}
