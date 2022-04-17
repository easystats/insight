#' @title Checks if a regression model object is supported in \pkg{insight} package
#' @name is_model_supported
#'
#' @description Small helper that checks if a model is a *supported*
#'  (regression) model object. `supported_models()` prints a list
#'  of currently supported model classes.
#'
#' @inheritParams is_model
#'
#' @return A logical, `TRUE` if `x` is a (supported) model object.
#'
#' @details
#'
#' This function returns `TRUE` if `x` is a model object that works with the
#' package's functions. A list of supported models can also be found here:
#' <https://github.com/easystats/insight>.
#'
#' @examples
#'
#' data(mtcars)
#' m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
#'
#' is_model_supported(m)
#' is_model_supported(mtcars)
#'
#' # to see all supported models
#' supported_models()
#'
#' @export
is_model_supported <- function(x) {
  inherits(x, .supported_models_list())
}


#' @rdname is_model_supported
#' @export
supported_models <- function() {
  sort(.supported_models_list())
}




.supported_models_list <- function() {
  c(
    # a ----------------------------
    "aareg", "afex_aov", "aov", "aovlist", "AKP", "Anova.mlm", "Arima",
    "averaging",

    # b ----------------------------
    "bamlss", "bamlss.frame", "bayesx", "bayesQR", "BBmm", "BBreg", "bcplm",
    "betamfx", "betaor", "betareg", "bfsl", "BFBayesFactor", "BGGM", "bife",
    "bifeAPEs", "biglm", "bigglm", "blavaan", "blrm", "bracl", "brglm", "brmsfit",
    "brmultinom", "btergm",

    # c ----------------------------
    "censReg", "cgam", "cgamm", "cglm", "clm", "clm2", "clmm", "clmm2",
    "clogit", "coeftest", "complmrob", "confusionMatrix", "coxme", "coxph",
    "coxph.penal", "coxr", "cpglm", "cpglmm", "crch", "crq", "crqs", "crr",

    # d ----------------------------
    "dep.effect", "DirichletRegModel", "drc",

    # e ----------------------------
    "eglm", "elm", "epi.2by2", "ergm",

    # f ----------------------------
    "feis", "felm", "feglm", "fitdistr", "fixest", "flexsurvreg",

    # g ----------------------------
    "gam", "Gam", "gamlss", "gamm", "gamm4", "garch", "gbm", "gee", "geeglm",
    "glht", "glimML", "glmm", "glm", "Glm", "glmmadmb", "glmmPQL", "glmmTMB",
    "glmrob", "glmRob", "glmx", "gls", "gmnl",

    # h ----------------------------
    "HLfit", "htest", "hurdle",

    # i ----------------------------
    "ivFixed", "iv_robust", "ivreg", "ivprobit",

    # l ----------------------------
    "lavaan", "lm", "lm_robust", "lme", "lmrob", "lmRob", "lmerMod",
    "lmerModLmerTest", "lmodel2", "logitmfx", "logitor", "logistf", "LORgee",
    "lqm", "lqmm", "lrm",

    # m ----------------------------
    "manova", "MANOVA", "margins", "marginaleffects", "marginaleffects.summary",
    "maxLik", "mcmc", "MCMCglmm", "mcp12", "mcp1", "mcp2", "med1way", "mediate",
    "metaplus", "merMod", "merModList", "mipo", "mira", "mixed", "mixor", "MixMod",
    "mhurdle", "mjoint", "mle", "mle2", "mlm", "mclogit", "mcmc.list", "meta_bma",
    "meta_fixed", "meta_random", "mlogit", "mmlogit", "model_fit", "multinom",
    "mvord",

    # n ----------------------------
    "negbinmfx", "negbinirr",

    # o ----------------------------
    "ols", "onesampb", "orm",

    # p ----------------------------
    "PMCMR", "poissonmfx", "poissonirr", "pgmm", "plm", "polr", "psm",
    "probitmfx",

    # r ----------------------------
    "Rchoice", "ridgelm", "riskRegression", "rjags", "rlm", "rlmerMod",
    "robtab", "RM", "rma", "rma.uni", "robmixglm", "rq", "rqs", "rqss",

    # s ----------------------------
    "Sarlm", "scam", "selection", "sem", "semLm", "semLme", "SemiParBIV", "slm",
    "speedlm", "speedglm", "stanfit", "stanmvreg", "stanreg", "summary.lm",
    "survfit", "survreg", "svy_vglm", "svychisq", "svyglm", "svyolr",

    # t ----------------------------
    "t1way", "tobit", "trimcibt", "truncreg",

    # v ----------------------------
    "vgam", "vglm",

    # w ----------------------------
    "wbm", "wblm", "wbgee", "wmcpAKP",

    # y ----------------------------
    "yuen", "yuend",

    # z ----------------------------
    "zcpglm", "zeroinfl", "zerotrunc"
  )
}
