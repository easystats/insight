#' @title Checks if an object is a regression model object supported in
#'   \pkg{insight} package.
#' @name is_model_supported
#'
#' @description Small helper that checks if a model is a \emph{supported}
#'  (regression) model object. \code{supported_models()} prints a list
#'  of currently supported model classes.
#'
#' @inheritParams is_model
#'
#' @return A logical, \code{TRUE} if \code{x} is a (supported) model object.
#'
#' @details This function returns \code{TRUE} if \code{x} is a model object
#'   that works with the package's functions. A list of supported models can
#'   also be found here: \url{https://github.com/easystats/insight}.
#'
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
#'
#' is_model_supported(m)
#' is_model_supported(mtcars)
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
    "aareg",
    "afex_aov",
    "aov",
    "aovlist",
    "Arima",
    "averaging",
    "bamlss",
    "bamlss.frame",
    "bayesx",
    "bayesQR",
    "BBmm",
    "BBreg",
    "bcplm",
    "betamfx",
    "betaor",
    "betareg",
    "BFBayesFactor",
    "BGGM",
    "bife",
    "biglm",
    "bigglm",
    "blavaan",
    "bracl",
    "brglm",
    "brmsfit",
    "brmultinom",
    "censReg",
    "cgam",
    "cgamm",
    "cglm",
    "clm",
    "clm2",
    "clmm",
    "clmm2",
    "clogit",
    "complmrob",
    "coxme",
    "coxph",
    "cpglm",
    "cpglmm",
    "crch",
    "crq",
    "crqs",
    "DirichletRegModel",
    "feis",
    "felm",
    "feglm",
    "fixest",
    "flexsurvreg",
    "gam",
    "Gam",
    "gamlss",
    "gamm",
    "gamm4",
    "gbm",
    "gee",
    "geeglm",
    "glht",
    "glimML",
    "glm",
    "glmmadmb",
    "glmmPQL",
    "glmmTMB",
    "glmrob",
    "glmRob",
    "glmx",
    "gls",
    "gmnl",
    "htest",
    "hurdle",
    "iv_robust",
    "ivreg",
    "lavaan",
    "lm",
    "lm_robust",
    "lme",
    "lmrob",
    "lmRob",
    "lmerMod",
    "lmerModLmerTest",
    "logitmfx",
    "logitor",
    "logistf",
    "LORgee",
    "lrm",
    "MANOVA",
    "maxLik",
    "mcmc",
    "MCMCglmm",
    "metaplus",
    "merMod",
    "mixed",
    "mixor",
    "MixMod",
    "mlm",
    "mclogit",
    "mlogit",
    "mmlogit",
    "multinom",
    "negbinmfx",
    "negbinirr",
    "ols",
    "poissonmfx",
    "poissonirr",
    "plm",
    "polr",
    "psm",
    "probitmfx",
    "rlm",
    "rlmerMod",
    "RM",
    "rma",
    "rma.uni",
    "robmixglm",
    "rq",
    "rqss",
    "speedlm",
    "speedglm",
    "stanmvreg",
    "stanreg",
    "survfit",
    "survreg",
    "svyglm",
    "svyolr",
    "tobit",
    "truncreg",
    "vgam",
    "vglm",
    "wbm",
    "wblm",
    "wbgee",
    "zcpglm",
    "zeroinfl",
    "zerotrunc"
  )
}
