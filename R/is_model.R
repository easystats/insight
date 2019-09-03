#' @title Checks if an object is a regression model object
#' @name is_model
#'
#' @description Small helper that checks if a model is a regression model
#'   object.
#'
#' @param x An object.
#'
#' @return A logical, \code{TRUE} if \code{x} is a (supported) model object.
#'
#' @details This function returns \code{TRUE} if \code{x} is a model object.
#'
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
#'
#' is_model(m)
#' is_model(mtcars)
#' @export
is_model <- function(x) {
  inherits(
    x,
    c(
      "aareg",
      "aov",
      "aovlist",
      "BBmm",
      "BBreg",
      "betareg",
      "BFBayesFactor",
      "bglmerMod",
      "biglm",
      "bigglm",
      "blmerMod",
      "brmsfit",
      "btergm",
      "cch",
      "censReg",
      "clm",
      "clm2",
      "clmm",
      "confusionMatrix",
      "coxme",
      "coxph",
      "crch",
      "crq",
      "drc",
      "ergm",
      "feis",
      "felm",
      "gam",
      "Gam",
      "gamlss",
      "gamm",
      "gamm4",
      "garch",
      "gbm",
      "gee",
      "geeglm",
      "glimML",
      "glm",
      "glmerMod",
      "glmmadmb",
      "glmmPQL",
      "glmmTMB",
      "glmnet",
      "glmrob",
      "glmRob",
      "gls",
      "gmnl",
      "htest",
      "hurdle",
      "iv_robust",
      "ivreg",
      "lm",
      "lm_robust",
      "lme",
      "lmrob",
      "lmRob",
      "logistf",
      "loo",
      "LORgee",
      "lmodel2",
      "lqmm",
      "lrm",
      "ols",
      "orcutt",
      "mclogit",
      "mmclogit",
      "MCMCglmm",
      "mediate",
      "merMod",
      "mixed",
      "MixMod",
      "mjoint",
      "mle2",
      "mlm",
      "mlogit",
      "multinom",
      "negbin",
      "nlrq",
      "nls",
      "plm",
      "plmm",
      "polr",
      "psm",
      "ridgelm",
      "rjags",
      "rlm",
      "rlme",
      "rlmerMod",
      "rms",
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
      "zeroinfl",
      "zerotrunc"
    )
  )
}
