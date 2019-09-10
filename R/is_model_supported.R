#' @title Checks if an object is a regression model object supported in
#'   `insight` package.
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
  .supported_models_list()
}




.supported_models_list <- function() {
  c(
    "aov",
    "aovlist",
    "BBmm",
    "BBreg",
    "betareg",
    "BFBayesFactor",
    "biglm",
    "bigglm",
    "brmsfit",
    "censReg",
    "clm",
    "clm2",
    "clmm",
    "coxme",
    "coxph",
    "crch",
    "crq",
    "feis",
    "felm",
    "gam",
    "Gam",
    "gamlss",
    "gamm",
    "gamm4",
    "gbm",
    "gee",
    "geeglm",
    "glimML",
    "glm",
    "glmmPQL",
    "glmmTMB",
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
    "LORgee",
    "lrm",
    "ols",
    "MCMCglmm",
    "merMod",
    "mixed",
    "MixMod",
    "mlm",
    "mlogit",
    "multinom",
    "plm",
    "polr",
    "psm",
    "rlm",
    "rlmerMod",
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
}