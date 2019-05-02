#' @title Checks if an object is a (supported) regression model object
#' @name is_model
#'
#' @description Small helper that checks if a model is a \emph{supported}
#'  (regression) model object.
#'
#' @param x An object.
#'
#' @return A logical, \code{TRUE} if \code{x} is a (supported) model object.
#'
#' @details This function returns \code{TRUE} is \code{x} is a model object
#'   that works with the package's functions. A list of supported models can
#'   also be found here: \url{https://github.com/easystats/insight}.
#'
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
#'
#' is_model(m)
#' is_model(mtcars)
#'
#' @export
is_model <- function(x) {
  inherits(x, c(
    "aov", "aovlist", "betareg", "BFBayesFactor", "biglm", "bigglm", "brmsfit", "censReg",
    "clm", "clm2", "clmm", "coxme", "coxph", "crch", "crq", "feis",
    "felm", "gam", "Gam", "gamlss", "gamm", "gbm", "gee", "geeglm",
    "glm", "glmmPQL", "glmmTMB", "glmrob", "glmRob", "gls", "gmnl",
    "htest", "hurdle", "iv_robust", "ivreg", "lm", "lm_robust", "lme",
    "lmrob", "lmRob", "logistf", "LORgee", "lrm", "ols", "MCMCglmm", "merMod",
    "mixed", "MixMod", "mlm", "mlogit", "multinom", "plm", "polr",
    "psm", "rlmerMod", "rq", "rqss", "speedlm", "speedglm", "stanmvreg", "stanreg", "survreg",
    "tobit", "truncreg", "vgam", "vglm", "zeroinfl", "zerotrunc"
  ))
}
