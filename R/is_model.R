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
  inherits(x,
    c("lm", "glm", "aov", "aovlist", "brmsfit", "Gam", "MixMod", "crq", "gam",
      "gamlss", "glmmTMB", "glmRob", "glmrob", "lmRob", "lmrob", "coxph", "coxme",
      "MCMCglmm", "clm2", "clm", "clmm", "felm", "gee", "gls", "hurdle", "ivreg",
      "iv_robust", "lme", "merMod", "mixed", "plm", "rlmerMod", "stanreg", "geeglm",
      "stanmvreg", "zeroinfl", "zerotrunc", "lrm", "rqss", "rq", "vgam", "vglm",
      "betareg", "logistf", "mlogit", "multinom", "polr", "truncreg", "glmmPQL",
      "gmnl", "lm_robust", "tobit", "survreg", "crch", "feis", "gbm", "BFBayesFactor"
  ))
}
