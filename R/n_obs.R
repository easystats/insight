#' @title Get number of observations from a model
#' @name n_obs
#'
#' @description This method returns the number of observation that were used
#'   to fit the model, as numeric value.
#'
#' @param weighted For survey designs, returns the weighted sample size.
#' @inheritParams find_predictors
#' @inheritParams find_formula
#'
#' @return The number of observations used to fit the model, or \code{NULL} if
#'   this information is not available.
#'
#' @note For model-objects supported by \pkg{insight} that \emph{do not} have
#' a \code{nobs()}-method, \pkg{insight} provides a \code{nobs()}-method as well.
#'
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
#' n_obs(m)
#'
#' @importFrom stats model.frame nobs
#' @export
n_obs <- function(x, ...) {
  UseMethod("n_obs")
}


#' @export
n_obs.default <- function(x, ...) {
  if (inherits(x, "list") && .obj_has_name(x, "gam")) {
    x <- x$gam
    class(x) <- c(class(x), c("glm", "lm"))
  }

  tryCatch({
    stats::nobs(x)
  },
  error = function(x) {
    NULL
  }
  )
}



#' @rdname n_obs
#' @export
n_obs.svyolr <- function(x, weighted = FALSE, ...) {
  if (weighted)
    stats::nobs(x)
  else
    nrow(stats::model.frame(x))
}



#' @export
n_obs.gamm <- function(x, ...) {
  x <- x$gam
  class(x) <- c(class(x), c("glm", "lm"))
  NextMethod()
}

#' @export
nobs.gamm <- function(x, ...) {
  x <- x$gam
  class(x) <- c(class(x), c("glm", "lm"))
  n_obs(x, ...)
}



#' @export
n_obs.lmRob <- function(x, ...) {
  length(x$fitted.values)
}

#' @export
nobs.lmRob <- n_obs.lmRob



#' @export
n_obs.LORgee <- function(x, ...) {
  x$nobs
}


#' @export
n_obs.biglm <- function(x, ...) {
  x$n
}

#' @export
nobs.biglm <- n_obs.biglm



#' @export
n_obs.bigglm <- function(x, ...) {
  x$n
}

#' @export
nobs.bigglm <- n_obs.bigglm



#' @export
n_obs.gbm <- function(x, ...) {
  length(x$fit)
}

#' @export
nobs.gbm <- n_obs.gbm



#' @export
n_obs.glimML <- function(x, ...) {
  nrow(x@data)
}

#' @export
nobs.glimML <- n_obs.glimML



#' @export
n_obs.glmRob <- function(x, ...) {
  length(x$fitted.values)
}

#' @export
nobs.glmRob <- n_obs.glmRob



#' @export
n_obs.gmnl <- function(x, ...) {
  x$logLik$nobs
}

#' @export
nobs.gmnl <- n_obs.gmnl




#' @export
n_obs.multinom <- function(x, ...) {
  nrow(x$fitted.values)
}

#' @export
nobs.multinom <- n_obs.multinom



#' @export
n_obs.rq <- function(x, ...) {
  length(x$fitted.values)
}

#' @export
nobs.rq <- n_obs.rq



#' @export
n_obs.BBreg <- function(x, ...) {
  x$nObs
}


#' @export
n_obs.BBmm <- function(x, ...) {
  x$nObs
}


#' @export
n_obs.crq <- function(x, ...) {
  nrow(x$residuals)
}


#' @export
n_obs.survfit <- function(x, ...) {
  length(x$n.event)
}


#' @export
n_obs.coxph <- function(x, ...) {
  max(x$n)
}


#' @export
n_obs.coxme <- function(x, ...) {
  max(x$n)
}


#' @export
n_obs.felm <- function(x, ...) {
  x$N
}


#' @export
n_obs.feis <- function(x, ...) {
  length(x$fitted.values)
}


#' @export
n_obs.aovlist <- function(x, ...) {
  nrow(stats::model.frame(x))
}


#' @export
n_obs.stanmvreg <- function(x, ...) {
  min(x$n_yobs)
}


#' @export
n_obs.mlogit <- function(x, ...) {
  nrow(x$model)
}


#' @export
n_obs.hurdle <- function(x, ...) {
  x$n
}


#' @export
n_obs.zerotrunc <- function(x, ...) {
  x$n
}


#' @export
n_obs.zeroinfl <- function(x, ...) {
  x$n
}


#' @export
n_obs.wbm <- function(x, ...) {
  nrow(x@frame)
}
