#' @title Get number of observations from a model
#' @name n_obs
#'
#' @description This method returns the number of observation that were used
#'   to fit the model, as numeric value.
#'
#' @param weighted For survey designs, returns the weighted sample size.
#' @inheritParams find_predictors
#' @inheritParams get_response
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

  tryCatch(
    {
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
  if (weighted) {
    stats::nobs(x)
  } else {
    nrow(stats::model.frame(x))
  }
}



#' @export
n_obs.gamm <- function(x, ...) {
  x <- x$gam
  class(x) <- c(class(x), c("glm", "lm"))
  NextMethod()
}

#' @export
nobs.gamm <- function(object, ...) {
  x <- object$gam
  class(x) <- c(class(x), c("glm", "lm"))
  n_obs(x, ...)
}



#' @export
n_obs.bayesx <- function(x, ...) {
  length(x$response)
}

#' @export
nobs.bayesx <- function(object, ...) {
  n_obs(object, ...)
}



#' @export
n_obs.flexsurvreg <- function(x, ...) {
  x$N
}

#' @export
nobs.flexsurvreg <- function(object, ...) {
  n_obs(object, ...)
}



#' @export
n_obs.bamlss <- function(x, ...) {
  nrow(x$model.frame)
}

#' @export
nobs.bamlss <- function(object, ...) {
  n_obs(object, ...)
}



#' @export
n_obs.lmRob <- function(x, ...) {
  length(x$fitted.values)
}

#' @export
nobs.lmRob <- function(object, ...) {
  n_obs(object, ...)
}



#' @export
n_obs.LORgee <- function(x, ...) {
  x$nobs
}


#' @export
n_obs.biglm <- function(x, ...) {
  x$n
}

#' @export
nobs.biglm <- function(object, ...) {
  n_obs(object, ...)
}



#' @export
n_obs.bigglm <- function(x, ...) {
  x$n
}

#' @export
nobs.bigglm <- function(object, ...) {
  n_obs(object, ...)
}



#' @export
n_obs.gbm <- function(x, ...) {
  length(x$fit)
}

#' @export
nobs.gbm <- function(object, ...) {
  n_obs(object, ...)
}



#' @export
n_obs.glimML <- function(x, ...) {
  nrow(x@data)
}

#' @export
nobs.glimML <- function(object, ...) {
  n_obs(object, ...)
}



#' @export
n_obs.glmRob <- function(x, ...) {
  length(x$fitted.values)
}

#' @export
nobs.glmRob <- function(object, ...) {
  n_obs(object, ...)
}



#' @export
n_obs.gmnl <- function(x, ...) {
  x$logLik$nobs
}

#' @export
nobs.gmnl <- function(object, ...) {
  n_obs(object, ...)
}




#' @export
n_obs.multinom <- function(x, ...) {
  nrow(x$fitted.values)
}

#' @export
nobs.multinom <- function(object, ...) {
  n_obs(object, ...)
}



#' @export
n_obs.rq <- function(x, ...) {
  length(x$fitted.values)
}

#' @export
nobs.rq <- function(object, ...) {
  n_obs(object, ...)
}



#' @export
n_obs.BBreg <- function(x, ...) {
  x$nObs
}

#' @export
nobs.BBreg <- function(object, ...) {
  n_obs(object, ...)
}




#' @export
n_obs.BBmm <- function(x, ...) {
  x$nObs
}

#' @export
nobs.BBmm <- function(object, ...) {
  n_obs(object, ...)
}




#' @export
n_obs.crq <- function(x, ...) {
  n <- nrow(x$residuals)
  if (.is_empty_object(n)) {
    n <- nrow(x$fitted.values)
  }
  n
}

#' @export
nobs.crq <- function(object, ...) {
  n_obs(object, ...)
}


#' @importFrom stats fitted
#' @export
n_obs.nlrq <- function(x, ...) {
  length(stats::fitted(x))
}

#' @export
nobs.nlrq <- function(object, ...) {
  n_obs(object, ...)
}



#' @export
n_obs.survfit <- function(x, ...) {
  length(x$n.event)
}

#' @export
nobs.survfit <- function(object, ...) {
  n_obs(object, ...)
}




#' @export
n_obs.survreg <- function(x, ...) {
  length(x$y)
}

#' @export
nobs.survreg <- function(object, ...) {
  n_obs(object, ...)
}




#' @export
n_obs.aareg <- function(x, ...) {
  max(x$n)
}


#' @export
n_obs.coxph <- function(x, ...) {
  max(x$n)
}

#' @export
nobs.coxph <- function(object, ...) {
  n_obs(object, ...)
}




#' @export
n_obs.coxme <- function(x, ...) {
  max(x$n)
}

#' @export
nobs.coxme <- function(object, ...) {
  n_obs(object, ...)
}




#' @export
n_obs.felm <- function(x, ...) {
  x$N
}

#' @export
nobs.felm <- function(object, ...) {
  n_obs(object, ...)
}




#' @export
n_obs.feis <- function(x, ...) {
  length(x$fitted.values)
}

#' @export
nobs.feis <- function(object, ...) {
  n_obs(object, ...)
}



#' @export
n_obs.aovlist <- function(x, ...) {
  nrow(stats::model.frame(x))
}

#' @export
nobs.aovlist <- function(object, ...) {
  n_obs(object, ...)
}



#' @rdname n_obs
#' @export
n_obs.stanmvreg <- function(x, select = NULL, ...) {
  n <- min(x$n_yobs)
  if (!is.null(select)) {
    if (select %in% names(x$n_yobs)) {
      n <- x$n_yobs[select]
    } else {
      print_color(sprintf("Could not find response '%s'. Model's response variables are named %s.\n", select, paste(names(x$n_yobs), collapse = ", ")), "red")
      cat("Returning smallest number of observations now.\n")
      n <- min(x$n_yobs)
    }
  }
  n
}



#' @export
n_obs.mlogit <- function(x, ...) {
  nrow(x$model)
}

#' @export
nobs.mlogit <- function(object, ...) {
  n_obs(object, ...)
}




#' @export
n_obs.hurdle <- function(x, ...) {
  x$n
}

#' @export
nobs.hurdle <- function(object, ...) {
  n_obs(object, ...)
}



#' @export
n_obs.zerotrunc <- function(x, ...) {
  x$n
}

#' @export
nobs.zerotrunc <- function(object, ...) {
  n_obs(object, ...)
}




#' @export
n_obs.zeroinfl <- function(x, ...) {
  x$n
}

#' @export
nobs.zeroinfl <- function(object, ...) {
  n_obs(object, ...)
}




#' @export
n_obs.wbm <- function(x, ...) {
  nrow(x@frame)
}
