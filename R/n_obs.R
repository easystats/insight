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


#' @export
n_obs.censReg <- n_obs.default


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
n_obs.bayesx <- function(x, ...) {
  length(x$response)
}



#' @export
n_obs.flexsurvreg <- function(x, ...) {
  x$N
}



#' @export
n_obs.bamlss <- function(x, ...) {
  nrow(x$model.frame)
}



#' @export
n_obs.lmRob <- function(x, ...) {
  length(x$fitted.values)
}



#' @export
n_obs.LORgee <- function(x, ...) {
  x$nobs
}



#' @export
n_obs.mcmc <- function(x, ...) {
  nrow(as.data.frame(x))
}



#' @export
n_obs.biglm <- function(x, ...) {
  x$n
}

#' @export
n_obs.bigglm <- n_obs.biglm

#' @export
n_obs.rqss <- n_obs.biglm

#' @export
n_obs.hurdle <- n_obs.biglm

#' @export
n_obs.zerotrunc <- n_obs.biglm

#' @export
n_obs.zeroinfl <- n_obs.biglm





#' @export
n_obs.cgam <- function(x, ...) {
  nrow(get_data(x))
}

#' @export
n_obs.cglm <- n_obs.cgam



#' @export
n_obs.gbm <- function(x, ...) {
  length(x$fit)
}



#' @export
n_obs.glimML <- function(x, ...) {
  nrow(x@data)
}



#' @export
n_obs.glmRob <- function(x, ...) {
  length(x$fitted.values)
}



#' @export
n_obs.gmnl <- function(x, ...) {
  x$logLik$nobs
}



#' @export
n_obs.multinom <- function(x, ...) {
  nrow(x$fitted.values)
}



#' @export
n_obs.cpglmm <- function(x, ...) {
  nrow(x@frame)
}



#' @export
n_obs.cpglm <- function(x, ...) {
  nrow(x$model.frame)
}

#' @export
n_obs.zcpglm <- n_obs.cpglm

#' @export
n_obs.bcplm <- n_obs.cpglm


#' @export
n_obs.rq <- function(x, ...) {
  length(x$fitted.values)
}



#' @export
n_obs.BBreg <- function(x, ...) {
  x$nObs
}

#' @export
n_obs.BBmm <- n_obs.BBreg



#' @export
n_obs.crq <- function(x, ...) {
  n <- nrow(x$residuals)
  if (.is_empty_object(n)) {
    n <- nrow(x$fitted.values)
  }
  n
}

#' @export
n_obs.crqs <- n_obs.crq


#' @export
n_obs.MANOVA <- function(x, ...) {
  nrow(x$input$data)
}

#' @export
n_obs.RM <- n_obs.MANOVA



#' @importFrom stats fitted
#' @export
n_obs.nlrq <- function(x, ...) {
  length(stats::fitted(x))
}



#' @export
n_obs.survfit <- function(x, ...) {
  length(x$n.event)
}



#' @export
n_obs.survreg <- function(x, ...) {
  length(x$linear.predictors)
}



#' @export
n_obs.aareg <- function(x, ...) {
  max(x$n)
}

#' @export
n_obs.coxph <- n_obs.aareg


#' @export
n_obs.coxme <- n_obs.aareg



#' @export
n_obs.felm <- function(x, ...) {
  x$N
}



#' @export
n_obs.feis <- function(x, ...) {
  length(x$fitted.values)
}



#' @export
n_obs.fixest <- function(x, ...) {
  x$nobs
}



#' @export
n_obs.feglm <- function(x, ...) {
  x$nobs[["nobs"]]
}

#' @export
n_obs.bife <- n_obs.feglm

#' @export
n_obs.complmrob <- n_obs.cgam


#' @export
n_obs.aovlist <- function(x, ...) {
  nrow(stats::model.frame(x))
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
n_obs.maxLik <- n_obs.mlogit


#' @export
n_obs.wbm <- function(x, ...) {
  nrow(x@frame)
}



#' @export
n_obs.wbgee <- function(x, ...) {
  stats::nobs(x)
}
