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
#' @export
n_obs <- function(x, ...) {
  UseMethod("n_obs")
}


#' @export
n_obs.default <- function(x, ...) {
  is_binomial <- tryCatch(
    {
      fam <- stats::family(x)
      fam$family == "binomial"
    },
    error = function(e) {
      FALSE
    }
  )

  if (isTRUE(is_binomial)) {
    return(n_obs.glm(x, ...))
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
n_obs.glm <- function(x, ...) {
  is_binomial <- tryCatch(
    {
      fam <- stats::family(x)
      fam$family == "binomial"
    },
    error = function(e) {
      FALSE
    }
  )

  .nobs <- stats::nobs(x)

  if (isTRUE(is_binomial)) {
    resp <- deparse(stats::formula(x)[[2]])
    resp_data <- get_response(x, verbose = FALSE)

    # response is a matrix of numbers of trials and successes
    if (grepl("^cbind\\(", resp)) {
      trials <- trimws(sub("cbind\\((.*),(.*)\\)", "\\2", resp))
      if (grepl("-", trials, fixed = TRUE)) {
        .nobs <- sum(resp_data[[2]])
      } else {
        .nobs <- sum(resp_data)
      }

      # response is a fraction
    } else if (!is.data.frame(resp_data) && .is.fraction(resp_data)) {
      .nobs <- sum(get_weights(x))
    }
  }

  .nobs
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
n_obs.svy_vglm <- function(x, ...) {
  n_obs(x$fit)
}


#' @export
n_obs.model_fit <- n_obs.svy_vglm


#' @export
n_obs.gam <- function(x, ...) {
  if (!is.null(dim(x$y))) {
    dim(x$y)[1]
  } else {
    length(x$y)
  }
}

#' @export
n_obs.gamm <- function(x, ...) {
  if (.obj_has_name(x, "gam")) {
    n_obs(x$gam, ...)
  } else {
    stop("Cannot find n_obs for this object. Please an open an issue!")
  }
}

#' @export
n_obs.list <- n_obs.gamm


#' @export
n_obs.lavaan <- function(x, ...) {
  x@SampleStats@ntotal
}


#' @export
n_obs.selection <- function(x, type = c("all", "observed", "censored"), ...) {
  type <- match.arg(type)
  s <- summary(x)
  switch(type,
    "all" = s$param$nObs,
    "observed" = s$param$N1,
    s$param$N0
  )
}


#' @export
n_obs.mjoint <- function(x, ...) {
  nrow(x$data[[1]])
}


#' @export
n_obs.joint <- function(x, ...) {
  nrow(x$data$longitudinal)
}


#' @export
n_obs.merModList <- function(x, ...) {
  stats::nobs(x[[1]])
}



#' @export
n_obs.summary.lm <- function(x, ...) {
  length(x$residuals)
}



#' @export
n_obs.mediate <- function(x, ...) {
  x$nobs
}


#' @export
n_obs.garch <- function(x, ...) {
  x$n.used
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
n_obs.SemiParBIV <- function(x, ...) {
  x$n
}



#' @export
n_obs.ivprobit <- function(x, ...) {
  nrow(x$mr1)
}



#' @export
n_obs.mvord <- function(x, ...) {
  x$rho$n
}



#' @export
n_obs.bamlss <- function(x, ...) {
  nrow(x$model.frame)
}



#' @export
n_obs.coeftest <- function(x, ...) {
  attributes(x)$nobs
}



#' @export
n_obs.lmRob <- function(x, ...) {
  length(x$fitted.values)
}



#' @export
n_obs.lqmm <- function(x, ...) {
  x$nobs
}

#' @export
n_obs.lqm <- n_obs.lqmm




#' @export
n_obs.sem <- function(x, ...) {
  if (!.is_semLme(x)) {
    return(NULL)
  }
  length(x$original.y)
}



#' @export
n_obs.LORgee <- function(x, ...) {
  x$nobs
}



#' @export
n_obs.crr <- function(x, ...) {
  x$n
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
n_obs.eglm <- n_obs.biglm

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
#' @rdname n_obs
#' @inheritParams get_data
n_obs.afex_aov <- function(x, shape = c("long", "wide"), ...) {
  nrow(get_data(x, shape = shape))
}


#' @export
n_obs.glimML <- function(x, ...) {
  nrow(x@data)
}



#' @export
n_obs.mle2 <- function(x, ...) {
  nrow(get_data(x))
}

#' @export
n_obs.mle <- n_obs.mle2



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
n_obs.lmodel2 <- function(x, ...) {
  nrow(get_data(x))
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
n_obs.ivFixed <- n_obs.rq

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
n_obs.comprisk <- function(x, ...) {
  x$n
}


#' @export
n_obs.riskRegression <- function(x, ...) {
  nrow(x$response)
}


#' @export
n_obs.MANOVA <- function(x, ...) {
  nrow(x$input$data)
}

#' @export
n_obs.RM <- n_obs.MANOVA



#' @export
n_obs.nlrq <- function(x, ...) {
  length(stats::fitted(x))
}



#' @export
n_obs.survfit <- function(x, ...) {
  length(x$n.event)
}



#' @export
n_obs.mhurdle <- function(x, ...) {
  nrow(x$model)
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
n_obs.coxr <- function(x, ...) {
  nrow(x$y)
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
n_obs.averaging <- function(x, ...) {
  attr(x, "nobs")
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
n_obs.blrm <- function(x, ...) {
  x$N
}


#' @export
n_obs.mlogit <- function(x, ...) {
  nrow(x$model)
}


#' @export
n_obs.Glm <- n_obs.mlogit


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


#' @export
n_obs.Rchoice <- function(x, ...) {
  nrow(x$mf)
}





# mfx models --------------------------------------

#' @export
n_obs.betamfx <- function(x, ...) {
  stats::nobs(x$fit)
}

#' @export
n_obs.betaor <- n_obs.betamfx

#' @export
n_obs.logitmfx <- n_obs.betamfx

#' @export
n_obs.poissonmfx <- n_obs.betamfx

#' @export
n_obs.probitmfx <- n_obs.betamfx

#' @export
n_obs.negbinmfx <- n_obs.betamfx

#' @export
n_obs.negbinirr <- n_obs.betamfx

#' @export
n_obs.poissonirr <- n_obs.betamfx

#' @export
n_obs.logitor <- n_obs.betamfx





# special models -----------

#' @export
n_obs.mipo <- function(x, ...) {
  x$glanced$nobs
}


#' @export
n_obs.mira <- function(x, ...) {
  if (!requireNamespace("mice", quietly = TRUE)) {
    stop("Package `mice` required. Please install it.", call. = FALSE)
  }
  n_obs(mice::pool(x), ...)
}


#' @export
n_obs.emm_list <- function(x, ...) {
  NULL
}
