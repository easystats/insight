#' @title Get the data that was used to fit the model
#' @name get_data
#'
#' @description This functions tries to get the data that was used to fit the
#'   model and returns it as data frame.
#'
#' @param effects Should model data for fixed effects, random effects
#'    or both be returned? Only applies to mixed models.
#'
#' @inheritParams find_predictors
#' @inheritParams find_formula
#'
#' @return The data that was used to fit the model.
#'
#' @note Unlike \code{model.frame()}, which may contain transformed variables
#'   (e.g. if \code{poly()} or \code{scale()} was used inside the formula to
#'   specify the model), \code{get_data()} aims at returning the "original",
#'   untransformed data.
#'
#' @examples
#' data(cbpp, package = "lme4")
#' cbpp$trials <- cbpp$size - cbpp$incidence
#' m <- glm(cbind(incidence, trials) ~ period, data = cbpp, family = binomial)
#' head(get_data(m))
#' @importFrom stats model.frame na.omit
#' @export
get_data <- function(x, ...) {
  UseMethod("get_data")
}


# default method ------------------------------------------------------


#' @export
get_data.default <- function(x, ...) {
  if (inherits(x, "list") && .obj_has_name(x, "gam")) {
    x <- x$gam
    class(x) <- c(class(x), c("glm", "lm"))
  }

  mf <- tryCatch(
    {
      if (inherits(x, "Zelig-relogit")) {
        .get_zelig_relogit_frame(x)
      } else {
        stats::model.frame(x)
      }
    },
    error = function(x) {
      NULL
    }
  )

  if (is.null(mf)) {
    mf <- tryCatch(
      {
        .get_data_from_env(x)[, find_variables(x, flatten = TRUE), drop = FALSE]
      },
      error = function(x) {
        NULL
      }
    )
  }

  .prepare_get_data(x, mf)
}


#' @export
get_data.data.frame <- function(x, ...) {
  x
}






# classical and survival models -----------------------------------------------


#' @rdname get_data
#' @export
get_data.gee <- function(x, effects = c("all", "fixed", "random"), ...) {
  effects <- match.arg(effects)
  mf <- tryCatch(
    {
      dat <- .get_data_from_env(x)
      switch(
        effects,
        all = dat[, find_variables(x, flatten = TRUE), drop = FALSE],
        fixed = dat[, find_variables(x, effects = "fixed", flatten = TRUE), drop = FALSE],
        random = dat[, find_random(x, flatten = TRUE), drop = FALSE]
      )
    },
    error = function(x) {
      NULL
    }
  )

  .prepare_get_data(x, stats::na.omit(mf))
}

#' @rdname get_data
#' @export
get_data.rqss <- get_data.gee



#' @export
get_data.gls <- function(x, ...) {
  mf <- tryCatch(
    {
      dat <- .get_data_from_env(x)
      data_columns <- intersect(colnames(dat), find_variables(x, flatten = TRUE))
      dat[, data_columns, drop = FALSE]
    },
    error = function(x) {
      NULL
    }
  )

  .prepare_get_data(x, stats::na.omit(mf))
}

#' @export
get_data.survfit <- get_data.gls

#' @export
get_data.aareg <- get_data.gls

#' @export
get_data.complmrob <- get_data.gls

#' @export
get_data.nlrq <- get_data.gls









# zero-inflated models -------------------------------------------------------


#' @rdname get_data
#' @export
get_data.hurdle <- function(x, component = c("all", "conditional", "zi", "zero_inflated", "dispersion"), ...) {
  component <- match.arg(component)
  .return_zeroinf_data(x, component)
}

#' @export
get_data.zeroinfl <- get_data.hurdle

#' @export
get_data.zerotrunc <- get_data.hurdle








# mixed models -------------------------------------------------------------


#' @rdname get_data
#' @export
get_data.glmmTMB <- function(x, effects = c("all", "fixed", "random"), component = c("all", "conditional", "zi", "zero_inflated", "dispersion"), ...) {
  effects <- match.arg(effects)
  component <- match.arg(component)

  model.terms <- find_variables(x, effects = "all", component = "all", flatten = FALSE)

  mf <- tryCatch(
    {
      stats::model.frame(x)
    },
    error = function(x) {
      NULL
    }
  )

  mf <- .prepare_get_data(x, mf)

  # add variables from other model components
  mf <- .add_zeroinf_data(x, mf, model.terms$dispersion)
  mf <- .add_zeroinf_data(x, mf, model.terms$zero_inflated)
  mf <- .add_zeroinf_data(x, mf, model.terms$zero_inflated_random)

  .return_data(x, mf, effects, component, model.terms)
}



#' @rdname get_data
#' @export
get_data.merMod <- function(x, effects = c("all", "fixed", "random"), ...) {
  effects <- match.arg(effects)

  mf <- tryCatch(
    {
      switch(
        effects,
        fixed = stats::model.frame(x, fixed.only = TRUE),
        all = stats::model.frame(x, fixed.only = FALSE),
        random = stats::model.frame(x, fixed.only = FALSE)[, find_random(x, split_nested = TRUE, flatten = TRUE), drop = FALSE]
      )
    },
    error = function(x) {
      NULL
    }
  )

  .prepare_get_data(x, mf, effects)
}



#' @rdname get_data
#' @export
get_data.glmmadmb <- function(x, effects = c("all", "fixed", "random"), ...) {
  effects <- match.arg(effects)

  fixed_data <- x$frame
  random_data <- .get_data_from_env(x)[, find_random(x, split_nested = TRUE, flatten = TRUE), drop = FALSE]

  mf <- tryCatch(
    {
      switch(
        effects,
        fixed = fixed_data,
        all = cbind(fixed_data, random_data),
        random = random_data
      )
    },
    error = function(x) {
      NULL
    }
  )

  .prepare_get_data(x, mf, effects)
}



#' @rdname get_data
#' @export
get_data.rlmerMod <- function(x, effects = c("all", "fixed", "random"), ...) {
  effects <- match.arg(effects)
  .get_data_from_modelframe(x, stats::model.frame(x), effects)
}

#' @rdname get_data
#' @export
get_data.clmm <- get_data.rlmerMod

#' @rdname get_data
#' @export
get_data.mixed <- function(x, effects = c("all", "fixed", "random"), ...) {
  effects <- match.arg(effects)
  .get_data_from_modelframe(x, x$data, effects)
}



#' @rdname get_data
#' @export
get_data.lme <- function(x, effects = c("all", "fixed", "random"), ...) {
  effects <- match.arg(effects)
  dat <- tryCatch(
    {
      x$data
    },
    error = function(x) {
      NULL
    }
  )

  stats::na.omit(.get_data_from_modelframe(x, dat, effects))
}



#' @rdname get_data
#' @export
get_data.MixMod <- function(x, effects = c("all", "fixed", "random"), component = c("all", "conditional", "zi", "zero_inflated", "dispersion"), ...) {
  effects <- match.arg(effects)
  component <- match.arg(component)

  tryCatch(
    {
      fitfram <- x$model_frames$mfX
      if (!.is_empty_object(x$model_frames$mfZ)) {
        fitfram <- .merge_dataframes(x$model_frames$mfZ, fitfram, replace = TRUE)
      }
      if (!.is_empty_object(x$model_frames$mfX_zi)) {
        fitfram <- .merge_dataframes(x$model_frames$mfX_zi, fitfram, replace = TRUE)
      }
      if (!.is_empty_object(x$model_frames$mfZ_zi)) {
        fitfram <- .merge_dataframes(x$model_frames$mfZ_zi, fitfram, replace = TRUE)
      }

      fitfram$grp__id <- unlist(x$id)
      colnames(fitfram)[ncol(fitfram)] <- x$id_name[1]

      # test...
      fitfram <- .prepare_get_data(x, fitfram)

      model.terms <- find_variables(x, effects = "all", component = "all", flatten = FALSE)
      .return_data(x, mf = fitfram, effects, component, model.terms)
    },
    error = function(x) {
      NULL
    }
  )
}



#' @export
get_data.BBmm <- function(x, effects = c("all", "fixed", "random"), ...) {
  effects <- match.arg(effects)
  mf <- tryCatch(
    {
      dat <- .get_data_from_env(x)[, find_variables(x, flatten = TRUE), drop = FALSE]
      switch(
        effects,
        all = dat[, find_variables(x, flatten = TRUE), drop = FALSE],
        fixed = dat[, find_variables(x, effects = "fixed", flatten = TRUE), drop = FALSE],
        random = dat[, find_random(x, flatten = TRUE), drop = FALSE]
      )
    },
    error = function(x) {
      x$X
    }
  )

  .prepare_get_data(x, stats::na.omit(mf))
}



#' @export
get_data.glimML <- function(x, effects = c("all", "fixed", "random"), ...) {
  effects <- match.arg(effects)
  dat <- x@data
  mf <- switch(
    effects,
    all = dat[, find_variables(x, flatten = TRUE), drop = FALSE],
    fixed = dat[, find_variables(x, effects = "fixed", flatten = TRUE), drop = FALSE],
    random = dat[, find_random(x, flatten = TRUE), drop = FALSE]
  )

  .prepare_get_data(x, stats::na.omit(mf))
}





# sem models -------------------------------------

#' @export
get_data.lavaan <- function(x, ...) {
  mf <- tryCatch(
    {
      .get_S4_data_from_env(x)
    },
    error = function(x) {
      NULL
    }
  )

  .prepare_get_data(x, stats::na.omit(mf))
}

#' @export
get_data.blavaan <- get_data.lavaan







# additive models (gam) -------------------------------------

#' @export
get_data.vgam <- function(x, ...) {
  mf <- tryCatch(
    {
      get(x@misc$dataname, envir = parent.frame())[, find_variables(x, flatten = TRUE), drop = FALSE]
    },
    error = function(x) {
      NULL
    }
  )

  .prepare_get_data(x, mf)
}


#' @export
get_data.gamm <- function(x, ...) {
  x <- x$gam
  class(x) <- c(class(x), c("glm", "lm"))
  mf <- stats::model.frame(x)
  .prepare_get_data(x, mf)
}


#' @export
get_data.gamlss <- function(x, ...) {
  mf <- tryCatch(
    {
      elements <- c("mu", "sigma", "nu", "tau")
      mf_list <- .compact_list(lapply(elements, function(e) {
        if (paste0(e, ".x") %in% names(x)) {
          stats::model.frame(x, what = e)
        } else {
          NULL
        }
      }))

      mf_data <- mf_list[[1]]

      if (length(mf_list) > 1) {
        for (i in 2:length(mf_list)) {
          cn <- setdiff(colnames(mf_list[[i]]), colnames(mf_data))
          if (length(cn)) mf_data <- cbind(mf_data, mf_list[[i]][, cn, drop = FALSE])
        }
      }

      mf_data
    },
    error = function(x) {
      NULL
    }
  )

  .prepare_get_data(x, mf, effects = "all")
}




# fixed effects and panel regression --------------------------------------

#' @export
get_data.felm <- function(x, effects = c("all", "fixed", "random"), ...) {
  effects <- match.arg(effects)
  .get_data_from_modelframe(x, stats::model.frame(x), effects)
}


#' @export
get_data.feis <- function(x, effects = c("all", "fixed", "random"), ...) {
  effects <- match.arg(effects)
  mf <- tryCatch(
    {
      .get_data_from_env(x)
    },
    error = function(x) {
      stats::model.frame(x)
    }
  )

  .get_data_from_modelframe(x, mf, effects)
}


#' @export
get_data.fixest <- function(x, ...) {
  mf <- .get_data_from_env(x)
  .get_data_from_modelframe(x, mf, effects = "all")
}


#' @export
get_data.plm <- function(x, ...) {
  mf <- stats::model.frame(x)
  model_terms <- find_variables(x, effects = "all", component = "all", flatten = TRUE)
  cn <- colnames(mf)
  mf <- as.data.frame(lapply(mf, as.vector))
  colnames(mf) <- clean_names(cn)

  # find index variables
  index <- eval(parse(text = .safe_deparse(x$call))[[1]]$index)

  # try to get index variables from orignal data
  if (!is.null(index)) {
    original_data <- .get_data_from_env(x)
    keep <- intersect(index, colnames(original_data))
    if (length(keep)) {
      mf <- cbind(mf, original_data[, keep, drop = FALSE])
      model_terms <- c(model_terms, keep)
    }
  }

  .prepare_get_data(x, mf[, model_terms, drop = FALSE])
}


#' @export
get_data.wbm <- function(x, effects = c("all", "fixed", "random"), ...) {
  effects <- match.arg(effects)
  mf <- stats::model.frame(x)

  # dat <- as.data.frame(x@orig_data)

  if (effects == "random") {
    return(stats::na.omit(mf[, unique(find_random(x, split_nested = TRUE, flatten = TRUE)), drop = FALSE]))
  }

  resp.col <- which(colnames(mf) == find_response(x))
  mf <- mf[, c(resp.col, (1:ncol(mf))[-resp.col])]

  .prepare_get_data(x, stats::na.omit(mf))
}


#' @export
get_data.wbgee <- get_data.wbm


#' @export
get_data.ivreg <- function(x, ...) {
  mf <- stats::model.frame(x)
  cn <- clean_names(colnames(mf))
  ft <- find_variables(x, flatten = TRUE)

  remain <- setdiff(ft, cn)

  if (.is_empty_object(remain)) {
    final_mf <- mf
  } else {
    final_mf <- tryCatch(
      {
        dat <- .get_data_from_env(x)
        cbind(mf, dat[, remain, drop = FALSE])
      },
      error = function(x) {
        NULL
      }
    )
  }

  .prepare_get_data(x, stats::na.omit(final_mf))
}

#' @export
get_data.iv_robust <- get_data.ivreg








# Bayesian regression ---------------------------------------------------


#' @rdname get_data
#' @export
get_data.brmsfit <- function(x, effects = c("all", "fixed", "random"), component = c("all", "conditional", "zi", "zero_inflated"), ...) {
  effects <- match.arg(effects)
  component <- match.arg(component)

  model.terms <- find_variables(x, effects = "all", component = "all", flatten = FALSE)
  mf <- stats::model.frame(x)

  if (.is_multi_membership(x)) {
    model.terms <- lapply(model.terms, .clean_brms_mm)
    rs <- setdiff(unname(unlist(find_random_slopes(x))), unname(unlist(model.terms)))
    if (!.is_empty_object(rs)) model.terms$random <- c(rs, model.terms$random)
  }

  .return_data(
    x,
    .prepare_get_data(x, mf, effects = effects),
    effects,
    component,
    model.terms,
    is_mv = is_multivariate(x)
  )
}



#' @rdname get_data
#' @export
get_data.stanreg <- function(x, effects = c("all", "fixed", "random"), ...) {
  effects <- match.arg(effects)

  model.terms <- find_variables(x, effects = "all", component = "all", flatten = FALSE)
  mf <- stats::model.frame(x)

  .return_data(
    x,
    .prepare_get_data(x, mf, effects = effects),
    effects,
    component = "all",
    model.terms,
    is_mv = is_multivariate(x)
  )
}



#' @export
get_data.BFBayesFactor <- function(x, ...) {
  x@data
}



#' @rdname get_data
#' @export
get_data.MCMCglmm <- function(x, effects = c("all", "fixed", "random"), ...) {
  effects <- match.arg(effects)
  mf <- tryCatch(
    {
      env_dataframes <- names(which(unlist(eapply(.GlobalEnv, is.data.frame))))
      pv <- find_predictors(x, effects = effects, component = "all", flatten = TRUE)
      matchframe <- unlist(lapply(env_dataframes, function(.x) {
        dat <- get(.x)
        all(pv %in% colnames(dat))
      }))
      mf <- env_dataframes[matchframe][1]
      if (!is.na(mf)) {
        dat <- get(mf)
        switch(
          effects,
          fixed = dat[, setdiff(colnames(dat), find_random(x, flatten = T)), drop = FALSE],
          all = dat,
          random = dat[, find_random(x, flatten = TRUE), drop = FALSE]
        )
      } else {
        NULL
      }
    },
    error = function(x) {
      NULL
    }
  )

  .prepare_get_data(x, mf, effects = effects)
}



#' @export
get_data.stanmvreg <- function(x, ...) {
  mf <- tryCatch(
    {
      out <- data.frame()
      for (i in stats::model.frame(x)) {
        out <- .merge_dataframes(out, i)
      }

      out
    },
    error = function(x) {
      NULL
    }
  )

  .prepare_get_data(x, mf)
}






# other models ------------------------------------------------------


#' @export
get_data.vglm <- function(x, ...) {
  mf <- tryCatch(
    {
      if (!length(x@model)) {
        env <- environment(x@terms$terms)
        if (is.null(env)) env <- parent.frame()
        fcall <- x@call
        fcall$method <- "model.frame"
        fcall$smart <- FALSE
        eval(fcall, env, parent.frame())
      } else {
        x@model
      }
    },
    error = function(x) {
      NULL
    }
  )

  .prepare_get_data(x, mf)
}


#' @export
get_data.biglm <- function(x, ...) {
  mf <- stats::model.frame(x)
  .prepare_get_data(x, mf)
}

#' @export
get_data.bigglm <- get_data.biglm



#' @export
get_data.LORgee <- function(x, effects = c("all", "fixed", "random"), ...) {
  effects <- match.arg(effects)
  mf <- tryCatch(
    {
      dat <- .get_data_from_env(x)[, find_variables(x, flatten = TRUE), drop = FALSE]
      switch(
        effects,
        all = dat[, find_variables(x, flatten = TRUE), drop = FALSE],
        fixed = dat[, find_variables(x, effects = "fixed", flatten = TRUE), drop = FALSE],
        random = dat[, find_random(x, flatten = TRUE), drop = FALSE]
      )
    },
    error = function(x) {
      stats::model.frame(x)
    }
  )

  .prepare_get_data(x, stats::na.omit(mf))
}



#' @export
get_data.gmnl <- function(x, ...) {
  mf <- tryCatch(
    {
      x$mf
    },
    error = function(x) {
      NULL
    }
  )

  .prepare_get_data(x, mf)
}



#' @export
get_data.gbm <- function(x, ...) {
  mf <- tryCatch(
    {
      get(.safe_deparse(x$call$data), envir = parent.frame())[, find_variables(x, flatten = TRUE), drop = FALSE]
    },
    error = function(x) {
      stats::model.frame(x)
    }
  )

  .get_data_from_modelframe(x, mf, effects = "all")
}



#' @export
get_data.tobit <- function(x, ...) {
  dat <- .get_data_from_env(x)
  ft <- find_variables(x, flatten = TRUE)
  remain <- intersect(ft, colnames(dat))

  .prepare_get_data(x, stats::na.omit(dat[, remain, drop = FALSE]))
}



#' @export
get_data.clm2 <- function(x, ...) {
  mf <- tryCatch(
    {
      x$location
    },
    error = function(x) {
      NULL
    }
  )

  .prepare_get_data(x, mf)
}



#' @export
get_data.bracl <- function(x, ...) {
  mf <- stats::model.frame(x)
  suppressWarnings(.prepare_get_data(x, mf))
}



#' @export
get_data.rma <- function(x, ...) {
  mf <- tryCatch(
    {
      .get_data_from_env(x)
    },
    error = function(x) {
      NULL
    }
  )

  .prepare_get_data(x, stats::na.omit(mf))
}
