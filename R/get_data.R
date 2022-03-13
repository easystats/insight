#' @title Get the data that was used to fit the model
#' @name get_data
#'
#' @description This functions tries to get the data that was used to fit the
#'   model and returns it as data frame.
#'
#' @param effects Should model data for fixed effects, random effects
#'   or both be returned? Only applies to mixed models.
#' @param verbose Toggle messages and warnings.
#'
#' @inheritParams find_predictors
#' @inheritParams find_formula
#'
#' @return The data that was used to fit the model.
#'
#' @note Unlike `model.frame()`, which may contain transformed variables
#'   (e.g. if `poly()` or `scale()` was used inside the formula to
#'   specify the model), `get_data()` aims at returning the "original",
#'   untransformed data (if possible). Consequently, column names are changed
#'   accordingly, i.e. `"log(x)"` will become `"x"` etc. for all data
#'   columns with transformed values.
#'
#' @examples
#' if (require("lme4")) {
#'   data(cbpp, package = "lme4")
#'   cbpp$trials <- cbpp$size - cbpp$incidence
#'   m <- glm(cbind(incidence, trials) ~ period, data = cbpp, family = binomial)
#'   head(get_data(m))
#' }
#' @export
get_data <- function(x, ...) {
  UseMethod("get_data")
}


# default method ------------------------------------------------------


#' @export
get_data.default <- function(x, verbose = TRUE, ...) {
  if (inherits(x, "list") && object_has_names(x, "gam")) {
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
        dat <- .recover_data_from_environment(x)
        vars <- find_variables(x, flatten = TRUE, verbose = FALSE)
        dat[, intersect(vars, colnames(dat)), drop = FALSE]
      },
      error = function(x) {
        NULL
      }
    )
  }

  .prepare_get_data(x, mf, verbose = verbose)
}


#' @export
get_data.data.frame <- function(x, ...) {
  x
}


#' @export
get_data.summary.lm <- function(x, verbose = TRUE, ...) {
  mf <- tryCatch(
    {
      .recover_data_from_environment(x)[, all.vars(x$terms), drop = FALSE]
    },
    error = function(x) {
      NULL
    }
  )
  .prepare_get_data(x, mf, verbose = verbose)
}


#' @export
get_data.model_fit <- function(x, verbose = TRUE, ...) {
  get_data(x$fit, verbose = verbose, ...)
}





# classical and survival models -----------------------------------------------


#' @export
get_data.mjoint <- function(x, verbose = TRUE, ...) {
  mf <- tryCatch(
    {
      dat <- x$data[[1]]
      data_columns <- intersect(
        colnames(dat),
        unique(c(
          find_response(x, combine = FALSE, component = "all"),
          find_variables(x, flatten = TRUE, verbose = FALSE)
        ))
      )
      dat[, data_columns, drop = FALSE]
    },
    error = function(x) {
      NULL
    }
  )

  .prepare_get_data(x, stats::na.omit(mf), verbose = verbose)
}


#' @rdname get_data
#' @export
get_data.gee <- function(x,
                         effects = c("all", "fixed", "random"),
                         verbose = TRUE,
                         ...) {
  effects <- match.arg(effects)
  mf <- tryCatch(
    {
      dat <- .recover_data_from_environment(x)
      vars <- switch(effects,
        all = find_variables(x, flatten = TRUE, verbose = FALSE),
        fixed = find_variables(x, effects = "fixed", flatten = TRUE, verbose = FALSE),
        random = find_random(x, flatten = TRUE)
      )
      dat[, intersect(vars, colnames(dat)), drop = FALSE]
    },
    error = function(x) {
      NULL
    }
  )

  .prepare_get_data(x, stats::na.omit(mf), effects = effects, verbose = verbose)
}



#' @rdname get_data
#' @export
get_data.rqss <- function(x,
                          component = c("all", "conditional", "smooth_terms"),
                          verbose = TRUE,
                          ...) {
  component <- match.arg(component)

  mf <- tryCatch(
    {
      dat <- .recover_data_from_environment(x)
      vars <- find_variables(
        x,
        effects = "all",
        component = component,
        flatten = TRUE,
        verbose = FALSE
      )
      dat[, intersect(vars, colnames(dat)), drop = FALSE]
    },
    error = function(x) {
      NULL
    }
  )

  .prepare_get_data(x, stats::na.omit(mf), verbose = verbose)
}



#' @export
get_data.gls <- function(x, verbose = TRUE, ...) {
  mf <- tryCatch(
    {
      dat <- .recover_data_from_environment(x)
      data_columns <- intersect(
        colnames(dat),
        find_variables(x, flatten = TRUE, verbose = FALSE)
      )
      dat[, data_columns, drop = FALSE]
    },
    error = function(x) {
      NULL
    }
  )

  .prepare_get_data(x, stats::na.omit(mf), verbose = verbose)
}

#' @export
get_data.survfit <- get_data.gls

#' @export
get_data.aareg <- get_data.gls

#' @export
get_data.complmrob <- get_data.gls

#' @export
get_data.nlrq <- get_data.gls

#' @export
get_data.robmixglm <- get_data.gls

#' @export
get_data.selection <- get_data.gls

# if ("lm" %in% names(x)) {
#   suppressWarnings(get_data(x$lm, verbose = verbose))
# } else if (!is.null(x$twoStep$lm)) {
#   suppressWarnings(get_data(x$twoStep$lm, verbose = verbose))
# } else {
#   NULL
# }


#' @export
get_data.lqmm <- function(x, verbose = TRUE, ...) {
  mf <- tryCatch(
    {
      x$mfArgs$data
    },
    error = function(x) {
      NULL
    }
  )

  .prepare_get_data(x, stats::na.omit(mf), verbose = verbose)
}

#' @export
get_data.nls <- get_data.gls

#' @export
get_data.gnls <- get_data.gls


# zero-inflated models -------------------------------------------------------


#' @rdname get_data
#' @export
get_data.hurdle <- function(x,
                            component = c("all", "conditional", "zi", "zero_inflated", "dispersion"),
                            verbose = TRUE,
                            ...) {
  component <- match.arg(component)
  .return_zeroinf_data(x, component, verbose = verbose)
}

#' @export
get_data.zeroinfl <- get_data.hurdle

#' @export
get_data.zerotrunc <- get_data.hurdle


#' @rdname get_data
#' @export
get_data.zcpglm <- function(x,
                            component = c("all", "conditional", "zi", "zero_inflated"),
                            verbose = TRUE,
                            ...) {
  component <- match.arg(component)

  mf <- stats::model.frame(x)
  mf_zero <- mf$zero
  mf_tweedie <- mf$tweedie

  # zcpglm saves variables twice, once in the model frame for zero-inflated
  # model and once for the tweedie-model. we now need to remove duplicates
  cn <- setdiff(colnames(mf$zero), colnames(mf$tweedie))

  if (length(cn)) {
    mf_zero <- mf_zero[cn]
  } else {
    mf_zero <- NULL
  }

  mf <- switch(component,
    "all" = do.call(cbind, compact_list(list(mf_tweedie, mf_zero))),
    "conditional" = mf_tweedie,
    "zi" = ,
    "zero_inflated" = mf_zero
  )

  .prepare_get_data(x, stats::na.omit(mf), verbose = verbose)
}


# mixed models -------------------------------------------------------------


#' @rdname get_data
#' @export
get_data.glmmTMB <- function(x,
                             effects = c("all", "fixed", "random"),
                             component = c("all", "conditional", "zi", "zero_inflated", "dispersion"),
                             verbose = TRUE,
                             ...) {
  effects <- match.arg(effects)
  component <- match.arg(component)

  model.terms <- find_variables(
    x,
    effects = "all",
    component = "all",
    flatten = FALSE,
    verbose = FALSE
  )

  mf <- tryCatch(
    {
      stats::model.frame(x)
    },
    error = function(x) {
      NULL
    }
  )

  mf <- .prepare_get_data(x, mf, effects, verbose = verbose)

  # add variables from other model components
  mf <- .add_zeroinf_data(x, mf, model.terms$dispersion)
  mf <- .add_zeroinf_data(x, mf, model.terms$zero_inflated)
  mf <- .add_zeroinf_data(x, mf, model.terms$zero_inflated_random)

  .return_combined_data(x, mf, effects, component, model.terms, verbose = verbose)
}



#' @rdname get_data
#' @export
get_data.merMod <- function(x,
                            effects = c("all", "fixed", "random"),
                            verbose = TRUE,
                            ...) {
  effects <- match.arg(effects)

  mf <- tryCatch(
    {
      switch(effects,
        fixed = stats::model.frame(x, fixed.only = TRUE),
        all = stats::model.frame(x, fixed.only = FALSE),
        random = stats::model.frame(x, fixed.only = FALSE)[, find_random(x, split_nested = TRUE, flatten = TRUE), drop = FALSE]
      )
    },
    error = function(x) {
      NULL
    }
  )

  .prepare_get_data(x, mf, effects, verbose = verbose)
}

#' @export
get_data.merModList <- function(x, effects = c("all", "fixed", "random"), ...) {
  warning("Can't access data for 'merModList' objects.", call. = FALSE)
  return(NULL)
}



#' @export
get_data.MANOVA <- function(x,
                            effects = c("all", "fixed", "random"),
                            verbose = TRUE,
                            ...) {
  effects <- match.arg(effects)

  mf <- tryCatch(
    {
      switch(effects,
        fixed = .remove_column(x$input$data, x$input$subject),
        all = x$input$data,
        random = x$input$data[, x$input$subject, drop = FALSE]
      )
    },
    error = function(x) {
      NULL
    }
  )

  .prepare_get_data(x, mf, effects, verbose = verbose)
}

#' @export
get_data.RM <- get_data.MANOVA



#' @export
get_data.cpglmm <- function(x,
                            effects = c("all", "fixed", "random"),
                            verbose = TRUE,
                            ...) {
  effects <- match.arg(effects)
  dat <- stats::model.frame(x)

  mf <- tryCatch(
    {
      switch(effects,
        fixed = dat[, find_predictors(x, effects = "fixed", flatten = TRUE, verbose = FALSE), drop = FALSE],
        all = dat,
        random = dat[, find_random(x, split_nested = TRUE, flatten = TRUE), drop = FALSE]
      )
    },
    error = function(x) {
      NULL
    }
  )

  .prepare_get_data(x, mf, effects, verbose = verbose)
}

#' @export
get_data.HLfit <- get_data.cpglmm


#' @export
get_data.glmm <- function(x,
                          effects = c("all", "fixed", "random"),
                          verbose = TRUE,
                          ...) {
  effects <- match.arg(effects)
  dat <- get_data.default(x, verbose = verbose)

  mf <- tryCatch(
    {
      switch(effects,
        fixed = dat[, find_predictors(x,
          effects = "fixed",
          flatten = TRUE,
          verbose = FALSE
        ), drop = FALSE],
        all = dat,
        random = dat[, find_random(x, split_nested = TRUE, flatten = TRUE), drop = FALSE]
      )
    },
    error = function(x) {
      NULL
    }
  )
}



#' @export
get_data.mixor <- function(x,
                           effects = c("all", "fixed", "random"),
                           verbose = TRUE,
                           ...) {
  effects <- match.arg(effects)

  mf <- tryCatch(
    {
      switch(effects,
        fixed = stats::model.frame(x),
        all = cbind(stats::model.frame(x), x$id),
        random = data.frame(x$id)
      )
    },
    error = function(x) {
      NULL
    }
  )

  fix_cn <- which(colnames(mf) %in% c("x.id", "x$id"))
  colnames(mf)[fix_cn] <- .safe_deparse(x$call$id)

  .prepare_get_data(x, mf, effects, verbose = verbose)
}



#' @rdname get_data
#' @export
get_data.glmmadmb <- function(x,
                              effects = c("all", "fixed", "random"),
                              verbose = TRUE,
                              ...) {
  effects <- match.arg(effects)

  fixed_data <- x$frame
  random_data <- .recover_data_from_environment(x)[, find_random(x, split_nested = TRUE, flatten = TRUE), drop = FALSE]

  mf <- tryCatch(
    {
      switch(effects,
        fixed = fixed_data,
        all = cbind(fixed_data, random_data),
        random = random_data
      )
    },
    error = function(x) {
      NULL
    }
  )

  .prepare_get_data(x, mf, effects, verbose = verbose)
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



#' @export
#' @rdname get_data
#' @param shape Return long or wide data? Only applicable in repeated measures
#'   designs.
get_data.afex_aov <- function(x, shape = c("long", "wide"), ...) {
  if (!length(attr(x, "within"))) {
    shape <- "long"
  } else {
    shape <- match.arg(shape)
  }
  x$data[[shape]]
}



#' @export
get_data.sem <- function(x, effects = c("all", "fixed", "random"), verbose = TRUE, ...) {
  effects <- match.arg(effects)
  mf <- tryCatch(
    {
      dat <- .recover_data_from_environment(x)
      vars <- switch(effects,
        all = find_variables(x, flatten = TRUE, verbose = FALSE),
        fixed = find_variables(x, effects = "fixed", flatten = TRUE, verbose = FALSE),
        random = find_random(x, flatten = TRUE)
      )
      dat[, intersect(vars, colnames(dat)), drop = FALSE]
    },
    error = function(x) {
      NULL
    }
  )

  .prepare_get_data(x, stats::na.omit(mf), effects, verbose = verbose)
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
get_data.MixMod <- function(x,
                            effects = c("all", "fixed", "random"),
                            component = c("all", "conditional", "zi", "zero_inflated", "dispersion"),
                            verbose = TRUE,
                            ...) {
  effects <- match.arg(effects)
  component <- match.arg(component)

  tryCatch(
    {
      fitfram <- stats::model.frame(x, type = "fixed")
      fitfram_re <- stats::model.frame(x, type = "random")
      fitfram_zi <- stats::model.frame(x, type = "zi_fixed")
      fitfram_zi_re <- stats::model.frame(x, type = "zi_random")

      if (!is_empty_object(fitfram_re)) {
        for (i in 1:length(fitfram_re)) {
          fitfram <- .merge_dataframes(fitfram_re[[i]], fitfram, replace = TRUE)
        }
      }
      if (!is_empty_object(fitfram_zi)) {
        fitfram <- .merge_dataframes(fitfram_zi, fitfram, replace = TRUE)
      }
      if (!is_empty_object(fitfram_zi_re)) {
        for (i in 1:length(fitfram_zi_re)) {
          fitfram <- .merge_dataframes(fitfram_zi_re[[i]], fitfram, replace = TRUE)
        }
      }

      fitfram$grp__id <- unlist(x$id)
      colnames(fitfram)[ncol(fitfram)] <- x$id_name[1]

      # test...
      fitfram <- .prepare_get_data(x, fitfram, effects, verbose = verbose)

      model.terms <- find_variables(
        x,
        effects = "all",
        component = "all",
        flatten = FALSE,
        verbose = FALSE
      )

      .return_combined_data(x, mf = fitfram, effects, component, model.terms, verbose = verbose)
    },
    error = function(x) {
      NULL
    }
  )
}



#' @export
get_data.BBmm <- function(x, effects = c("all", "fixed", "random"), verbose = TRUE, ...) {
  effects <- match.arg(effects)
  mf <- tryCatch(
    {
      dat <- .recover_data_from_environment(x)[, find_variables(x, flatten = TRUE), drop = FALSE]
      switch(effects,
        all = dat[, find_variables(x, flatten = TRUE), drop = FALSE],
        fixed = dat[, find_variables(x, effects = "fixed", flatten = TRUE), drop = FALSE],
        random = dat[, find_random(x, flatten = TRUE), drop = FALSE]
      )
    },
    error = function(x) {
      x$X
    }
  )

  .prepare_get_data(x, stats::na.omit(mf), effects, verbose = verbose)
}


#' @export
get_data.glimML <- function(x, effects = c("all", "fixed", "random"), verbose = TRUE, ...) {
  effects <- match.arg(effects)
  dat <- x@data
  mf <- switch(effects,
    all = dat[, find_variables(x, flatten = TRUE), drop = FALSE],
    fixed = dat[, find_variables(x, effects = "fixed", flatten = TRUE), drop = FALSE],
    random = dat[, find_random(x, flatten = TRUE), drop = FALSE]
  )

  .prepare_get_data(x, stats::na.omit(mf), effects, verbose = verbose)
}


# sem models -------------------------------------

#' @export
get_data.lavaan <- function(x, verbose = TRUE, ...) {
  mf <- tryCatch(
    {
      .get_S4_data_from_env(x)
    },
    error = function(x) {
      NULL
    }
  )

  .prepare_get_data(x, stats::na.omit(mf), verbose = verbose)
}

#' @export
get_data.blavaan <- get_data.lavaan


# additive models (gam) -------------------------------------

#' @export
get_data.vgam <- function(x, verbose = TRUE, ...) {
  mf <- tryCatch(
    {
      get(x@misc$dataname, envir = parent.frame())[, find_variables(x, flatten = TRUE), drop = FALSE]
    },
    error = function(x) {
      NULL
    }
  )

  .prepare_get_data(x, mf, verbose = verbose)
}


#' @export
get_data.gamm <- function(x, verbose = TRUE, ...) {
  x <- x$gam
  class(x) <- c(class(x), c("glm", "lm"))
  mf <- stats::model.frame(x)
  .prepare_get_data(x, mf, verbose = verbose)
}


#' @export
get_data.gamlss <- function(x, verbose = TRUE, ...) {
  mf <- tryCatch(
    {
      elements <- c("mu", "sigma", "nu", "tau")
      mf_list <- compact_list(lapply(elements, function(e) {
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

  .prepare_get_data(x, mf, effects = "all", verbose = verbose)
}




# fixed effects and panel regression --------------------------------------

#' @export
get_data.felm <- function(x, effects = c("all", "fixed", "random"), verbose = TRUE, ...) {
  effects <- match.arg(effects)
  .get_data_from_modelframe(x, stats::model.frame(x), effects, verbose = verbose)
}


#' @export
get_data.feis <- function(x, effects = c("all", "fixed", "random"), ...) {
  effects <- match.arg(effects)
  mf <- tryCatch(
    {
      .recover_data_from_environment(x)
    },
    error = function(x) {
      stats::model.frame(x)
    }
  )

  .get_data_from_modelframe(x, mf, effects)
}


#' @export
get_data.fixest <- function(x, ...) {
  mf <- .recover_data_from_environment(x)
  .get_data_from_modelframe(x, mf, effects = "all")
}


#' @export
get_data.feglm <- function(x, ...) {
  mf <- as.data.frame(x$data)
  .get_data_from_modelframe(x, mf, effects = "all")
}


#' @export
get_data.pgmm <- function(x, verbose = TRUE, ...) {
  model_terms <- find_variables(x, effects = "all", component = "all", flatten = TRUE)
  mf <- tryCatch(.recover_data_from_environment(x)[, model_terms, drop = FALSE],
    error = function(x) NULL
  )
  .prepare_get_data(x, mf, verbose = verbose)
}


#' @export
get_data.plm <- function(x, verbose = TRUE, ...) {
  mf <- stats::model.frame(x)
  model_terms <- find_variables(x, effects = "all", component = "all", flatten = TRUE)
  cn <- colnames(mf)
  mf <- as.data.frame(lapply(mf, as.vector))
  colnames(mf) <- clean_names(cn)

  # find index variables
  index <- eval(parse(text = .safe_deparse(x$call))[[1]]$index)

  # try to get index variables from orignal data
  if (!is.null(index)) {
    original_data <- .recover_data_from_environment(x)
    keep <- intersect(index, colnames(original_data))
    if (length(keep)) {
      mf <- cbind(mf, original_data[, keep, drop = FALSE])
      model_terms <- c(model_terms, keep)
    }
  }

  .prepare_get_data(x, mf[, model_terms, drop = FALSE], verbose = verbose)
}


#' @export
get_data.wbm <- function(x, effects = c("all", "fixed", "random"), verbose = TRUE, ...) {
  effects <- match.arg(effects)
  mf <- stats::model.frame(x)

  # dat <- as.data.frame(x@orig_data)

  if (effects == "random") {
    return(stats::na.omit(mf[, unique(find_random(x, split_nested = TRUE, flatten = TRUE)), drop = FALSE]))
  }

  resp.col <- which(colnames(mf) == find_response(x))
  mf <- mf[, c(resp.col, (1:ncol(mf))[-resp.col])]

  .prepare_get_data(x, stats::na.omit(mf), effects, verbose = verbose)
}


#' @export
get_data.wbgee <- get_data.wbm


#' @export
get_data.ivreg <- function(x, verbose = TRUE, ...) {
  mf <- stats::model.frame(x)
  cn <- clean_names(colnames(mf))
  ft <- find_variables(x, flatten = TRUE)

  remain <- setdiff(ft, cn)

  if (is_empty_object(remain)) {
    final_mf <- mf
  } else {
    final_mf <- tryCatch(
      {
        dat <- .recover_data_from_environment(x)
        cbind(mf, dat[, remain, drop = FALSE])
      },
      error = function(x) {
        NULL
      }
    )
  }

  .prepare_get_data(x, stats::na.omit(final_mf), verbose = verbose)
}

#' @export
get_data.iv_robust <- get_data.ivreg


#' @export
get_data.ivprobit <- function(x, verbose = TRUE, ...) {
  .prepare_get_data(x, stats::na.omit(as.data.frame(x$mr1)), verbose = verbose)
}


#' @export
get_data.bife <- function(x, effects = c("all", "fixed", "random"), verbose = TRUE, ...) {
  effects <- match.arg(effects)
  mf <- as.data.frame(x$data)

  if (effects == "random") {
    return(stats::na.omit(mf[, unique(find_random(x, split_nested = TRUE, flatten = TRUE)), drop = FALSE]))
  } else if (effects == "fixed") {
    mf <- mf[, setdiff(colnames(mf), unique(find_random(x, split_nested = TRUE, flatten = TRUE))), drop = FALSE]
  }

  .prepare_get_data(x, stats::na.omit(mf), effects, verbose = verbose)
}









# Bayesian regression ---------------------------------------------------


#' @rdname get_data
#' @export
get_data.brmsfit <- function(x,
                             effects = "all",
                             component = "all",
                             verbose = TRUE,
                             ...) {
  effects <- match.arg(effects, choices = c("all", "fixed", "random"))
  component <- match.arg(component, choices = c("all", .all_elements()))

  model.terms <- find_variables(x, effects = "all", component = "all", flatten = FALSE)
  mf <- stats::model.frame(x)

  if (.is_multi_membership(x)) {
    model.terms <- lapply(model.terms, .clean_brms_mm)
    rs <- setdiff(unname(unlist(find_random_slopes(x))), unname(unlist(model.terms)))
    if (!is_empty_object(rs)) model.terms$random <- c(rs, model.terms$random)
  }

  .return_combined_data(
    x,
    .prepare_get_data(x, mf, effects = effects, verbose = verbose),
    effects,
    component,
    model.terms,
    is_mv = is_multivariate(x),
    verbose = verbose
  )
}



#' @rdname get_data
#' @export
get_data.stanreg <- function(x,
                             effects = c("all", "fixed", "random"),
                             verbose = TRUE,
                             ...) {
  effects <- match.arg(effects)

  model.terms <- find_variables(x,
    effects = "all",
    component = "all",
    flatten = FALSE
  )

  mf <- stats::model.frame(x)

  .return_combined_data(
    x,
    .prepare_get_data(x, mf, effects = effects, verbose = verbose),
    effects,
    component = "all",
    model.terms,
    is_mv = is_multivariate(x),
    verbose = verbose
  )
}



#' @export
get_data.BFBayesFactor <- function(x, ...) {
  x@data
}



#' @rdname get_data
#' @export
get_data.MCMCglmm <- function(x, effects = c("all", "fixed", "random"), verbose = TRUE, ...) {
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
        switch(effects,
          fixed = dat[, setdiff(colnames(dat), find_random(x, flatten = TRUE)), drop = FALSE],
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

  .prepare_get_data(x, mf, effects = effects, verbose = verbose)
}



#' @export
get_data.stanmvreg <- function(x, verbose = TRUE, ...) {
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

  .prepare_get_data(x, mf, verbose = verbose)
}


# mfx models ------------------------------------------------------

#' @export
get_data.betamfx <- function(x, ...) {
  get_data(x$fit, ...)
}

#' @export
get_data.betaor <- get_data.betamfx

#' @export
get_data.logitor <- get_data.betamfx

#' @export
get_data.poissonirr <- get_data.betamfx

#' @export
get_data.negbinirr <- get_data.betamfx

#' @export
get_data.logitmfx <- get_data.betamfx

#' @export
get_data.poissonmfx <- get_data.betamfx

#' @export
get_data.probitmfx <- get_data.betamfx

#' @export
get_data.negbinmfx <- get_data.betamfx



# other models ------------------------------------------------------


#' @export
get_data.svy_vglm <- function(x, verbose = TRUE, ...) {
  mf <- x$design$variables[find_variables(x, flatten = TRUE)]
  .prepare_get_data(x, mf, verbose = verbose)
}


#' @export
get_data.mediate <- function(x, ...) {
  d1 <- get_data(x$model.m)
  d2 <- get_data(x$model.y)
  merge(d1, d2, sort = FALSE, all = TRUE)
}


#' @export
get_data.mle2 <- function(x, ...) {
  as.data.frame(do.call(cbind, x@data))
}

#' @export
get_data.mle <- get_data.mle2



#' @export
get_data.glht <- function(x, ...) {
  get_data(x$model, ...)
}


#' @export
get_data.averaging <- function(x, ...) {
  ml <- attributes(x)$modelList
  if (is.null(ml)) {
    warning("Can't retrieve data. Please use 'fit = TRUE' in 'model.avg()'.", call. = FALSE)
    return(NULL)
  }

  mf <- tryCatch(
    {
      Reduce(function(x, y) merge(x, y, all = TRUE, sort = FALSE), lapply(ml, stats::model.frame))
    },
    error = function(x) {
      NULL
    }
  )

  if (is.null(mf)) {
    mf <- tryCatch(
      {
        .recover_data_from_environment(x)[, find_variables(x, flatten = TRUE), drop = FALSE]
      },
      error = function(x) {
        NULL
      }
    )
  }

  .prepare_get_data(x, mf)
}


#' @export
get_data.Arima <- function(x, ...) {
  # first try, parent frame
  dat <- tryCatch(eval(x$call$x, envir = parent.frame()), error = function(e) NULL)

  if (is.null(dat)) {
    # second try, global env
    dat <- tryCatch(eval(x$call$x, envir = globalenv()), error = function(e) NULL)
  }

  dat
}


#' @export
get_data.coxph <- function(x, ...) {
  # first try, parent frame
  dat <- tryCatch({
    mf <- .recover_data_from_environment(x)
    mf <- .prepare_get_data(x, stats::na.omit(mf), verbose = FALSE)
    },
    error = function(x) NULL)

  # second try, default extractor. Less good because of coercion to other types
  if (is.null(dat)) {
    # second try, global env
    dat <- get_data.default(x, ...)
  }

  dat
}

#' @export
get_data.BGGM <- function(x, ...) {
  x$Y
}


#' @export
get_data.mcmc.list <- function(x, ...) {
  NULL
}


#' @export
get_data.DirichletRegModel <- function(x, verbose = TRUE, ...) {
  mf <- x$data
  resp <- sapply(x$data, inherits, "DirichletRegData")
  .prepare_get_data(x, mf[!resp], verbose = verbose)
}


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
      dat <- .recover_data_from_environment(x)[, find_variables(x, flatten = TRUE), drop = FALSE]
      switch(effects,
        all = dat[, find_variables(x, flatten = TRUE), drop = FALSE],
        fixed = dat[, find_variables(x, effects = "fixed", flatten = TRUE), drop = FALSE],
        random = dat[, find_random(x, flatten = TRUE), drop = FALSE]
      )
    },
    error = function(x) {
      stats::model.frame(x)
    }
  )

  .prepare_get_data(x, stats::na.omit(mf), effects = effects)
}



#' @export
get_data.gmnl <- function(x, ...) {
  mf <- tryCatch(x$mf, error = function(x) NULL)
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
get_data.tobit <- function(x, verbose = TRUE, ...) {
  dat <- .recover_data_from_environment(x)
  ft <- find_variables(x, flatten = TRUE, verbose = FALSE)
  remain <- intersect(ft, colnames(dat))

  .prepare_get_data(x, stats::na.omit(dat[, remain, drop = FALSE]), verbose = verbose)
}



#' @export
get_data.clmm2 <- function(x, ...) {
  mf <- tryCatch(
    {
      data_complete <- x$location
      data_scale <- x$scale

      if (!is.null(data_scale)) {
        remain <- setdiff(colnames(data_scale), colnames(data_complete))
        if (length(remain)) data_complete <- cbind(data_complete, data_scale[, remain, drop = FALSE])
      }

      data_complete <- cbind(data_complete, x$grFac)
      colnames(data_complete)[ncol(data_complete)] <- unlist(.find_random_effects(x, f = find_formula(x, verbose = FALSE), split_nested = TRUE))

      data_complete
    },
    error = function(x) {
      NULL
    }
  )

  .prepare_get_data(x, mf)
}


#' @export
get_data.clm2 <- function(x, ...) {
  mf <- tryCatch(
    {
      data_complete <- x$location
      data_scale <- x$scale

      if (!is.null(data_scale)) {
        remain <- setdiff(colnames(data_scale), colnames(data_complete))
        if (length(remain)) data_complete <- cbind(data_complete, data_scale[, remain, drop = FALSE])
      }

      data_complete
    },
    error = function(x) {
      NULL
    }
  )

  .prepare_get_data(x, mf)
}



#' @export
get_data.bracl <- function(x, verbose = TRUE, ...) {
  mf <- stats::model.frame(x)
  suppressWarnings(.prepare_get_data(x, mf, verbose = verbose))
}



#' @export
get_data.mlogit <- function(x, verbose = TRUE, ...) {
  mf <- tryCatch(as.data.frame(stats::model.frame(x)), error = function(x) NULL)
  .prepare_get_data(x, mf, verbose = verbose)
}



#' @export
#' @rdname get_data
#' @param include_interval For meta-analysis models, should normal-approximation
#'   confidence intervals be added for each response effect size?
#' @param transf For meta-analysis models, if intervals are included, a function
#'   applied to each response effect size and its interval.
#' @param transf_args For meta-analysis models, an optional list of arguments
#'   passed to the `transf` function.
#' @param ci For meta-analysis models, the Confidence Interval (CI) level if
#'   `include_interval = TRUE`. Default to 0.95 (95%).
get_data.rma <- function(x, verbose = TRUE, include_interval = FALSE, transf = NULL, transf_args = NULL, ci = .95, ...) {
  mf <- tryCatch(.recover_data_from_environment(x), error = function(x) NULL)
  mf_attr <- attributes(mf)
  mf <- merge(mf, data.frame(Weight = get_weights(mod)), by = "row.names", all = TRUE, sort = FALSE)
  rownames(mf) <- mf$Row.names
  mf$Row.names <- NULL
  mostattributes(mf) <- c(attributes(mf)[c("names", "row.names")],
                          mf_attr[c("yi.names", "vi.names", "digits", "class")])
  if (isTRUE(include_interval)) {
    model_call <-  get_call(x)
    model_response <- tryCatch(mf[[find_response(x)]], error = function(x) NULL)
    sei <- tryCatch(mf[[model_call$sei]], error = function(x) NULL)
    if (is.null(sei)) {
      sei <- tryCatch(sqrt(mf[[model_call$vi]]), error = function(x) NULL)
    }
    if (is.null(sei)) {
      stop("Could not find `sei` or `vi` for this model.", call. = FALSE)
    }
    mf$ci <- ci
    mf$CI_low <- model_response - qnorm((1 - ci) / 2, lower.tail = FALSE) * sei
    mf$CI_high <- model_response + qnorm((1 - ci) / 2, lower.tail = FALSE) * sei
    if (!is.null(transf)) {
      if (!is.function(transf)) {
        stop("`transf` must be a function.")
      }
      if (!is.null(transf_args)) {
        mf[[find_response(x)]] <- sapply(mf[[find_response(x)]], transf, transf_args)
        mf$CI_low <- sapply(mf$CI_low, transf, transf_args)
        mf$CI_high <- sapply(mf$CI_high, transf, transf_args)
      } else {
        mf[[find_response(x)]] <- sapply(mf[[find_response(x)]], transf)
        mf$CI_low <- sapply(mf$CI_low, transf)
        mf$CI_high <- sapply(mf$CI_high, transf)
      }
    }
  }
  .prepare_get_data(x, mf[rownames(x$X),], verbose = verbose)
}


#' @export
# TODO: Check these
get_data.metaplus <- get_data.rma


#' @export
# TODO: Check these
get_data.meta_random <- function(x, verbose = TRUE, ...) {
  mf <- tryCatch(x$data$data, error = function(x) NULL)
  .prepare_get_data(x, stats::na.omit(mf), verbose = verbose)
}


#' @export
# TODO: Check these
get_data.meta_bma <- function(x, verbose = TRUE, ...) {
  mf <- tryCatch(x$meta$fixed$data$data, error = function(x) NULL)
  .prepare_get_data(x, stats::na.omit(mf), verbose = verbose)
}


#' @export
# TODO: Check these
get_data.meta_fixed <- get_data.meta_random


#' @export
# TODO: Check these
get_data.ivFixed <- get_data.rma


#' @export
get_data.bfsl <- function(x, ...) {
  as.data.frame(x$data[c("x", "y", "sd_x", "sd_y")])
}


#' @export
get_data.mipo <- function(x, ...) {
  tryCatch(
    {
      models <- eval(x$call$object)
      get_data(models$analyses[[1]], ...)
    },
    error = function(e) {
      NULL
    }
  )
}


#' @export
get_data.htest <- function(x, ...) {
  out <- NULL
  if (!is.null(x$data.name)) {
    out <- .retrieve_htest_data(x)
  }
  out
}
