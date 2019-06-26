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
#'
#' @importFrom stats model.frame na.omit
#' @export
get_data <- function(x, ...) {
  UseMethod("get_data")
}


#' @export
get_data.default <- function(x, ...) {
  if (inherits(x, "list") && .obj_has_name(x, "gam")) {
    x <- x$gam
    class(x) <- c(class(x), c("glm", "lm"))
  }

  mf <- tryCatch({
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

  .prepare_get_data(x, mf)
}


#' @export
get_data.biglm <- function(x, ...) {
  mf <- stats::model.frame(x)
  .prepare_get_data(x, mf)
}


#' @export
get_data.bigglm <- function(x, ...) {
  mf <- stats::model.frame(x)
  .prepare_get_data(x, mf)
}


#' @export
get_data.felm <- function(x, effects = c("all", "fixed", "random"), ...) {
  effects <- match.arg(effects)
  .get_data_from_modelframe(x, stats::model.frame(x), effects)
}


#' @export
get_data.feis <- function(x, effects = c("all", "fixed", "random"), ...) {
  effects <- match.arg(effects)
  mf <- tryCatch({
    get(.safe_deparse(x$call$data), envir = parent.frame())[, find_variables(x, flatten = TRUE), drop = FALSE]
  },
  error = function(x) {
    stats::model.frame(x)
  })

  .get_data_from_modelframe(x, mf, effects)
}


#' @export
get_data.LORgee <- function(x, effects = c("all", "fixed", "random"), ...) {
  effects <- match.arg(effects)
  mf <- tryCatch({
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
  })

  .prepare_get_data(x, stats::na.omit(mf))
}


#' @export
get_data.survfit <- function(x, ...) {
  mf <- tryCatch({
    .get_data_from_env(x)[, find_variables(x, flatten = TRUE), drop = FALSE]
  },
  error = function(x) {
    NULL
  })

  .prepare_get_data(x, stats::na.omit(mf))
}


#' @export
get_data.BBmm <- function(x, effects = c("all", "fixed", "random"), ...) {
  effects <- match.arg(effects)
  mf <- tryCatch({
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
  })

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


#' @export
get_data.gbm <- function(x, ...) {
  mf <- tryCatch({
    get(.safe_deparse(x$call$data), envir = parent.frame())[, find_variables(x, flatten = TRUE), drop = FALSE]
  },
  error = function(x) {
    stats::model.frame(x)
  })

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
get_data.data.frame <- function(x, ...) {
  x
}


#' @export
get_data.plm <- function(x, ...) {
  mf <- stats::model.frame(x)
  cn <- colnames(mf)
  mf <- as.data.frame(lapply(mf, as.vector))
  colnames(mf) <- clean_names(cn)
  model_terms <- find_variables(x, effects = "all", component = "all", flatten = TRUE)
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
get_data.gamm <- function(x, ...) {
  x <- x$gam
  class(x) <- c(class(x), c("glm", "lm"))
  mf <- stats::model.frame(x)
  .prepare_get_data(x, mf)
}


#' @export
get_data.ivreg <- function(x, ...) {
  mf <- stats::model.frame(x)
  cn <- clean_names(colnames(mf))
  ft <- find_variables(x, flatten = TRUE)

  remain <- setdiff(ft, cn)

  if (.is_empty_object(remain)) {
    final_mf <- mf
  } else {
    final_mf <- tryCatch({
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
get_data.iv_robust <- function(x, ...) {
  mf <- stats::model.frame(x)
  cn <- clean_names(colnames(mf))
  ft <- find_variables(x, flatten = TRUE)

  remain <- setdiff(ft, cn)

  if (.is_empty_object(remain)) {
    final_mf <- mf
  } else {
    final_mf <- tryCatch({
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
get_data.clm2 <- function(x, ...) {
  mf <- tryCatch({
    x$location
  },
  error = function(x) {
    NULL
  }
  )

  .prepare_get_data(x, mf)
}


#' @rdname get_data
#' @export
get_data.hurdle <- function(x, component = c("all", "conditional", "zi", "zero_inflated", "dispersion"), ...) {
  component <- match.arg(component)
  .reurn_zeroinf_data(x, component)
}


#' @export
get_data.zeroinfl <- function(x, component = c("all", "conditional", "zi", "zero_inflated", "dispersion"), ...) {
  component <- match.arg(component)
  .reurn_zeroinf_data(x, component)
}


#' @export
get_data.zerotrunc <- function(x, component = c("all", "conditional", "zi", "zero_inflated", "dispersion"), ...) {
  component <- match.arg(component)
  .reurn_zeroinf_data(x, component)
}


#' @rdname get_data
#' @export
get_data.glmmTMB <- function(x, effects = c("all", "fixed", "random"), component = c("all", "conditional", "zi", "zero_inflated", "dispersion"), ...) {
  effects <- match.arg(effects)
  component <- match.arg(component)

  model.terms <- find_variables(x, effects = "all", component = "all", flatten = FALSE)

  mf <- tryCatch({
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

  mf <- tryCatch({
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
get_data.rlmerMod <- function(x, effects = c("all", "fixed", "random"), ...) {
  effects <- match.arg(effects)
  .get_data_from_modelframe(x, stats::model.frame(x), effects)
}


#' @rdname get_data
#' @export
get_data.mixed <- function(x, effects = c("all", "fixed", "random"), ...) {
  effects <- match.arg(effects)
  .get_data_from_modelframe(x, x$data, effects)
}


#' @rdname get_data
#' @export
get_data.clmm <- function(x, effects = c("all", "fixed", "random"), ...) {
  effects <- match.arg(effects)
  .get_data_from_modelframe(x, stats::model.frame(x), effects)
}


#' @rdname get_data
#' @export
get_data.lme <- function(x, effects = c("all", "fixed", "random"), ...) {
  effects <- match.arg(effects)
  dat <- tryCatch({
      x$data
    },
    error = function(x) {
      NULL
    }
  )

  .get_data_from_modelframe(x, dat, effects)
}


#' @export
get_data.vgam <- function(x, ...) {
  mf <- tryCatch({
    get(x@misc$dataname, envir = parent.frame())[, find_variables(x, flatten = TRUE), drop = FALSE]
  },
  error = function(x) {
    NULL
  }
  )

  .prepare_get_data(x, mf)
}


#' @rdname get_data
#' @export
get_data.gee <- function(x, effects = c("all", "fixed", "random"), ...) {
  effects <- match.arg(effects)
  mf <- tryCatch({
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
get_data.rqss <- function(x, effects = c("all", "fixed", "random"), ...) {
  mf <- tryCatch({
    .get_data_from_env(x)[, find_variables(x, flatten = TRUE), drop = FALSE]
  },
  error = function(x) {
    NULL
  }
  )

  .prepare_get_data(x, stats::na.omit(mf))
}


#' @export
get_data.gls <- function(x, ...) {
  mf <- tryCatch({
    .get_data_from_env(x)[, find_variables(x, flatten = TRUE), drop = FALSE]
  },
  error = function(x) {
    NULL
  }
  )

  .prepare_get_data(x, stats::na.omit(mf))
}


#' @export
get_data.gmnl <- function(x, ...) {
  mf <- tryCatch({
    x$mf
  },
  error = function(x) {
    NULL
  }
  )

  .prepare_get_data(x, mf)
}


#' @rdname get_data
#' @export
get_data.MixMod <- function(x, effects = c("all", "fixed", "random"), component = c("all", "conditional", "zi", "zero_inflated", "dispersion"), ...) {
  effects <- match.arg(effects)
  component <- match.arg(component)

  tryCatch({
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

    fitfram$grp__id <- x$id
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


#' @rdname get_data
#' @export
get_data.brmsfit <- function(x, effects = c("all", "fixed", "random"), component = c("all", "conditional", "zi", "zero_inflated"), ...) {
  effects <- match.arg(effects)
  component <- match.arg(component)

  model.terms <- find_variables(x, effects = "all", component = "all", flatten = FALSE)
  mf <- stats::model.frame(x)

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
get_data.vglm <- function(x, ...) {
  mf <- tryCatch({
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
get_data.stanmvreg <- function(x, ...) {
  mf <- tryCatch({
    out <- data.frame()
    for (i in stats::model.frame(x))
      out <- .merge_dataframes(out, i)

    out
  },
  error = function(x) {
    NULL
  }
  )

  .prepare_get_data(x, mf)
}


#' @export
get_data.BFBayesFactor <- function(x, ...) {
  x@data
}


#' @rdname get_data
#' @export
get_data.MCMCglmm <- function(x, effects = c("all", "fixed", "random"), ...) {
  effects <- match.arg(effects)
  mf <- tryCatch({
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


#' @importFrom stats getCall formula na.omit
.prepare_get_data <- function(x, mf, effects = "fixed") {
  if (.is_empty_object(mf)) {
    warning("Could not get model data.", call. = F)
    return(NULL)
  }

  # we may store model weights here later
  mw <- NULL

  # do we have an offset, not specified in the formula?
  if ("(offset)" %in% colnames(mf) && .obj_has_name(x, "call") && .obj_has_name(x$call, "offset")) {
    offcol <- which(colnames(mf) == "(offset)")
    colnames(mf)[offcol] <- clean_names(.safe_deparse(x$call$offset))
  }

  # clean 1-dimensional matrices
  mf[] <- lapply(mf, function(.x) {
    if (is.matrix(.x) && dim(.x)[2] == 1 && !inherits(.x, c("ns", "bs"))) {
      as.vector(.x)
    } else {
      .x
    }
  })

  # check if we have any matrix columns, e.g. from splines
  mc <- sapply(mf, is.matrix)

  # don't change response value, if it's a matrix
  # bound with cbind()
  rn <- find_response(x, combine = TRUE)
  rn_not_combined <- find_response(x, combine = FALSE)

  trials.data <- NULL

  if (mc[1] && rn == colnames(mf)[1]) {
    mc[1] <- FALSE
    if (inherits(x, c("coxph", "coxme", "survreg", "survfit", "crq", "psm"))) {
      mf <- cbind(mf[[1]][, 1], mf[[1]][, 2], mf)
      colnames(mf)[1:2] <- rn_not_combined
    } else {
      tryCatch({
        trials.data <- as.data.frame(mf[[1]])
        colnames(trials.data) <- rn_not_combined

        # if columns were bound via substraction, e.g.
        # "cbind(succes, total - success)", we need to sum up success and
        # total for the original total-column.

        pattern <- sprintf("%s(\\s*)-(\\s*)%s", rn_not_combined[2], rn_not_combined[1])
        if (grepl(pattern = pattern, x = rn)) {
          trials.data[[2]] <- trials.data[[1]] + trials.data[[2]]
        }
      },
      error = function(x) {
        NULL
      }
      )
    }
  }

  # if we have any matrix columns, we remove them from original
  # model frame and convert them to regular data frames, give
  # proper column names and bind them back to the original model frame
  if (any(mc)) {
    # try to get model data from environment
    md <- tryCatch({
      eval(stats::getCall(x)$data, environment(stats::formula(x)))
    },
    error = function(x) {
      NULL
    }
    )

    # if data not found in environment, reduce matrix variables into regular vectors
    if (is.null(md)) {
      # we select the non-matrix variables and convert matrix-variables into
      # regular data frames, then binding them together
      mf_matrix <- mf[, which(mc), drop = FALSE]
      mf_nonmatrix <- mf[, -which(mc), drop = FALSE]
      # fix for rms::rcs() functions
      if (class(mf_matrix[[1]]) == "rms") class(mf_matrix[[1]]) <- "matrix"
      mf_list <- lapply(mf_matrix, as.data.frame, stringsAsFactors = FALSE)
      mf_matrix <- do.call(cbind, mf_list)
      mf <- cbind(mf_nonmatrix, mf_matrix)
    } else {

      # fix NA in column names
      if (any(is.na(colnames(md)))) colnames(md) <- make.names(colnames(md))

      # get "matrix" terms and "normal" predictors, but exclude
      # response variable(s)
      mf_matrix <- mf[, -which(mc), drop = FALSE]
      spline.term <- clean_names(names(which(mc)))
      other.terms <- clean_names(colnames(mf_matrix))[-1]

      # now we have all variable names that we need from the original
      # data set
      needed.vars <- c(other.terms, spline.term)

      # if response is a matrix vector (e.g. multivariate response),
      # we need to include all response names as well, because else
      # rows may not match due to additional missings in the response variables

      if (is.matrix(mf[[1]])) {
        needed.vars <- c(dimnames(mf[[1]])[[2]], needed.vars)
      } else {
        needed.vars <- c(colnames(mf)[1], needed.vars)
      }

      # check model weights

      if ("(weights)" %in% needed.vars && !.obj_has_name(md, "(weights)")) {
        needed.vars <- needed.vars[-which(needed.vars == "(weights)")]
        mw <- mf[["(weights)"]]
      }

      if (inherits(x, c("coxph", "coxme"))) {
        mf <- md
      } else {
        needed.vars <- unique(clean_names(needed.vars))
        mf <- stats::na.omit(md[, needed.vars, drop = FALSE])
      }

      # add back model weights, if any
      if (!is.null(mw)) mf$`(weights)` <- mw
    }

    # check if we really have all formula terms in our model frame now
    pv <- tryCatch({
      find_predictors(x, effects = effects, flatten = TRUE)
    },
    error = function(x) {
      NULL
    }
    )

    if (!is.null(pv) && !all(pv %in% colnames(mf))) {
      warning("Some model terms could not be found in model data. You probably need to load the data into the environment.", call. = FALSE)
    }
  }

  # check if we have monotonic variables, included in formula
  # with "mo()"? If yes, remove from model frame
  mos_eisly <- grepl(pattern = "^mo\\(([^,)]*).*", x = colnames(mf))
  if (any(mos_eisly)) mf <- mf[!mos_eisly]

  # clean variable names
  cvn <- .remove_pattern_from_names(colnames(mf), ignore_lag = TRUE)

  # keep "as is" variable for response variables in data frame
  if (colnames(mf)[1] == rn[1] && grepl("^I\\(", rn[1])) {
    md <- tryCatch({
      tmp <- .get_data_from_env(x)[, unique(c(rn_not_combined, cvn)), drop = FALSE]
      tmp[, rn_not_combined, drop = FALSE]
    },
    error = function(x) {
      NULL
    }
    )

    if (!is.null(md)) {
      mf <- cbind(mf, md)
      cvn <- .remove_pattern_from_names(colnames(mf), ignore_lag = TRUE)
      cvn[1] <- rn[1]
    }
  }

  # do we have duplicated names?
  dupes <- which(duplicated(cvn))
  if (!.is_empty_string(dupes)) cvn[dupes] <- sprintf("%s.%s", cvn[dupes], 1:length(dupes))

  colnames(mf) <- cvn

  # add weighting variable
  weighting_var <- find_weights(x)
  if (!is.null(weighting_var) && !weighting_var %in% colnames(mf) && length(weighting_var) == 1) {
    mf <- tryCatch(
      {
        tmp <- cbind(mf, get_weights(x))
        colnames(tmp)[ncol(tmp)] <- weighting_var
        tmp
      },
      error = function(e) { mf }
    )
  }

  # add back possible trials-data
  if (!is.null(trials.data)) {
    new.cols <- setdiff(colnames(trials.data), colnames(mf))
    if (!.is_empty_string(new.cols)) mf <- cbind(mf, trials.data[, new.cols, drop = FALSE])
  }

  mf
}


.return_data <- function(x, mf, effects, component, model.terms, is_mv = FALSE) {
  response <- unlist(model.terms$response)

  if (is_mv) {
    fixed.component.data <- switch(
      component,
      all = c(
        sapply(model.terms[-1], function(i) i$conditional),
        sapply(model.terms[-1], function(i) i$zero_inflated),
        sapply(model.terms[-1], function(i) i$dispersion)
      ),
      conditional = sapply(model.terms[-1], function(i) i$conditional),
      zi = ,
      zero_inflated = sapply(model.terms[-1], function(i) i$zero_inflated),
      dispersion = sapply(model.terms[-1], function(i) i$dispersion)
    )

    random.component.data <- switch(
      component,
      all = c(
        sapply(model.terms[-1], function(i) i$random),
        sapply(model.terms[-1], function(i) i$zero_inflated_random)
      ),
      conditional = sapply(model.terms[-1], function(i) i$random),
      zi = ,
      zero_inflated = sapply(model.terms[-1], function(i) i$zero_inflated_random)
    )

    fixed.component.data <- unlist(fixed.component.data)
    random.component.data <- unlist(random.component.data)

  } else {
    fixed.component.data <- switch(
      component,
      all           = c(model.terms$conditional, model.terms$zero_inflated, model.terms$dispersion),
      conditional   = model.terms$conditional,
      zi = ,
      zero_inflated = model.terms$zero_inflated,
      dispersion    = model.terms$dispersion
    )

    random.component.data <- switch(
      component,
      all           = c(model.terms$random, model.terms$zero_inflated_random),
      conditional   = model.terms$random,
      zi = ,
      zero_inflated = model.terms$zero_inflated_random
    )
  }


  # this is to remove the "1" from intercept-ony-models

  if (!.is_empty_object(fixed.component.data)) {
    fixed.component.data <- .remove_values(fixed.component.data, c("1", "0"))
    fixed.component.data <- .remove_values(fixed.component.data, c(1, 0))
  }
  if (!.is_empty_object(random.component.data)) {
    random.component.data <- .remove_values(random.component.data, c("1", "0"))
    random.component.data <- .remove_values(random.component.data, c(1, 0))
  }


  dat <- switch(
    effects,
    all = mf[, unique(c(response, fixed.component.data, random.component.data, find_weights(x))), drop = FALSE],
    fixed = mf[, unique(c(response, fixed.component.data, find_weights(x))), drop = FALSE],
    random = mf[, unique(random.component.data), drop = FALSE]
  )

  if (.is_empty_object(dat)) {
    print_color(sprintf("Warning: Data frame is empty, probably component '%s' does not exist in the %s-part of the model?\n", component, effects), "red")
    return(NULL)
  }

  if ("(offset)" %in% colnames(mf) && !("(offset)" %in% colnames(dat))) {
    dat <- cbind(dat, mf[["(offset"]])
  }


  dat
}


.add_zeroinf_data <- function(x, mf, tn) {
  tryCatch({
    env_data <- eval(x$call$data, envir = parent.frame())[, tn, drop = FALSE]
    if (.obj_has_name(x$call, "subset")) {
      env_data <- subset(env_data, subset = eval(x$call$subset))
    }

    .merge_dataframes(env_data, mf, replace = TRUE)
  },
  error = function(x) {
    mf
  }
  )
}


.get_zelig_relogit_frame <- function(x) {
  vars <- find_variables(x, flatten = TRUE)
  x$data[, vars, drop = FALSE]
}


.reurn_zeroinf_data <- function(x, component) {
  model.terms <- find_variables(x, effects = "all", component = "all", flatten = FALSE)

  mf <- tryCatch({
    stats::model.frame(x)
  },
  error = function(x) {
    NULL
  }
  )

  mf <- .prepare_get_data(x, mf)
  # add variables from other model components
  mf <- .add_zeroinf_data(x, mf, model.terms$zero_inflated)

  fixed.data <- switch(
    component,
    all = c(model.terms$conditional, model.terms$zero_inflated),
    conditional = model.terms$conditional,
    zi = ,
    zero_inflated = model.terms$zero_inflated
  )

  mf[, unique(c(model.terms$response, fixed.data, find_weights(x))), drop = FALSE]
}




.get_data_from_modelframe <- function(x, dat, effects) {
  cn <- clean_names(colnames(dat))

  ft <- switch(
    effects,
    fixed = find_variables(x, effects = "fixed", flatten = TRUE),
    all = find_variables(x, flatten = TRUE),
    random = find_random(x, split_nested = TRUE, flatten = TRUE)
  )

  remain <- intersect(c(ft, find_weights(x)), cn)

  mf <- tryCatch({
    dat[, remain, drop = FALSE]
  },
  error = function(x) {
    dat
  }
  )

  .prepare_get_data(x, mf, effects)
}
