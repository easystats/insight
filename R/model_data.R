#' @title Get name of a model's response variable
#' @name model_data
#'
#' @description Returns the name(s) of the response variable(s) from a model object.
#'
#' @param effects Should model data for fixed effects, random effects
#'    or both be returned? Only applies to mixed models.
#' @inheritParams model_predictors
#'
#' @return ## TODO docs
#'
#' @examples
#' data(cbpp, package = "lme4")
#' cbpp$trials <- cbpp$size - cbpp$incidence
#' m <- glm(cbind(incidence, trials) ~ period, data = cbpp, family = binomial)
#' head(model_data(m))
#'
#' @export
model_data <- function(x, ...) {
  UseMethod("model_data")
}


#' @importFrom stats model.frame
#' @export
model_data.default <- function(x, ...) {
  mf <- tryCatch(
    {
      if (inherits(x, "Zelig-relogit"))
        get_zelig_relogit_frame(x)
      else
        stats::model.frame(x)
    },
    error = function(x) { NULL }
  )

  prepare_model_data(x, mf)
}


#' @export
model_data.clm2 <- function(x, ...) {
  mf <- tryCatch(
    {x$location},
    error = function(x) { NULL }
  )

  prepare_model_data(x, mf)
}


#' @importFrom stats model.frame
#' @export
model_data.glmmTMB <- function(x, ...) {
  mf <- tryCatch(
    {stats::model.frame(x)},
    error = function(x) { NULL }
  )

  mf <- prepare_model_data(x, mf)

  disp <- tryCatch(
    {all.vars(x$modelInfo$allForm$dispformula[[2L]])},
    error = function(x) { NULL }
  )

  if (!is.null(disp)) {
    mf <- tryCatch(
      {
        env_data <- eval(x$call$data, envir = parent.frame())[, disp, drop = FALSE]
        merge_dataframes(env_data, mf, replace = TRUE)
      },
      error = function(x) { mf }
    )
  }

  zi <- tryCatch(
    {all.vars(x$modelInfo$allForm$ziformula[[2L]])},
    error = function(x) { NULL }
  )

  if (!is.null(zi)) {
    mf <- tryCatch(
      {
        env_data <- eval(x$call$data, envir = parent.frame())[, zi, drop = FALSE]
        merge_dataframes(env_data, mf, replace = TRUE)
      },
      error = function(x) { mf }
    )
  }

  mf
}


#' @rdname model_data
#' @importFrom stats model.frame
#' @export
model_data.merMod <- function(x, effects = c("fixed", "random", "all"), ...) {
  effects <- match.arg(effects)

  mf <- tryCatch(
    {
      switch(
        effects,
        fixed = stats::model.frame(x, fixed.only = TRUE),
        all = stats::model.frame(x, fixed.only = FALSE),
        random = stats::model.frame(x, fixed.only = FALSE)[, re_terms(x), drop = FALSE]
      )
    },
    error = function(x) { NULL }
  )

  prepare_model_data(x, mf, effects)
}


#' @export
model_data.lme <- function(x, ...) {
  mf <- tryCatch(
    {x$data},
    error = function(x) { NULL }
  )

  prepare_model_data(x, mf)
}


#' @export
model_data.vgam <- function(x, ...) {
  mf <- tryCatch(
    {get(x@misc$dataname, envir = parent.frame())},
    error = function(x) { NULL}
  )

  prepare_model_data(x, mf)
}


#' @export
model_data.gee <- function(x, ...) {
  mf <- tryCatch(
    {eval(x$call$data, envir = parent.frame())},
    error = function(x) { NULL }
  )

  prepare_model_data(x, mf)
}


#' @export
model_data.gls <- function(x, ...) {
  mf <- tryCatch(
    {eval(x$call$data, envir = parent.frame())},
    error = function(x) { NULL }
  )

  prepare_model_data(x, mf)
}


#' @export
model_data.gmnl <- function(x, ...) {
  mf <- tryCatch(
    {x$mf},
    error = function(x) { NULL }
  )

  prepare_model_data(x, mf)
}


#' @export
model_data.MixMod <- function(x, ...) {
  mf <- tryCatch(
    {
      fitfram <- x$model_frames$mfX
      if (!is_empty_object(x$model_frames$mfZ))
        fitfram <- merge_dataframes(x$model_frames$mfZ, fitfram, replace = TRUE)
      if (!is_empty_object(x$model_frames$mfX_zi))
        fitfram <- merge_dataframes(x$model_frames$mfX_zi, fitfram, replace = TRUE)
      if (!is_empty_object(x$model_frames$mfZ_zi))
        fitfram <- merge_dataframes(x$model_frames$mfZ_zi, fitfram, replace = TRUE)

      fitfram$grp__id <- x$id
      colnames(fitfram)[ncol(fitfram)] <- x$id_name[1]
      fitfram
    },
    error = function(x) { NULL }
  )

  prepare_model_data(x, mf)
}


#' @export
model_data.vglm <- function(x, ...) {
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
    error = function(x) { NULL }
  )

  prepare_model_data(x, mf)
}


#' @importFrom stats model.frame
#' @export
model_data.stanmvreg <- function(x, ...) {
  mf <- tryCatch(
    {
      out <- data.frame()

      for (i in stats::model.frame(x)) {
        out <- merge_dataframes(out, i)
      }

      out
    },
    error = function(x) { NULL }
  )

  prepare_model_data(x, mf)
}


#' @export
model_data.MCMCglmm <- function(x, ...) {
  mf <- tryCatch(
    {
      env_dataframes <- names(which(unlist(eapply(.GlobalEnv, is.data.frame))))
      pv <- model_predictors(x, effects = "all")
      matchframe <- unlist(lapply(env_dataframes, function(.x) {
        dat <- get(.x)
        all(pv %in% colnames(dat))
      }))
      mf <- env_dataframes[matchframe][1]
      if (!is.na(mf))
        get(mf)
      else
        NULL
    },
    error = function(x) { NULL }
  )

  prepare_model_data(x, mf, effects = "all")
}


#' @importFrom dplyr select
get_zelig_relogit_frame <- function(x) {
  vars <- c(model_response(x), model_predictors(x))
  dplyr::select(x$data, !! vars)
}



#' @importFrom stats getCall formula na.omit
prepare_model_data <- function(x, mf, effects = "fixed") {
  if (is.null(mf)) {
    warning("Could not get model data.", call. = F)
    return(NULL)
  }

  # do we have an offset, not specified in the formula?
  if ("(offset)" %in% colnames(mf) && obj_has_name(x, "call") && obj_has_name(x$call, "offset")) {
    offcol <- which(colnames(mf) == "(offset)")
    colnames(mf)[offcol] <- var_names(deparse(x$call$offset, width.cutoff = 500L))
  }

  # clean 1-dimensional matrices
  mf[] <- lapply(mf, function(.x) {
    if (is.matrix(.x) && dim(.x)[2] == 1 && !inherits(.x, c("ns", "bs")))
      as.vector(.x)
    else
      .x
  })

  # check if we have any matrix columns, e.g. from splines
  mc <- unlist(lapply(mf, is.matrix))

  # don't change response value, if it's a matrix
  # bound with cbind()
  rn <- model_response(x, combine = TRUE)
  trials.data <- NULL

  if (mc[1] && rn == colnames(mf)[1]) {
    mc[1] <- FALSE
    tryCatch(
      {
        trials.data <- as.data.frame(mf[[1]])
        colnames(trials.data) <- model_response(x, combine = FALSE)
      },
      error = function(x) { NULL }
    )
  }

  # if we have any matrix columns, we remove them from original
  # model frame and convert them to regular data frames, give
  # proper column names and bind them back to the original model frame
  if (any(mc)) {
    # try to get model data from environment
    md <- tryCatch(
      {
        eval(stats::getCall(x)$data, environment(stats::formula(x)))
      },
      error = function(x) { NULL }
    )

    # if data not found in environment, reduce matrix variables into regular vectors
    if (is.null(md)) {
      # first, we select the non-matrix variables. calling "as_tibble" would
      # remove their column name, so we us as_tibble to convert matrix
      # to vectors only for the matrix-columns
      mf_matrix <- mf[, which(mc), drop = FALSE]
      mf_nonmatrix <- mf[, -which(mc), drop = FALSE]
      mf_matrix <- cbind(lapply(mf_matrix, as.data.frame, stringsAsFactors = FALSE))
      mf <- cbind(mf_nonmatrix, mf_matrix)
    } else {

      # fix NA in column names
      if (any(is.na(colnames(md)))) colnames(md) <- make.names(colnames(md))

      # get "matrix" terms and "normal" predictors, but exclude
      # response variable(s)
      mf_matrix <- mf[, -which(mc), drop = FALSE]
      spline.term <- var_names(names(which(mc)))
      other.terms <- var_names(colnames(mf_matrix))[-1]

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

      if ("(weights)" %in% needed.vars && !obj_has_name(md, "(weights)")) {
        needed.vars <- needed.vars[-which(needed.vars == "(weights)")]
        mw <- mf[["(weights)"]]
      }


      if (inherits(x, "coxph")) {
        mf <- md
      } else {
        mf <- stats::na.omit(md[, needed.vars, drop = FALSE])
      }

      # add back model weights, if any
      if (!is.null(mw)) mf$`(weights)` <- mw
    }

    # check if we really have all formula terms in our model frame now
    pv <- tryCatch(
      {model_predictors(x, effects = effects)},
      error = function(x) { NULL }
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
  cvn <- var_names(colnames(mf))

  # do we have duplicated names?
  dupes <- which(duplicated(cvn))
  if (!is_empty_string(dupes)) cvn[dupes] <- sprintf("%s.%s", cvn[dupes], 1:length(dupes))

  colnames(mf) <- cvn

  # add back possible trials-data
  if (!is.null(trials.data)) {
    new.cols <- setdiff(colnames(trials.data), colnames(mf))
    if (!is_empty_string(new.cols)) mf <- cbind(mf, trials.data[, new.cols, drop = FALSE])
  }

  mf
}