#' @title Get the data that was used to fit the model
#' @name get_data
#'
#' @description This functions tries to get the data that was used to fit the
#'   model and returns it as data frame.
#'
#' @param effects Should model data for fixed effects (`"fixed"`), random
#'   effects (`"random"`) or both (`"all"`) be returned? Only applies to mixed
#'   or gee models.
#' @param source String, indicating from where data should be recovered. If
#'   `source = "environment"` (default), data is recovered from the environment
#'   (e.g. if the data is in the workspace). This option is usually the fastest
#'   way of getting data and ensures that the original variables used for model
#'   fitting are returned. Note that always the _current_ data is recovered from
#'   the environment. Hence, if the data was modified _after_ model fitting
#'   (e.g., variables were recoded or rows filtered), the returned data may no
#'   longer equal the model data. If `source = "frame"` (or `"mf"`), the data
#'   is taken from the model frame. Any transformed variables are back-transformed,
#'   if possible. This option returns the data even if it is not available in
#'   the environment, however, in certain edge cases back-transforming to the
#'   original data may fail. If `source = "environment"` fails to recover the
#'   data, it tries to extract the data from the model frame; if
#'   `source = "frame"` and data cannot be extracted from the model frame, data
#'   will be recovered from the environment. Both ways only returns observations
#'   that have no missing data in the variables used for model fitting.
#' @param verbose Toggle messages and warnings.
#'
#' @inheritParams find_predictors
#' @inheritParams find_formula
#'
#' @inheritSection find_predictors Model components
#'
#' @return The data that was used to fit the model.
#'
#' @examplesIf require("lme4", quietly = TRUE)
#' data(cbpp, package = "lme4")
#' cbpp$trials <- cbpp$size - cbpp$incidence
#' m <- glm(cbind(incidence, trials) ~ period, data = cbpp, family = binomial)
#' head(get_data(m))
#' @export
get_data <- function(x, ...) {
  UseMethod("get_data")
}


# extract data from environment -------------------------------

# main workhorse, we try to recover data from environment as good as possible.
# the dataset is subset if needed, and weights are added. only those columns
# are returned that we actually find in the model...
# data_name is useful when we have the name of the data frame object stored as
# a string (e.g., in brmsfit attr(x$data, "data_frame"))
.get_data_from_environment <- function(x,
                                       effects = "all",
                                       component = "all",
                                       source = "environment",
                                       additional_variables = NULL,
                                       verbose = FALSE,
                                       data_name = NULL) {
  # process arguments, check whether data should be recovered from
  # environment or model frame
  source <- .check_data_source_arg(source)
  # if not environment, leave
  if (source != "environment") {
    return(NULL)
  }

  # handle arguments
  effects <- validate_argument(effects, c("all", "fixed", "random"))
  component <- validate_argument(
    component,
    c("all", "conditional", "zero_inflated", "zi", "smooth_terms", "dispersion")
  )

  # we want to add the variable for subsettig, too
  model_call <- get_call(x)

  # for random effects, we still need all variables to be extracted
  # in case we have missing data. E.g., if random effects variables have
  # no missing data, but response or other fixed effects has, "get_random()"
  # should only return non-missing data for the model - thus, missing cases
  # in any fixed effects variable should be removed, even if non-missing in
  # random effects variables (see #777)
  if (effects == "random") {
    selected_vars <- "all"
  } else {
    selected_vars <- effects
  }

  # extract model variables, if possible
  vars <- try(
    find_variables(x, effects = selected_vars, component = component, flatten = TRUE, verbose = FALSE),
    silent = TRUE
  )

  # if "find_variables()" fails, we set it to NULL
  if (inherits(vars, "try-error")) {
    vars <- NULL

    # if "find_variables()" returns NULL, we assume this is intentional, as
    # specific model components were requested, which are not available
  } else if (is.null(vars) && effects != "fixed") {
    # for fixed effects, always include response,
    # so return NULL only if effects != "fixed"
    if (verbose) {
      format_warning(
        "Could not find any variables for the specified model component.",
        "You may try other values for the `effects` and `component` argument to retrieve model data."
      )
    }
    return(NULL)
  }

  out <- tryCatch(
    {
      # recover data frame from environment
      dat <- .recover_data_from_environment(x, data_name = data_name, verbose = verbose)
      # for metafor, we need to add weights...
      if (inherits(x, c("rma.uni", "rma"))) {
        ## TODO: check if we need to do this for other meta-analysis packages, too
        wdat <- data.frame(Weights = get_weights(x))
        additional_variables <- c(additional_variables, "Weights")
        dat <- tryCatch(cbind(dat, wdat), error = function(e) dat)
      }
      # additional variables? Some models, like plm::plm(), have an "index"
      # slot in the model call with further variables
      if (!is.null(additional_variables) && !isTRUE(additional_variables)) {
        vars <- c(vars, additional_variables)
      }
      # add response, only required if "find_variables()" does not already
      # return it (which is the case when component is "all" or "conditional")
      if (!component %in% c("all", "conditional")) {
        vars <- c(vars, find_response(x, combine = FALSE))
      }

      ## TODO: do we want random slopes included? Previuosly, we did not.
      # add random slopes, if any
      # if (effects %in% c("all", "random")) {
      #   vars <- c(vars, unlist(find_random_slopes(x)))
      # }

      # select only those variables from the data that we find in the model
      if (!is.null(vars)) {
        # weighting variable?
        vars <- c(vars, find_weights(x))
        # offset?
        vars <- c(vars, find_offset(x))
        # subset?
        if (!is.null(model_call$subset)) {
          subset_vars <- .safe(all.vars(model_call$subset))
          vars <- c(vars, subset_vars)
        }
        vars <- unique(vars)
        # if "additional_variables" is TRUE, keep *all* variables from original
        # data, else make sure only required columns are returned
        if (!isTRUE(additional_variables)) {
          dat <- dat[, intersect(vars, colnames(dat)), drop = FALSE]
        }
      }

      # complete cases only, as in model frames, need to filter attributes
      # only use model variables in complete.cases()
      if (is.null(vars)) {
        cc <- stats::complete.cases(dat)
      } else {
        cc <- stats::complete.cases(dat[, intersect(vars, colnames(dat))])
      }

      # only preserve random effects
      if (effects == "random") {
        dat <- dat[find_random(x, split_nested = TRUE, flatten = TRUE)]
      }

      if (!all(cc)) {
        # save original data, for attributes
        original_dat <- dat
        # filter
        dat <- dat[cc, , drop = FALSE]
        # add back labels
        var_label <- compact_list(lapply(original_dat, attr, "label", exact = TRUE))
        if (length(var_label)) {
          for (i in names(var_label)) {
            attr(dat[[i]], "label") <- var_label[[i]]
          }
        }
        val_labels <- compact_list(lapply(original_dat, attr, "labels", exact = TRUE))
        if (length(val_labels)) {
          for (i in names(val_labels)) {
            attr(dat[[i]], "labels") <- val_labels[[i]]
          }
        }
      }
      # any data left?
      if (nrow(dat) == 0 || ncol(dat) == 0) {
        dat <- NULL
      }
      dat
    },
    error = function(x) {
      NULL
    }
  )
  # successful?
  if (is.null(out) && verbose) {
    format_warning(
      "Could not recover model data from environment. Please make sure your data is available in your workspace.",
      "Trying to retrieve data from the model frame now."
    )
  }
  out
}


# find data from the environment -----------------------------------

# return data from a data frame that is in the environment,
# and subset the data, if necessary
.recover_data_from_environment <- function(x, data_name = NULL, verbose = FALSE) {
  model_call <- get_call(x)

  if (is.null(model_call[["data"]]) && is.character(data_name)) {
    model_call[["data"]] <- as.name(data_name)
  }

  # special handling for fixest, see #767
  if (inherits(x, "fixest")) {
    # when called from inside function, fixest seems to have a different
    # environment that requires recovering from parent-environment
    dat <- .safe(eval(model_call$data, envir = parent.env(x$call_env)))
  } else {
    # first, try environment of formula, see #666. set enclos = NULL so eval()
    # does not fall back to parent frame when the environment is NULL, since we
    # want to try that after checking the formula
    dat <- .safe(eval(model_call$data,
      envir = environment(model_call$formula),
      enclos = NULL
    ))
  }

  # second, try to extract formula directly
  if (is.null(dat)) {
    dat <- .safe(eval(model_call$data,
      envir = environment(find_formula(x, verbose = FALSE)$conditional),
      enclos = NULL
    ))
  }

  # validation check- if data frame is named like a function, e.g.
  # rep <- data.frame(...), we now have a function instead of the data
  # we then need to reset "dat" to NULL and search in the global env

  if (!is.null(dat) && !is.data.frame(dat)) {
    dat <- .safe(as.data.frame(dat))
  }

  # third try, global env
  if (is.null(dat)) {
    dat <- .safe(eval(model_call$data, envir = globalenv()))
  }

  # last try, internal env
  if (is.null(dat)) {
    dat <- .safe(eval(model_call$data, envir = parent.env(x$call_env)))
  }

  if (!is.null(dat) && object_has_names(model_call, "subset")) {
    subset_data <- .safe(subset(dat, subset = eval(model_call$subset)))
    if (!is.null(subset_data)) {
      dat <- subset_data
    } else if (verbose) {
      format_warning("Looks like the original data was subset, however `get_data()` could not retrieve the subset of the data. The full data set is returned.") # nolint
    }
  }

  dat
}


# default method ------------------------------------------------------

#' @rdname get_data
#' @export
get_data.default <- function(x, source = "environment", verbose = TRUE, ...) {
  if (inherits(x, "list") && object_has_names(x, "gam")) {
    x <- x$gam
    class(x) <- c(class(x), c("glm", "lm"))
  }

  # try to recover data from environment
  model_data <- .get_data_from_environment(x, source = source, verbose = verbose, ...)

  # fall back to extract data from model frame
  if (is.null(model_data)) {
    mf <- tryCatch(
      if (inherits(x, "Zelig-relogit")) {
        .get_zelig_relogit_frame(x)
      } else {
        stats::model.frame(x)
      },
      error = function(x) NULL
    )
    # process arguments, check whether data should be recovered from
    # environment or model frame
    source <- .check_data_source_arg(source)
    # if no data found, extract from environment - we repeat this step here
    # in case the source was not already environment
    if ((is.null(mf) || nrow(mf) == 0) && source != "environment") {
      mf <- tryCatch(
        {
          dat <- .recover_data_from_environment(x, verbose = verbose)
          vars <- find_variables(x, flatten = TRUE, verbose = FALSE)
          dat[, intersect(vars, colnames(dat)), drop = FALSE]
        },
        error = function(x) {
          NULL
        }
      )
    }
    model_data <- .prepare_get_data(x, mf, verbose = verbose)
  }
  model_data
}


#' @export
get_data.data.frame <- function(x, ...) {
  x
}

#' @export
get_data.censReg <- get_data.default

#' @export
get_data.maxLik <- get_data.default

#' @export
get_data.maxim <- get_data.default


#' @export
get_data.summary.lm <- function(x, verbose = TRUE, ...) {
  mf <- tryCatch(
    .recover_data_from_environment(x, verbose = verbose)[, all.vars(x$terms), drop = FALSE],
    error = function(x) NULL
  )
  .prepare_get_data(x, mf, verbose = verbose)
}


#' @export
get_data.model_fit <- function(x, verbose = TRUE, ...) {
  get_data(x$fit, verbose = verbose, ...)
}


#' @export
get_data.mhurdle <- function(x, verbose = TRUE, ...) {
  x[["model"]]
}


# classical and survival models -----------------------------------------------


#' @export
get_data.mjoint <- function(x, source = "environment", verbose = TRUE, ...) {
  # try to recover data from environment
  model_data <- .get_data_from_environment(x, source = source, verbose = verbose, ...)

  if (!is.null(model_data)) {
    return(model_data)
  }

  # fall back to extract data from model frame
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


#' @export
get_data.geeglm <- function(x,
                            effects = "all",
                            source = "environment",
                            verbose = TRUE,
                            ...) {
  # try to recover data from environment
  model_data <- .get_data_from_environment(
    x,
    effects = effects,
    source = source,
    verbose = verbose,
    ...
  )

  if (!is.null(model_data)) {
    return(model_data)
  }

  # fall back to extract data from model frame
  effects <- match.arg(effects, choices = c("all", "fixed", "random"))
  mf <- tryCatch(stats::model.frame(x), error = function(x) NULL)
  if (!is.null(mf)) {
    id <- data.frame(x$id)
    colnames(id) <- deparse(parse(text = safe_deparse(get_call(x)))[[1]][["id"]])
    mf <- cbind(mf, id)
    # select effects
    vars <- switch(effects,
      all = find_variables(x, flatten = TRUE, verbose = FALSE),
      fixed = find_variables(x, effects = "fixed", flatten = TRUE, verbose = FALSE),
      random = find_random(x, flatten = TRUE)
    )
    mf <- mf[, intersect(vars, colnames(mf)), drop = FALSE]
  }
  .prepare_get_data(x, mf, effects = effects, verbose = verbose)
}

#' @export
get_data.glmgee <- get_data.geeglm


#' @export
get_data.gee <- function(x,
                         effects = "all",
                         source = "environment",
                         verbose = TRUE,
                         ...) {
  # try to recover data from environment
  model_data <- .get_data_from_environment(x,
    effects = effects,
    source = source,
    verbose = verbose,
    ...
  )

  if (!is.null(model_data)) {
    return(model_data)
  }

  # fall back to extract data from model frame
  effects <- match.arg(effects, choices = c("all", "fixed", "random"))
  mf <- tryCatch(
    {
      dat <- .recover_data_from_environment(x, verbose = verbose)
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


#' @export
get_data.rqss <- function(x,
                          component = "all",
                          source = "environment",
                          verbose = TRUE,
                          ...) {
  # try to recover data from environment
  model_data <- .get_data_from_environment(x,
    component = component,
    source = source,
    verbose = verbose,
    ...
  )

  if (!is.null(model_data)) {
    return(model_data)
  }

  # fall back to extract data from model frame
  component <- match.arg(component, choices = c("all", "conditional", "smooth_terms"))

  mf <- tryCatch(
    {
      dat <- .recover_data_from_environment(x, verbose = verbose)
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
get_data.gls <- function(x, source = "environment", verbose = TRUE, ...) {
  # try to recover data from environment
  model_data <- .get_data_from_environment(x, source = source, verbose = verbose, ...)

  if (!is.null(model_data)) {
    return(model_data)
  }

  # fall back to extract data from model frame
  mf <- tryCatch(
    {
      dat <- .recover_data_from_environment(x, verbose = verbose)
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


#' @export
get_data.lqmm <- function(x, source = "environment", verbose = TRUE, ...) {
  # try to recover data from environment
  model_data <- .get_data_from_environment(x, source = source, verbose = verbose, ...)

  if (!is.null(model_data)) {
    return(model_data)
  }

  # fall back to extract data from model frame
  mf <- tryCatch(x$mfArgs$data, error = function(x) NULL)
  .prepare_get_data(x, stats::na.omit(mf), verbose = verbose)
}

#' @export
get_data.nls <- get_data.gls

#' @export
get_data.gnls <- get_data.gls


# zero-inflated models -------------------------------------------------------


#' @export
get_data.hurdle <- function(x,
                            component = "all",
                            source = "environment",
                            verbose = TRUE,
                            ...) {
  # try to recover data from environment
  model_data <- .get_data_from_environment(x,
    component = component,
    source = source,
    verbose = verbose,
    ...
  )

  if (!is.null(model_data)) {
    return(model_data)
  }

  # fall back to extract data from model frame
  component <- match.arg(component, choices = c("all", "conditional", "zi", "zero_inflated", "dispersion"))
  .return_zeroinf_data(x, component, verbose = verbose)
}

#' @export
get_data.zeroinfl <- get_data.hurdle

#' @export
get_data.zerotrunc <- get_data.hurdle


#' @export
get_data.zcpglm <- function(x,
                            component = "all",
                            source = "environment",
                            verbose = TRUE,
                            ...) {
  # try to recover data from environment
  model_data <- .get_data_from_environment(x,
    component = component,
    source = source,
    verbose = verbose,
    ...
  )

  if (!is.null(model_data)) {
    return(model_data)
  }

  # fall back to extract data from model frame
  component <- match.arg(component, choices = c("all", "conditional", "zi", "zero_inflated"))

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
    all = do.call(cbind, compact_list(list(mf_tweedie, mf_zero))),
    conditional = mf_tweedie,
    zi = ,
    zero_inflated = mf_zero
  )
  .prepare_get_data(x, stats::na.omit(mf), verbose = verbose)
}


# mixed models -------------------------------------------------------------


#' @rdname get_data
#' @export
get_data.glmmTMB <- function(x,
                             effects = "all",
                             component = "all",
                             source = "environment",
                             verbose = TRUE,
                             ...) {
  # try to recover data from environment
  model_data <- .get_data_from_environment(
    x,
    effects = effects,
    component = component,
    source = source,
    verbose = verbose,
    ...
  )

  if (!is.null(model_data)) {
    return(model_data)
  }

  # fall back to extract data from model frame
  effects <- match.arg(effects, choices = c("all", "fixed", "random"))
  component <- match.arg(component,
    choices = c("all", "conditional", "zi", "zero_inflated", "dispersion")
  )

  model.terms <- find_variables(
    x,
    effects = "all",
    component = "all",
    flatten = FALSE,
    verbose = FALSE
  )

  mf <- tryCatch(stats::model.frame(x), error = function(x) NULL)
  mf <- .prepare_get_data(x, mf, effects, verbose = verbose)

  # add variables from other model components
  mf <- .add_zeroinf_data(x, mf, model.terms$dispersion)
  mf <- .add_zeroinf_data(x, mf, model.terms$zero_inflated)
  mf <- .add_zeroinf_data(x, mf, model.terms$zero_inflated_random)
  .return_combined_data(x, mf, effects, component, model.terms, verbose = verbose)
}


#' @export
get_data.merMod <- function(x,
                            effects = "all",
                            source = "environment",
                            verbose = TRUE,
                            ...) {
  # try to recover data from environment
  model_data <- .get_data_from_environment(x,
    effects = effects,
    source = source,
    verbose = verbose,
    ...
  )

  if (!is.null(model_data)) {
    return(model_data)
  }

  # fall back to extract data from model frame
  effects <- match.arg(effects, choices = c("all", "fixed", "random"))

  mf <- .safe({
    switch(effects,
      fixed = stats::model.frame(x, fixed.only = TRUE),
      all = stats::model.frame(x, fixed.only = FALSE),
      random = stats::model.frame(x, fixed.only = FALSE)[, find_random(x, split_nested = TRUE, flatten = TRUE), drop = FALSE] # nolint
    )
  })
  .prepare_get_data(x, mf, effects, verbose = verbose)
}


#' @export
get_data.mmrm <- function(x,
                          effects = "all",
                          source = "environment",
                          verbose = TRUE,
                          ...) {
  effects <- match.arg(effects, choices = c("all", "fixed", "random"))
  # find variables
  fixed_vars <- find_variables(x, effects = "fixed", flatten = TRUE)
  random_vars <- find_random(x, split_nested = TRUE, flatten = TRUE)
  # data from model frame
  mf <- .prepare_get_data(x, stats::model.frame(x), effects, verbose = verbose)
  tryCatch(
    switch(effects,
      fixed = mf[intersect(colnames(mf), fixed_vars)],
      all = mf[intersect(colnames(mf), unique(c(fixed_vars, random_vars)))],
      random = mf[intersect(colnames(mf), random_vars)]
    ),
    error = function(x) NULL
  )
}

#' @export
get_data.mmrm_fit <- get_data.mmrm

#' @export
get_data.mmrm_tmb <- get_data.mmrm


#' @export
get_data.merModList <- function(x, effects = "all", ...) {
  format_warning("Can't access data for `merModList` objects.")
  return(NULL)
}


#' @export
get_data.MANOVA <- function(x,
                            effects = "all",
                            source = "environment",
                            verbose = TRUE,
                            ...) {
  # try to recover data from environment
  model_data <- .get_data_from_environment(x, effects = effects, source = source, verbose = verbose, ...)

  if (!is.null(model_data)) {
    return(model_data)
  }

  # fall back to extract data from model frame
  effects <- match.arg(effects, choices = c("all", "fixed", "random"))

  mf <- .safe({
    switch(effects,
      fixed = .remove_column(x$input$data, x$input$subject),
      all = x$input$data,
      random = x$input$data[, x$input$subject, drop = FALSE]
    )
  })
  .prepare_get_data(x, mf, effects, verbose = verbose)
}

#' @export
get_data.RM <- get_data.MANOVA


#' @export
get_data.cpglmm <- function(x,
                            effects = "all",
                            source = "environment",
                            verbose = TRUE,
                            ...) {
  # try to recover data from environment
  model_data <- .get_data_from_environment(x, effects = effects, source = source, verbose = verbose, ...)

  if (!is.null(model_data)) {
    return(model_data)
  }

  # fall back to extract data from model frame
  effects <- match.arg(effects, choices = c("all", "fixed", "random"))
  dat <- stats::model.frame(x)

  mf <- tryCatch(
    switch(effects,
      fixed = dat[, find_predictors(x, effects = "fixed", flatten = TRUE, verbose = FALSE), drop = FALSE],
      all = dat,
      random = dat[, find_random(x, split_nested = TRUE, flatten = TRUE), drop = FALSE]
    ),
    error = function(x) NULL
  )
  .prepare_get_data(x, mf, effects, verbose = verbose)
}

#' @export
get_data.HLfit <- get_data.cpglmm


#' @export
get_data.glmm <- function(x,
                          effects = "all",
                          source = "environment",
                          verbose = TRUE,
                          ...) {
  # try to recover data from environment
  model_data <- .get_data_from_environment(x,
    effects = effects,
    source = source,
    verbose = verbose,
    ...
  )

  if (!is.null(model_data)) {
    return(model_data)
  }

  # fall back to extract data from model frame
  effects <- match.arg(effects, choices = c("all", "fixed", "random"))
  dat <- get_data.default(x, verbose = verbose)

  mf <- .safe({
    switch(effects,
      fixed = dat[, find_predictors(
        x,
        effects = "fixed",
        flatten = TRUE,
        verbose = FALSE
      ), drop = FALSE],
      all = dat,
      random = dat[, find_random(x, split_nested = TRUE, flatten = TRUE), drop = FALSE]
    )
  })
  .prepare_get_data(x, mf, effects, verbose = verbose)
}


#' @export
get_data.mixor <- function(x,
                           effects = "all",
                           source = "environment",
                           verbose = TRUE,
                           ...) {
  # try to recover data from environment
  model_data <- .get_data_from_environment(x,
    effects = effects,
    source = source,
    verbose = verbose,
    ...
  )

  if (!is.null(model_data)) {
    return(model_data)
  }

  # fall back to extract data from model frame
  effects <- match.arg(effects, choices = c("all", "fixed", "random"))

  mf <- tryCatch(
    switch(effects,
      fixed = stats::model.frame(x),
      all = cbind(stats::model.frame(x), x$id),
      random = data.frame(x$id)
    ),
    error = function(x) NULL
  )
  fix_cn <- which(colnames(mf) %in% c("x.id", "x$id"))
  colnames(mf)[fix_cn] <- safe_deparse(x$call$id)

  .prepare_get_data(x, mf, effects, verbose = verbose)
}


#' @export
get_data.glmmadmb <- function(x,
                              effects = "all",
                              source = "environment",
                              verbose = TRUE,
                              ...) {
  # try to recover data from environment
  model_data <- .get_data_from_environment(x,
    effects = effects,
    source = source,
    verbose = verbose,
    ...
  )

  if (!is.null(model_data)) {
    return(model_data)
  }

  # fall back to extract data from model frame
  effects <- match.arg(effects, choices = c("all", "fixed", "random"))

  fixed_data <- x$frame
  random_data <- .recover_data_from_environment(x, verbose = verbose)[, find_random(x, split_nested = TRUE, flatten = TRUE), drop = FALSE]

  mf <- .safe({
    switch(effects,
      fixed = fixed_data,
      all = cbind(fixed_data, random_data),
      random = random_data
    )
  })
  .prepare_get_data(x, mf, effects, verbose = verbose)
}


#' @export
get_data.rlmerMod <- function(x, effects = "all", source = "environment", verbose = TRUE, ...) {
  # try to recover data from environment
  model_data <- .get_data_from_environment(x, effects = effects, source = source, verbose = verbose, ...)

  if (!is.null(model_data)) {
    return(model_data)
  }

  # fall back to extract data from model frame
  effects <- match.arg(effects, choices = c("all", "fixed", "random"))
  .get_data_from_modelframe(x, stats::model.frame(x), effects)
}

#' @export
get_data.clmm <- get_data.rlmerMod


#' @export
get_data.mixed <- function(x,
                           effects = "all",
                           source = "environment",
                           verbose = TRUE,
                           ...) {
  # try to recover data from environment
  model_data <- .get_data_from_environment(x,
    effects = effects,
    source = source,
    verbose = verbose
  )

  if (!is.null(model_data)) {
    return(model_data)
  }

  # fall back to extract data from model frame
  effects <- match.arg(effects, choices = c("all", "fixed", "random"))
  .get_data_from_modelframe(x, x$data, effects)
}


#' @export
#' @rdname get_data
#' @param shape Return long or wide data? Only applicable in repeated measures
#'   designs.
get_data.afex_aov <- function(x, shape = c("long", "wide"), ...) {
  if (length(attr(x, "within"))) {
    shape <- match.arg(shape)
  } else {
    shape <- "long"
  }
  x$data[[shape]]
}


#' @export
get_data.sem <- function(x,
                         effects = "all",
                         source = "environment",
                         verbose = TRUE,
                         ...) {
  # try to recover data from environment
  model_data <- .get_data_from_environment(x,
    effects = effects,
    source = source,
    verbose = verbose,
    ...
  )

  if (!is.null(model_data)) {
    return(model_data)
  }

  # fall back to extract data from model frame
  effects <- match.arg(effects, choices = c("all", "fixed", "random"))
  mf <- tryCatch(
    {
      dat <- .recover_data_from_environment(x, verbose = verbose)
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


#' @export
get_data.lme <- function(x, effects = "all", source = "environment", verbose = TRUE, ...) {
  # try to recover data from environment
  model_data <- .get_data_from_environment(x, effects = effects, source = source, verbose = verbose, ...)

  if (!is.null(model_data)) {
    return(model_data)
  }

  # fall back to extract data from model frame
  effects <- match.arg(effects, choices = c("all", "fixed", "random"))
  dat <- .safe(x$data)

  stats::na.omit(.get_data_from_modelframe(x, dat, effects))
}


#' @export
get_data.MixMod <- function(x,
                            effects = "all",
                            component = "all",
                            source = "environment",
                            verbose = TRUE,
                            ...) {
  # try to recover data from environment
  model_data <- .get_data_from_environment(
    x,
    effects = effects,
    component = component,
    source = source,
    verbose = verbose,
    ...
  )

  if (!is.null(model_data)) {
    return(model_data)
  }

  # fall back to extract data from model frame
  effects <- match.arg(effects, choices = c("all", "fixed", "random"))
  component <- match.arg(component, choices = c("all", "conditional", "zi", "zero_inflated", "dispersion"))

  tryCatch(
    {
      fitfram <- stats::model.frame(x, type = "fixed")
      fitfram_re <- stats::model.frame(x, type = "random")
      fitfram_zi <- stats::model.frame(x, type = "zi_fixed")
      fitfram_zi_re <- stats::model.frame(x, type = "zi_random")

      if (!is_empty_object(fitfram_re)) {
        for (i in seq_along(fitfram_re)) {
          fitfram <- .merge_dataframes(fitfram_re[[i]], fitfram, replace = TRUE)
        }
      }
      if (!is_empty_object(fitfram_zi)) {
        fitfram <- .merge_dataframes(fitfram_zi, fitfram, replace = TRUE)
      }
      if (!is_empty_object(fitfram_zi_re)) {
        for (i in seq_along(fitfram_zi_re)) {
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
get_data.BBmm <- function(x, effects = "all", source = "environment", verbose = TRUE, ...) {
  # try to recover data from environment
  model_data <- .get_data_from_environment(x, effects = effects, source = source, verbose = verbose, ...)

  if (!is.null(model_data)) {
    return(model_data)
  }

  # fall back to extract data from model frame
  effects <- match.arg(effects, choices = c("all", "fixed", "random"))
  mf <- tryCatch(
    {
      dat <- .recover_data_from_environment(x, verbose = verbose)[, find_variables(x, flatten = TRUE), drop = FALSE]
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
get_data.glimML <- function(x, effects = "all", source = "environment", verbose = TRUE, ...) {
  # try to recover data from environment
  model_data <- .get_data_from_environment(x, effects = effects, source = source, verbose = verbose, ...)

  if (!is.null(model_data)) {
    return(model_data)
  }

  # fall back to extract data from model frame
  effects <- match.arg(effects, choices = c("all", "fixed", "random"))
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
get_data.lavaan <- function(x, source = "environment", verbose = TRUE, ...) {
  # try to recover data from environment
  if (identical(source, "environment")) {
    model_data <- .safe(.recover_data_from_environment(x, verbose = verbose), NULL)

    if (!is.null(model_data)) {
      return(model_data)
    }
  }

  # fall back to extract data from model frame
  check_if_installed("lavaan")
  as.data.frame(lavaan::lavInspect(x, what = "data"))
}

#' @export
get_data.blavaan <- get_data.lavaan


# additive models (gam) -------------------------------------

#' @export
get_data.gam <- function(x, source = "environment", verbose = TRUE, ...) {
  # try to recover data from environment
  model_data <- .get_data_from_environment(x, source = source, verbose = verbose, ...)

  if (!is.null(model_data)) {
    return(model_data)
  }

  mf <- tryCatch(stats::model.frame(x), error = function(x) NULL)
  .prepare_get_data(x, mf, verbose = verbose)
}


#' @export
get_data.list <- function(x, source = "environment", verbose = TRUE, ...) {
  model_data <- NULL
  if (any(c("gam", "gamm") %in% names(x))) {
    x <- x$gam
    class(x) <- c(class(x), c("glm", "lm"))
    mf <- tryCatch(stats::model.frame(x), error = function(x) NULL)
    model_data <- .prepare_get_data(x, mf, verbose = verbose)
  } else {
    get_data.default(x, source = source, verbose = verbose, ...)
  }
  model_data
}


#' @export
get_data.vgam <- function(x, source = "environment", verbose = TRUE, ...) {
  # try to recover data from environment
  model_data <- .get_data_from_environment(x, source = source, verbose = verbose, ...)

  if (!is.null(model_data)) {
    return(model_data)
  }

  # fall back to extract data from model frame
  mf <- tryCatch(
    get(x@misc$dataname, envir = parent.frame())[, find_variables(x, flatten = TRUE), drop = FALSE],
    error = function(x) NULL
  )

  .prepare_get_data(x, mf, verbose = verbose)
}


#' @export
get_data.gamm <- function(x, verbose = TRUE, ...) {
  x <- x$gam
  class(x) <- c(class(x), c("glm", "lm"))

  mf <- tryCatch(stats::model.frame(x), error = function(x) NULL)
  .prepare_get_data(x, mf, verbose = verbose)
}


#' @export
get_data.gamlss <- function(x, source = "environment", verbose = TRUE, ...) {
  # try to recover data from environment
  model_data <- .get_data_from_environment(x, source = source, verbose = verbose, ...)

  if (!is.null(model_data)) {
    return(model_data)
  }

  # fall back to extract data from model frame
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

      if (length(mf_list) > 1L) {
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
get_data.felm <- function(x, effects = "all", source = "environment", verbose = TRUE, ...) {
  # try to recover data from environment
  model_data <- .get_data_from_environment(x, effects = effects, source = source, verbose = verbose, ...)

  if (!is.null(model_data)) {
    return(model_data)
  }

  # fall back to extract data from model frame
  # original data does not appear to be stored in the model object
  effects <- match.arg(effects, choices = c("all", "fixed", "random"))
  .get_data_from_modelframe(x, stats::model.frame(x), effects, verbose = verbose)
}


#' @export
get_data.feis <- function(x, effects = "all", source = "environment", verbose = TRUE, ...) {
  # try to recover data from environment
  model_data <- .get_data_from_environment(x, effects = effects, source = source, verbose = verbose, ...)

  if (!is.null(model_data)) {
    return(model_data)
  }

  # fall back to extract data from model frame
  # original data does not appear to be stored in the model object
  effects <- match.arg(effects, choices = c("all", "fixed", "random"))
  mf <- tryCatch(.recover_data_from_environment(x, verbose = verbose),
    error = function(x) stats::model.frame(x)
  )
  .get_data_from_modelframe(x, mf, effects, verbose = verbose)
}


#' @export
get_data.fixest <- function(x, source = "environment", verbose = TRUE, ...) {
  # try to recover data from environment
  model_data <- .get_data_from_environment(x, source = source, verbose = verbose, ...)

  if (!is.null(model_data)) {
    return(model_data)
  }

  # fall back to extract data from model frame
  # original data does not appear to be stored in the model object
  # see https://github.com/lrberge/fixest/issues/340 and #629
  model_call <- get_call(x)
  mf <- eval(model_call$data, envir = parent.env(x$call_env))
  # mf <- .recover_data_from_environment(x, verbose = verbose)
  .get_data_from_modelframe(x, mf, effects = "all", verbose = verbose)
}


#' @export
get_data.feglm <- function(x, source = "environment", verbose = TRUE, ...) {
  # try to recover data from environment
  model_data <- .get_data_from_environment(x, source = source, verbose = verbose, ...)

  if (!is.null(model_data)) {
    return(model_data)
  }

  # fall back to extract data from model frame
  mf <- as.data.frame(x$data)
  .get_data_from_modelframe(x, mf, effects = "all", verbose = verbose)
}


#' @export
get_data.pgmm <- function(x, source = "environment", verbose = TRUE, ...) {
  # try to recover data from environment
  model_data <- .get_data_from_environment(x, source = source, verbose = verbose, ...)

  if (!is.null(model_data)) {
    return(model_data)
  }

  # fall back to extract data from model frame
  model_terms <- find_variables(x, effects = "all", component = "all", flatten = TRUE)
  mf <- tryCatch(.recover_data_from_environment(x, verbose = verbose)[, model_terms, drop = FALSE],
    error = function(x) NULL
  )
  .prepare_get_data(x, mf, verbose = verbose)
}


#' @export
get_data.plm <- function(x, source = "environment", verbose = TRUE, ...) {
  # extract index variables
  index <- eval(get_call(x)$index)
  # try to recover data from environment
  # avoid feeding the same argument twice
  if ("additional_variables" %in% names(list(...))) {
    model_data <- .get_data_from_environment(x, source = source, verbose = verbose, ...)
  } else {
    model_data <- .get_data_from_environment(x, source = source, additional_variables = index, verbose = verbose, ...)
  }

  if (!is.null(model_data)) {
    return(model_data)
  }

  # fall back to extract data from model frame
  mf <- stats::model.frame(x)
  model_terms <- find_variables(x, effects = "all", component = "all", flatten = TRUE)
  cn <- colnames(mf)
  mf <- as.data.frame(lapply(mf, function(i) {
    if (is.factor(i)) {
      as.factor(i)
    } else if (is.character(i)) {
      as.character(i)
    } else if (is.integer(i)) {
      as.integer(i)
    } else if (is.numeric(i)) {
      as.numeric(i)
    } else if (is.logical(i)) {
      as.logical(i)
    } else {
      as.vector(i)
    }
  }))
  colnames(mf) <- clean_names(cn)

  # find index variables
  index <- eval(parse(text = safe_deparse(x$call))[[1]]$index)

  # try to get index variables from orignal data
  if (!is.null(index)) {
    original_data <- .recover_data_from_environment(x, verbose = verbose)
    keep <- intersect(index, colnames(original_data))
    if (length(keep)) {
      mf <- cbind(mf, original_data[, keep, drop = FALSE])
      model_terms <- c(model_terms, keep)
    }
  }

  .prepare_get_data(x, mf[, model_terms, drop = FALSE], verbose = verbose)
}


#' @export
get_data.wbm <- function(x, effects = "all", verbose = TRUE, ...) {
  effects <- match.arg(effects, choices = c("all", "fixed", "random"))
  mf <- stats::model.frame(x)

  # dat <- as.data.frame(x@orig_data)

  if (effects == "random") {
    return(stats::na.omit(mf[, unique(find_random(x, split_nested = TRUE, flatten = TRUE)), drop = FALSE]))
  }

  resp.col <- which(colnames(mf) == find_response(x))
  mf <- mf[, c(resp.col, (seq_len(ncol(mf)))[-resp.col])]

  .prepare_get_data(x, stats::na.omit(mf), effects, verbose = verbose)
}


#' @export
get_data.wbgee <- get_data.wbm


#' @export
get_data.ivreg <- function(x, source = "environment", verbose = TRUE, ...) {
  # try to recover data from environment
  model_data <- .get_data_from_environment(x, source = source, verbose = verbose, ...)

  if (!is.null(model_data)) {
    return(model_data)
  }

  # fall back to extract data from model frame
  mf <- .safe(stats::model.frame(x))
  ft <- find_variables(x, flatten = TRUE)

  if (is_empty_object(mf)) {
    final_mf <- .safe({
      dat <- .recover_data_from_environment(x, verbose = verbose)
      dat[, ft, drop = FALSE]
    })
  } else {
    cn <- clean_names(colnames(mf))
    remain <- setdiff(ft, cn)
    if (is_empty_object(remain)) {
      final_mf <- mf
    } else {
      final_mf <- .safe({
        dat <- .recover_data_from_environment(x, verbose = verbose)
        cbind(mf, dat[, remain, drop = FALSE])
      })
    }
  }

  .prepare_get_data(x, stats::na.omit(final_mf), verbose = verbose)
}


#' @export
get_data.iv_robust <- get_data.ivreg


#' @export
get_data.ivprobit <- function(x, source = "environment", verbose = TRUE, ...) {
  # try to recover data from environment
  model_data <- .get_data_from_environment(x, source = source, verbose = verbose, ...)

  if (!is.null(model_data)) {
    return(model_data)
  }

  # fall back to extract data from model frame
  .prepare_get_data(x, stats::na.omit(as.data.frame(x$mr1)), verbose = verbose)
}


#' @export
get_data.bife <- function(x, effects = "all", source = "environment", verbose = TRUE, ...) {
  # try to recover data from environment
  model_data <- .get_data_from_environment(x, effects = effects, source = source, verbose = verbose, ...)

  if (!is.null(model_data)) {
    return(model_data)
  }

  # fall back to extract data from model frame
  effects <- match.arg(effects, choices = c("all", "fixed", "random"))
  mf <- as.data.frame(x$data)

  if (effects == "random") {
    return(stats::na.omit(mf[, unique(find_random(x, split_nested = TRUE, flatten = TRUE)), drop = FALSE]))
  } else if (effects == "fixed") {
    mf <- mf[, setdiff(colnames(mf), unique(find_random(x, split_nested = TRUE, flatten = TRUE))), drop = FALSE]
  }

  .prepare_get_data(x, stats::na.omit(mf), effects, verbose = verbose)
}


# Bayesian regression ---------------------------------------------------


#' @export
get_data.brmsfit <- function(x, effects = "all", component = "all", source = "environment", verbose = FALSE, ...) {
  # try to recover data from environment
  # verbose is FALSE by default because `get_call()` often does not work on
  # `brmsfit` objects, so we typically default to the `data` held in the object.
  data_name <- attr(x$data, "data_name")
  model_data <- .get_data_from_environment(
    x,
    effects = effects,
    component = component,
    source = source,
    verbose = verbose,
    data_name = data_name,
    ...
  )

  if (!is.null(model_data)) {
    return(model_data)
  }

  # fall back to extract data from model frame
  effects <- match.arg(effects, choices = c("all", "fixed", "random"))
  component <- match.arg(component, choices = c("all", .all_elements()))

  model.terms <- find_variables(x, effects = "all", component = "all", flatten = FALSE)
  mf <- stats::model.frame(x)

  if (.is_multi_membership(x)) {
    model.terms <- lapply(model.terms, .clean_brms_mm)
    rs <- setdiff(
      unlist(find_random_slopes(x), use.names = FALSE),
      unlist(model.terms, use.names = FALSE)
    )
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


#' @export
get_data.stanreg <- function(x, effects = "all", source = "environment", verbose = TRUE, ...) {
  # try to recover data from environment
  model_data <- .get_data_from_environment(x, effects = effects, source = source, verbose = verbose, ...)

  if (!is.null(model_data)) {
    return(model_data)
  }

  # fall back to extract data from model frame
  effects <- match.arg(effects, choices = c("all", "fixed", "random"))

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


#' @export
get_data.MCMCglmm <- function(x, effects = "all", source = "environment", verbose = TRUE, ...) {
  # try to recover data from environment
  model_data <- .get_data_from_environment(x, effects = effects, source = source, verbose = verbose, ...)

  if (!is.null(model_data)) {
    return(model_data)
  }

  # fall back to extract data from model frame
  effects <- match.arg(effects, choices = c("all", "fixed", "random"))
  mf <- tryCatch(
    {
      env_dataframes <- names(which(unlist(eapply(.GlobalEnv, is.data.frame))))
      pv <- find_predictors(x, effects = effects, component = "all", flatten = TRUE)
      matchframe <- unlist(lapply(env_dataframes, function(.x) {
        dat <- get(.x)
        all(pv %in% colnames(dat))
      }))
      mf <- env_dataframes[matchframe][1]
      if (is.na(mf)) {
        NULL
      } else {
        dat <- get(mf)
        switch(effects,
          fixed = dat[, setdiff(colnames(dat), find_random(x, flatten = TRUE)), drop = FALSE],
          all = dat,
          random = dat[, find_random(x, flatten = TRUE), drop = FALSE]
        )
      }
    },
    error = function(x) {
      NULL
    }
  )

  .prepare_get_data(x, mf, effects = effects, verbose = verbose)
}


#' @export
get_data.stanmvreg <- function(x, source = "environment", verbose = TRUE, ...) {
  # try to recover data from environment
  model_data <- .get_data_from_environment(x, source = source, verbose = verbose, ...)

  if (!is.null(model_data)) {
    return(model_data)
  }

  # fall back to extract data from model frame
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
get_data.svy_vglm <- function(x, source = "environment", verbose = TRUE, ...) {
  # try to recover data from environment
  model_data <- .get_data_from_environment(x, source = source, verbose = verbose, ...)

  if (!is.null(model_data)) {
    return(model_data)
  }

  # fall back to extract data from model frame
  mf <- x$design$variables[find_variables(x, flatten = TRUE)]
  .prepare_get_data(x, mf, verbose = verbose)
}


#' @export
get_data.mediate <- function(x, source = "environment", verbose = TRUE, ...) {
  d1 <- get_data(x$model.m, source = source, verbose = verbose)
  d2 <- get_data(x$model.y, source = source, verbose = verbose)
  merge(d1, d2, sort = FALSE, all = TRUE)
}


#' @export
get_data.mle2 <- function(x, ...) {
  as.data.frame(do.call(cbind, x@data))
}

#' @export
get_data.mle <- get_data.mle2


#' @export
get_data.nestedLogit <- function(x, ...) {
  d <- x$data
  if (!is.null(x$subset)) {
    d <- subset(d, eval(parse(text = x$subset), envir = d))
  }
  d
}


#' @export
get_data.glht <- function(x, source = "environment", verbose = TRUE, ...) {
  get_data(x$model, source = source, verbose = verbose, ...)
}


#' @export
get_data.averaging <- function(x, ...) {
  ml <- attributes(x)$modelList
  if (is.null(ml)) {
    format_warning("Can't retrieve data. Please use `fit = TRUE` in `model.avg()`.")
    return(NULL)
  }

  mf <- tryCatch(
    Reduce(function(x, y) merge(x, y, all = TRUE, sort = FALSE), lapply(ml, stats::model.frame)),
    error = function(x) NULL
  )

  if (is.null(mf)) {
    mf <- tryCatch(
      .recover_data_from_environment(x)[, find_variables(x, flatten = TRUE), drop = FALSE],
      error = function(x) NULL
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
get_data.coxph <- function(x, source = "environment", verbose = TRUE, ...) {
  # try to recover data from environment
  model_data <- .get_data_from_environment(x, source = source, verbose = verbose, ...)

  if (!is.null(model_data)) {
    return(model_data)
  }

  # fall back to extract data from model frame
  # first try, parent frame
  dat <- tryCatch(
    {
      mf <- .recover_data_from_environment(x, verbose = verbose)
      mf <- .prepare_get_data(x, stats::na.omit(mf), verbose = FALSE)
    },
    error = function(x) NULL
  )

  # second try, default extractor. Less good because of coercion to other types
  if (is.null(dat)) {
    # second try, global env
    dat <- get_data.default(x, source = source, verbose = verbose, ...)
  }

  dat
}

#' @export
get_data.coxme <- get_data.coxph


#' @export
get_data.BGGM <- function(x, ...) {
  x$Y
}


#' @export
get_data.mcmc.list <- function(x, ...) {
  NULL
}


#' @export
get_data.DirichletRegModel <- function(x, source = "environment", verbose = TRUE, ...) {
  # try to recover data from environment
  model_data <- .get_data_from_environment(x, source = source, verbose = verbose, ...)

  if (!is.null(model_data)) {
    return(model_data)
  }

  # fall back to extract data from model frame
  mf <- x$data
  resp <- vapply(x$data, inherits, TRUE, "DirichletRegData")
  .prepare_get_data(x, mf[!resp], verbose = verbose)
}


#' @export
get_data.vglm <- function(x, source = "environment", verbose = TRUE, ...) {
  # try to recover data from environment
  model_data <- .get_data_from_environment(x, source = source, verbose = verbose, ...)

  if (!is.null(model_data)) {
    return(model_data)
  }

  # fall back to extract data from model frame
  mf <- tryCatch(
    if (length(x@model)) {
      x@model
    } else {
      env <- environment(x@terms$terms)
      if (is.null(env)) env <- parent.frame()
      fcall <- x@call
      fcall$method <- "model.frame"
      fcall$smart <- FALSE
      eval(fcall, env, parent.frame())
    },
    error = function(x) NULL
  )

  .prepare_get_data(x, mf)
}


#' @export
get_data.biglm <- function(x, source = "environment", verbose = TRUE, ...) {
  # try to recover data from environment
  model_data <- .get_data_from_environment(x, source = source, verbose = verbose, ...)

  if (!is.null(model_data)) {
    return(model_data)
  }

  # fall back to extract data from model frame
  mf <- stats::model.frame(x)
  .prepare_get_data(x, mf)
}

#' @export
get_data.bigglm <- get_data.biglm


#' @export
get_data.LORgee <- function(x, source = "environment", effects = "all", verbose = TRUE, ...) {
  # try to recover data from environment
  model_data <- .get_data_from_environment(x, effects = effects, source = source, verbose = verbose, ...)

  if (!is.null(model_data)) {
    return(model_data)
  }

  # fall back to extract data from model frame
  effects <- match.arg(effects, choices = c("all", "fixed", "random"))
  mf <- tryCatch(
    {
      dat <- .recover_data_from_environment(x, verbose = verbose)[, find_variables(x, flatten = TRUE), drop = FALSE]
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
get_data.gmnl <- function(x, source = "environment", verbose = TRUE, ...) {
  # try to recover data from environment
  model_data <- .get_data_from_environment(x, source = source, verbose = verbose, ...)

  if (!is.null(model_data)) {
    return(model_data)
  }

  # fall back to extract data from model frame
  mf <- tryCatch(x$mf, error = function(x) NULL)
  .prepare_get_data(x, mf)
}


#' @export
get_data.gbm <- function(x, source = "environment", verbose = TRUE, ...) {
  # try to recover data from environment
  model_data <- .get_data_from_environment(x, source = source, verbose = verbose, ...)

  if (!is.null(model_data)) {
    return(model_data)
  }

  # fall back to extract data from model frame
  mf <- tryCatch(
    get(safe_deparse(x$call$data), envir = parent.frame())[, find_variables(x, flatten = TRUE), drop = FALSE],
    error = function(x) stats::model.frame(x)
  )

  .get_data_from_modelframe(x, mf, effects = "all")
}


#' @export
get_data.tobit <- function(x, source = "environment", verbose = TRUE, ...) {
  # try to recover data from environment
  model_data <- .get_data_from_environment(x, source = source, verbose = verbose, ...)

  if (!is.null(model_data)) {
    return(model_data)
  }

  # fall back to extract data from model frame
  dat <- .recover_data_from_environment(x, verbose = verbose)
  ft <- find_variables(x, flatten = TRUE, verbose = FALSE)
  remain <- intersect(ft, colnames(dat))

  .prepare_get_data(x, stats::na.omit(dat[, remain, drop = FALSE]), verbose = verbose)
}


#' @export
get_data.clmm2 <- function(x, source = "environment", verbose = TRUE, ...) {
  # try to recover data from environment
  model_data <- .get_data_from_environment(x, source = source, verbose = verbose, ...)

  if (!is.null(model_data)) {
    return(model_data)
  }

  # fall back to extract data from model frame
  mf <- tryCatch(
    {
      data_complete <- x$location
      data_scale <- x$scale

      if (!is.null(data_scale)) {
        remain <- setdiff(colnames(data_scale), colnames(data_complete))
        if (length(remain)) data_complete <- cbind(data_complete, data_scale[, remain, drop = FALSE])
      }

      data_complete <- cbind(data_complete, x$grFac)
      colnames(data_complete)[ncol(data_complete)] <- unlist(.find_random_effects(x, f = find_formula(x, verbose = FALSE), split_nested = TRUE)) # nolint

      data_complete
    },
    error = function(x) {
      NULL
    }
  )

  .prepare_get_data(x, mf)
}


#' @export
get_data.clm2 <- function(x, source = "environment", verbose = TRUE, ...) {
  # try to recover data from environment
  model_data <- .get_data_from_environment(x, source = source, verbose = verbose, ...)

  if (!is.null(model_data)) {
    return(model_data)
  }

  # fall back to extract data from model frame
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
get_data.bracl <- function(x, source = "environment", verbose = TRUE, ...) {
  # try to recover data from environment
  model_data <- .get_data_from_environment(x, source = source, verbose = verbose, ...)

  if (!is.null(model_data)) {
    return(model_data)
  }

  # fall back to extract data from model frame
  mf <- stats::model.frame(x)
  suppressWarnings(.prepare_get_data(x, mf, verbose = verbose))
}


#' @export
get_data.mlogit <- function(x, source = "environment", verbose = TRUE, ...) {
  # try to recover data from environment
  model_data <- .get_data_from_environment(x, source = source, verbose = verbose, ...)

  if (!is.null(model_data)) {
    return(model_data)
  }

  # fall back to extract data from model frame
  mf <- tryCatch(as.data.frame(stats::model.frame(x)), error = function(x) NULL)
  .prepare_get_data(x, mf, verbose = verbose)
}


#' @export
get_data.phylolm <- function(x, source = "environment", verbose = TRUE, ...) {
  # DO NOT TOUCH THE SOURCE ARGUMENT!
  # phylo models have no model.frame() method, so we can only recover from
  # environment. We still need the "source" argument, even if it's not used here,
  #  to avoid the "multiple argument match" error for those instances, where
  # `get_data()` is called # with `source = "frame"`.
  .get_data_from_environment(x, source = "environment", verbose = verbose, ...)
}

#' @export
get_data.phyloglm <- get_data.phylolm


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
get_data.rma <- function(x,
                         source = "environment",
                         verbose = TRUE,
                         include_interval = FALSE,
                         transf = NULL,
                         transf_args = NULL,
                         ci = 0.95,
                         ...) {
  # standard errors and moderators are not found by find_predictors(),
  # so we need them as additional variables
  model_call <- get_call(x)
  additional_variables <- c(
    safe_deparse(model_call$vi),
    safe_deparse(model_call$sei),
    safe_deparse(model_call$mods)
  )
  # try to recover data from environment
  model_data <- .get_data_from_environment(
    x,
    source = source,
    additional_variables = additional_variables,
    verbose = verbose,
    ...
  )

  if (!is.null(model_data)) {
    return(model_data)
  }

  # fall back to extract data from model frame
  mf <- tryCatch(.recover_data_from_environment(x, verbose = verbose), error = function(x) NULL)
  mf_attr <- attributes(mf)
  mf <- merge(mf, data.frame(Weights = get_weights(x)), by = "row.names", all = TRUE, sort = FALSE)
  rownames(mf) <- mf$Row.names
  mf$Row.names <- NULL
  mostattributes(mf) <- c(
    attributes(mf)[c("names", "row.names")],
    mf_attr[c("yi.names", "vi.names", "digits", "class")]
  )
  if (isTRUE(include_interval)) {
    model_response <- .safe(mf[[find_response(x)]])
    sei <- .safe(mf[[model_call$sei]])
    if (is.null(sei)) {
      sei <- .safe(sqrt(mf[[model_call$vi]]))
    }
    if (is.null(sei)) {
      format_error("Could not find `sei` or `vi` for this model.")
    }
    mf$ci <- ci
    mf$CI_low <- model_response - stats::qnorm((1 - ci) / 2, lower.tail = FALSE) * sei
    mf$CI_high <- model_response + stats::qnorm((1 - ci) / 2, lower.tail = FALSE) * sei
    if (!is.null(transf)) {
      if (!is.function(transf)) {
        format_error("`transf` must be a function.")
      }
      if (is.null(transf_args)) {
        mf[[find_response(x)]] <- sapply(mf[[find_response(x)]], transf)
        mf$CI_low <- sapply(mf$CI_low, transf)
        mf$CI_high <- sapply(mf$CI_high, transf)
      } else {
        mf[[find_response(x)]] <- sapply(mf[[find_response(x)]], transf, transf_args)
        mf$CI_low <- sapply(mf$CI_low, transf, transf_args)
        mf$CI_high <- sapply(mf$CI_high, transf, transf_args)
      }
    }
  }
  original_rownames <- rownames(x$X)
  if (is.null(original_rownames)) {
    original_rownames <- seq_len(nrow(mf))
  }
  .prepare_get_data(x, mf[original_rownames, , drop = FALSE], verbose = verbose)
}


#' @export
get_data.metaplus <- function(x, source = "environment", verbose = TRUE, ...) {
  # try to recover data from environment
  model_data <- .get_data_from_environment(x, source = source, verbose = verbose, ...)

  if (!is.null(model_data)) {
    return(model_data)
  }

  # fall back to extract data from model frame
  mf <- .safe(.recover_data_from_environment(x, verbose = verbose))
  .prepare_get_data(x, stats::na.omit(mf), verbose = verbose)
}

#' @export
get_data.ivFixed <- get_data.metaplus


#' @export
get_data.meta_random <- function(x, source = "environment", verbose = TRUE, ...) {
  # try to recover data from environment
  model_data <- .get_data_from_environment(x, source = source, verbose = verbose, ...)

  if (!is.null(model_data)) {
    return(model_data)
  }

  # fall back to extract data from model frame
  mf <- .safe(x$data$data)
  .prepare_get_data(x, stats::na.omit(mf), verbose = verbose)
}

#' @export
get_data.meta_fixed <- get_data.meta_random


#' @export
get_data.meta_bma <- function(x, source = "environment", verbose = TRUE, ...) {
  # try to recover data from environment
  model_data <- .get_data_from_environment(x, source = source, verbose = verbose, ...)

  if (!is.null(model_data)) {
    return(model_data)
  }

  # fall back to extract data from model frame
  mf <- tryCatch(x$meta$fixed$data$data, error = function(x) NULL)
  .prepare_get_data(x, stats::na.omit(mf), verbose = verbose)
}


#' @export
get_data.bfsl <- function(x, ...) {
  as.data.frame(x$data[c("x", "y", "sd_x", "sd_y")])
}


#' @export
get_data.mipo <- function(x, ...) {
  .safe({
    models <- eval(x$call$object)
    get_data(models$analyses[[1]], ...)
  })
}


#' @export
get_data.htest <- function(x, ...) {
  out <- NULL
  if (!is.null(x$data.name)) {
    out <- .retrieve_htest_data(x)
  }
  out
}


# helper -------------

.check_data_source_arg <- function(source) {
  source <- match.arg(source, choices = c("environment", "mf", "modelframe", "frame"))
  switch(source,
    environment = "environment",
    "frame"
  )
}
