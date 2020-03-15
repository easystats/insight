# Function that does the most work for preparing and transforming the data,
# to ensure we have a "clean" data frame from the data that was used to fit
# the model. This also means that, unless necessary for further processing,
# variables transformed during model fitting are not included in this data frame
#
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
    if (inherits(x, c("coxph", "flexsurvreg", "coxme", "survreg", "survfit", "crq", "psm"))) {
      mf <- cbind(mf[[1]][, 1], mf[[1]][, 2], mf)
      colnames(mf)[1:2] <- rn_not_combined
    } else {
      tryCatch(
        {
          trials.data <- as.data.frame(mf[[1]])
          colnames(trials.data) <- rn_not_combined

          # if columns were bound via subtraction, e.g.
          # "cbind(succes, total - success)", we need to sum up success and
          # total for the original total-column.

          pattern <- sprintf("%s(\\s*)-(\\s*)%s", rn_not_combined[2], rn_not_combined[1])
          if (any(grepl(pattern = pattern, x = rn))) {
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
    md <- tryCatch(
      {
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
      if (any(class(mf_matrix[[1]]) == "rms")) class(mf_matrix[[1]]) <- "matrix"
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
        fw <- find_weights(x)
        if (!is.null(fw) && fw %in% colnames(md)) {
          needed.vars <- c(needed.vars, fw)
        }
      }

      if (inherits(x, c("coxph", "coxme"))) {
        mf <- md
      } else {
        needed.vars <- unique(clean_names(needed.vars))
        mf <- md[, needed.vars, drop = FALSE]
        # we need this hack to save variable and value label attributes, if any
        value_labels <- lapply(mf, function(.l) attr(.l, "labels", exact = TRUE))
        variable_labels <- lapply(mf, function(.l) attr(.l, "label", exact = TRUE))
        # removing NAs drops all label-attributes
        mf <- stats::na.omit(mf)
        # then set back attributes
        mf <- as.data.frame(mapply(function(.d, .l) {
          attr(.d, "labels") <- .l
          .d
        }, mf, value_labels, SIMPLIFY = FALSE), stringsAsFactors = FALSE)
        mf <- as.data.frame(mapply(function(.d, .l) {
          attr(.d, "label") <- .l
          .d
        }, mf, variable_labels, SIMPLIFY = FALSE), stringsAsFactors = FALSE)
      }

      # add back model weights, if any
      if (!is.null(mw)) mf$`(weights)` <- mw
    }

    # check if we really have all formula terms in our model frame now
    pv <- tryCatch(
      {
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
    md <- tryCatch(
      {
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
        tmp <- suppressWarnings(cbind(mf, get_weights(x)))
        colnames(tmp)[ncol(tmp)] <- weighting_var
        tmp
      },
      error = function(e) {
        mf
      }
    )
  }

  # add back possible trials-data
  if (!is.null(trials.data)) {
    new.cols <- setdiff(colnames(trials.data), colnames(mf))
    if (!.is_empty_string(new.cols)) mf <- cbind(mf, trials.data[, new.cols, drop = FALSE])
  }

  .add_remaining_missing_variables(x, mf, effects, component = "all")
}



# add remainng variables with special pattern -------------------------------

.add_remaining_missing_variables <- function(model, mf, effects, component) {
  missing_vars <- setdiff(
    find_predictors(model, effects = effects, component = component, flatten = TRUE),
    colnames(mf)
  )

  if (!is.null(missing_vars) && length(missing_vars) > 0) {
    env_data <- .get_data_from_env(model)
    if (!is.null(env_data) && all(missing_vars %in% colnames(env_data))) {
      common_columns <- intersect(colnames(env_data), c(missing_vars, colnames(mf)))
      env_data <- stats::na.omit(env_data[common_columns])
      if (nrow(env_data) == nrow(mf) && !any(missing_vars %in% colnames(mf))) {
        mf <- cbind(mf, env_data[missing_vars])
      }
    }
  }

  mf
}






# combine data from different model components -------------------------------

# This helper functions ensures that data from different model components
# are included in the returned data frame
#
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
      all = c(model.terms$conditional, model.terms$zero_inflated, model.terms$dispersion),
      conditional = model.terms$conditional,
      zi = ,
      zero_inflated = model.terms$zero_inflated,
      dispersion = model.terms$dispersion
    )

    random.component.data <- switch(
      component,
      all = c(model.terms$random, model.terms$zero_inflated_random),
      conditional = model.terms$random,
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

  weights <- find_weights(x)
  # if (!is.null(weights) && "(weights)" %in% colnames(mf)) {
  #   weights <- c(weights, "(weights)")
  # }

  vars <- switch(
    effects,
    all = unique(c(response, fixed.component.data, random.component.data, weights)),
    fixed = unique(c(response, fixed.component.data, weights)),
    random = unique(random.component.data)
  )

  still_missing <- setdiff(vars, colnames(mf))
  vars <- intersect(vars, colnames(mf))
  dat <- mf[, vars, drop = FALSE]

  if (.is_empty_object(dat)) {
    print_color(sprintf("Warning: Data frame is empty, probably component '%s' does not exist in the %s-part of the model?\n", component, effects), "red")
    return(NULL)
  }

  if (length(still_missing)) {
    print_color(sprintf("Warning: Following potential variables could not be found in the data: %s\n", paste0(still_missing, collapse = " ,")), "red")
  }

  if ("(offset)" %in% colnames(mf) && !("(offset)" %in% colnames(dat))) {
    dat <- cbind(dat, mf[["(offset"]])
  }


  dat
}





# find zi-data -----------------------------------

# this function tries to get the data from variables from the zero-inflated
# component and adds them to the model frame. Useful if the zi-component
# has other variables than the count component.
#
.add_zeroinf_data <- function(x, mf, tn) {
  tryCatch(
    {
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




# special model handling -----------------------------------

.get_zelig_relogit_frame <- function(x) {
  vars <- find_variables(x, flatten = TRUE)
  x$data[, vars, drop = FALSE]
}




# combine data from count and zi-component -----------------------------------

.return_zeroinf_data <- function(x, component) {
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




# "clean" model frame and get data -----------------------------------

# here we have a model frame with many variables, so just extract the important ones...
#
.get_data_from_modelframe <- function(x, dat, effects) {
  cn <- clean_names(colnames(dat))

  ft <- switch(
    effects,
    fixed = find_variables(x, effects = "fixed", flatten = TRUE),
    all = find_variables(x, flatten = TRUE),
    random = find_random(x, split_nested = TRUE, flatten = TRUE)
  )

  remain <- intersect(c(ft, find_weights(x)), cn)

  mf <- tryCatch(
    {
      dat[, remain, drop = FALSE]
    },
    error = function(x) {
      dat
    }
  )

  .prepare_get_data(x, mf, effects)
}




# find data from the environment -----------------------------------

# return data from a data frame that is in the environment,
# and subset the data, if necessary
.get_data_from_env <- function(x) {
  # first try, parent frame
  dat <- tryCatch(
    {
      eval(x$call$data, envir = parent.frame())
    },
    error = function(e) {
      NULL
    }
  )

  if (is.null(dat)) {
    # second try, global env
    dat <- tryCatch(
      {
        eval(x$call$data, envir = globalenv())
      },
      error = function(e) {
        NULL
      }
    )
  }


  if (!is.null(dat) && .obj_has_name(x$call, "subset")) {
    dat <- subset(dat, subset = eval(x$call$subset))
  }

  dat
}



# find data from the environment, for models with S4 --------------------------

# return data from a data frame that is in the environment,
# and subset the data, if necessary
.get_S4_data_from_env <- function(x) {
  # first try, parent frame
  dat <- tryCatch(
    {
      eval(x@call$data, envir = parent.frame())
    },
    error = function(e) {
      NULL
    }
  )

  if (is.null(dat)) {
    # second try, global env
    dat <- tryCatch(
      {
        eval(x@call$data, envir = globalenv())
      },
      error = function(e) {
        NULL
      }
    )
  }


  if (!is.null(dat) && .obj_has_name(x@call, "subset")) {
    dat <- subset(dat, subset = eval(x@call$subset))
  }

  dat
}



# find start vector of nlmer-models from the environment -----------------------------------

# return data from a data frame that is in the environment,
# and subset the data, if necessary
.get_startvector_from_env <- function(x) {
  tryCatch(
    {
      sv <- eval(parse(text = .safe_deparse(x@call))[[1]]$start)
      if (is.list(sv)) sv <- sv[["nlpars"]]
      names(sv)
    },
    error = function(e) {
      NULL
    }
  )
}
