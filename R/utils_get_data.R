# Function that does the most work for preparing and transforming the data,
# to ensure we have a "clean" data frame from the data that was used to fit
# the model. This also means that, unless necessary for further processing,
# variables transformed during model fitting are not included in this data frame
#
.prepare_get_data <- function(x, mf, effects = "fixed", verbose = TRUE) {

  # check if we have any data yet
  if (is_empty_object(mf)) {
    if (isTRUE(verbose)) {
      warning("Could not get model data.", call. = FALSE)
    }
    return(NULL)
  }

  # we may store model weights here later
  mw <- NULL


  # offset variables ----------------------------------------------------------

  # do we have an offset, not specified in the formula?

  # This is a bit slower - restore if tests fail...
  # offcol <- grep("^(\\(offset\\)|offset\\((.*)\\))", colnames(mf))

  offcol <- grepl("(offset)", colnames(mf), fixed = TRUE) | grepl("offset(", colnames(mf), fixed = TRUE)
  if (length(offcol) && object_has_names(x, "call") && object_has_names(x$call, "offset")) {
    colnames(mf)[offcol] <- clean_names(safe_deparse(x$call$offset))
  }

  # backtransform variables, such as log, sqrt etc ----------------------------

  mf <- .backtransform(mf, x)

  # clean 1-dimensional matrices ---------------------------------------------

  # in particular, transformation like "scale()" may produce a 1D-matrix,
  # where we want a vector instead
  mf[] <- lapply(mf, function(.x) {
    if (is.matrix(.x) && dim(.x)[2] == 1 && !inherits(.x, c("ns", "bs", "poly", "mSpline"))) {
      as.vector(.x)
    } else {
      .x
    }
  })

  # detect matrix columns ----------------------------------------------------

  # check if we have any matrix columns, e.g. from splines
  mc <- sapply(mf, is.matrix)

  # save original response value and the respective single variable names of
  # the response for later. we don't want to change the response value,
  # if it's a matrix bound with "cbind()"
  rn <- find_response(x, combine = TRUE)
  rn_not_combined <- find_response(x, combine = FALSE)

  # make sure rn is not NULL, but empty string
  if (is.null(rn)) rn <- ""
  if (is.null(rn_not_combined)) rn_not_combined <- ""

  trials.data <- NULL

  # restore original variables used in matrix-response columns ----------------

  if (mc[1] && rn == colnames(mf)[1]) {
    mc[1] <- FALSE
    if (inherits(x, c("coxph", "flexsurvreg", "coxme", "survreg", "survfit", "crq", "psm", "coxr"))) {
      n_of_responses <- ncol(mf[[1]])
      mf <- cbind(as.data.frame(as.matrix(mf[[1]])), mf)
      colnames(mf)[1:n_of_responses] <- rn_not_combined
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

  # process matrix-variables (restore original data from matrix variables) ----

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

    # if data not found in environment,
    # reduce matrix variables into regular vectors
    if (is.null(md)) {

      # we select the non-matrix variables and convert matrix-variables into
      # regular data frames, then binding them together
      mf_matrix <- mf[, which(mc), drop = FALSE]
      mf_nonmatrix <- mf[, -which(mc), drop = FALSE]

      # fix for rms::rcs() functions
      if (any(class(mf_matrix[[1]]) == "rms")) {
        class(mf_matrix[[1]]) <- "matrix"
      }

      # matrix to data frame, bind to model frame
      mf_list <- lapply(mf_matrix, as.data.frame, stringsAsFactors = FALSE)
      mf_matrix <- do.call(cbind, mf_list)
      mf <- cbind(mf_nonmatrix, mf_matrix)
    } else {

      # fix NA in column names
      if (any(is.na(colnames(md)))) {
        colnames(md) <- make.names(colnames(md))
      }

      # get "matrix" terms and "normal" predictors,
      # but exclude response variable(s)
      mf_matrix <- mf[, -which(mc), drop = FALSE]
      spline.term <- clean_names(names(which(mc)))
      other.terms <- clean_names(colnames(mf_matrix))[-1]

      # now we have all variable names that we need
      # from the original data set
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
      if ("(weights)" %in% needed.vars && !object_has_names(md, "(weights)")) {
        needed.vars <- needed.vars[-which(needed.vars == "(weights)")]
        mw <- mf[["(weights)"]]
        fw <- find_weights(x)
        if (!is.null(fw) && fw %in% colnames(md)) {
          needed.vars <- c(needed.vars, fw)
        }
      }

      if (inherits(x, c("coxph", "coxme", "coxr")) || any(grepl("^Surv\\(", spline.term))) {
        # no further processing for survival models
        mf <- md
      } else {

        # get cleaned variable names for those variables
        # that we still need from the original model frame
        needed.vars <- compact_character(unique(clean_names(needed.vars)))
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
        find_predictors(x, effects = effects, flatten = TRUE, verbose = verbose)
      },
      error = function(x) {
        NULL
      }
    )

    # still some undetected matrix-variables?
    if (!is.null(pv) && !all(pv %in% colnames(mf)) && isTRUE(verbose)) {
      warning(format_message("Some model terms could not be found in model data. You probably need to load the data into the environment."), call. = FALSE)
    }
  }

  # monotonic predictors ------------------------------------------------------

  # check if we have monotonic variables, included in formula
  # with "mo()"? If yes, remove from model frame
  mos_eisly <- grepl(pattern = "^mo\\(([^,)]*).*", x = colnames(mf))
  if (any(mos_eisly)) {
    mf <- mf[!mos_eisly]
  }

  # strata-variables in coxph() -----------------------------------------------

  strata_columns <- grepl("^strata\\((.*)\\)", colnames(mf))
  if (any(strata_columns)) {
    for (sc in colnames(mf)[strata_columns]) {
      strata_variable <- gsub("strata\\((.*)\\)", "\\1", sc)
      levels(mf[[sc]]) <- gsub(paste0("\\Q", strata_variable, "=", "\\E"), "", levels(mf[[sc]]))
    }
  }

  # restore original data for factors -----------------------------------------

  # are there any factor variables that have been coerced "on-the-fly",
  # using "factor()" or "as.factor()"? if so, get names and convert back
  # to numeric later
  factors <- colnames(mf)[grepl("^(as\\.factor|as_factor|factor|as\\.ordered|ordered)\\((.*)\\)", colnames(mf))]

  # check for monotonic terms and valid values. In case 'mo()' is used,
  # and predictor is numeric, prettyfied values in the data grid are based
  # on the range of the numeric variable, although only those values are allowed
  # in the data grid that actually appear in the data
  if (inherits(x, "brmsfit")) {
    model_terms <- find_terms(x, flatten = TRUE)
    monotonics <- grepl("mo\\((.*)\\)", model_terms)
    if (any(monotonics)) {
      factors <- union(factors, gsub("mo\\((.*)\\)", "\\1", model_terms[monotonics]))
    }
  }

  # clean variable names
  cvn <- .remove_pattern_from_names(colnames(mf), ignore_lag = TRUE)

  # check for interaction pattern ----------------------------------------

  ints <- grepl("interaction(", colnames(mf), fixed = TRUE)
  # add names of 2nd interaction term
  if (any(ints)) {
    interactions <- stats::setNames(cvn[ints], trim_ws(gsub("interaction\\((.*),(.*)\\)", "\\2", colnames(mf)[ints])))
    factors <- unique(c(factors, names(interactions)))
  } else {
    interactions <- NULL
  }

  # as-is variables I() -------------------------------------------------------

  # keep "as is" variable for response variables in data frame
  if (colnames(mf)[1] == rn[1] && grepl("^I\\(", rn[1])) {
    md <- tryCatch(
      {
        tmp <- .recover_data_from_environment(x)[, unique(c(rn_not_combined, cvn)), drop = FALSE]
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

  # fix duplicated colnames ---------------------------------------------------

  # do we have duplicated names?
  dupes <- which(duplicated(cvn))
  if (!.is_empty_string(dupes)) cvn[dupes] <- sprintf("%s.%s", cvn[dupes], 1:length(dupes))

  colnames(mf) <- cvn

  # add weighting variable ----------------------------------------------------

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

  # add back possible trials-data ---------------------------------------------

  if (!is.null(trials.data)) {
    new.cols <- setdiff(colnames(trials.data), colnames(mf))
    if (!.is_empty_string(new.cols)) mf <- cbind(mf, trials.data[, new.cols, drop = FALSE])
  }

  # remove "trial response"
  # see https://github.com/easystats/modelbased/issues/164
  # if (rn == colnames(mf)[1] && is.matrix(mf[[1]])) {
  #   mf[[1]] <- NULL
  # }

  .add_remaining_missing_variables(
    x,
    mf,
    effects,
    component = "all",
    factors = factors,
    interactions = interactions
  )
}






# add remainng variables with special pattern -------------------------------

.add_remaining_missing_variables <- function(model, mf, effects, component, factors = NULL, interactions = NULL) {

  # check if data argument was used
  model_call <- get_call(model)
  if (!is.null(model_call)) {
    data_arg <- tryCatch(parse(text = safe_deparse(model_call))[[1]]$data,
      error = function(e) NULL
    )
  } else {
    data_arg <- NULL
  }

  # do we have variable names like "mtcars$mpg"?
  if (is.null(data_arg) && all(grepl("(.*)\\$(.*)", colnames(mf)))) {
    colnames(mf) <- gsub("(.*)\\$(.*)", "\\2", colnames(mf))
  }

  predictors <- find_predictors(
    model,
    effects = effects,
    component = component,
    flatten = TRUE,
    verbose = FALSE
  )

  missing_vars <- setdiff(predictors, colnames(mf))

  # check if missing variables can be recovered from the environment,
  # and if so, add to model frame.
  if (!is.null(missing_vars) && length(missing_vars) > 0) {
    env_data <- .recover_data_from_environment(model)
    if (!is.null(env_data) && all(missing_vars %in% colnames(env_data))) {
      shared_columns <- intersect(colnames(env_data), c(missing_vars, colnames(mf)))
      env_data <- stats::na.omit(env_data[shared_columns])
      if (nrow(env_data) == nrow(mf) && !any(missing_vars %in% colnames(mf))) {
        mf <- cbind(mf, env_data[missing_vars])
      }
    }
  }

  # add attributes for those that were factors
  if (length(factors)) {
    factors <- gsub("^(as\\.factor|as_factor|factor|as\\.ordered|ordered)\\((.*)\\)", "\\2", factors)
    for (i in factors) {
      if (.is_numeric_character(mf[[i]])) {
        mf[[i]] <- .to_numeric(mf[[i]])
      }
      if (is.numeric(mf[[i]])) {
        attr(mf[[i]], "factor") <- TRUE
      }
    }
    attr(mf, "factors") <- factors
  }

  # fix interaction terms
  if (!is.null(interactions)) {
    for (i in 1:length(interactions)) {
      int <- interactions[i]
      if (is.factor(mf[[int]])) {
        levels_i2 <- stats::na.omit(unique(mf[[names(int)]]))
        for (j in levels_i2) {
          pattern <- paste0("\\.", levels_i2[j], "$")
          mf[[int]] <- gsub(pattern, "", mf[[int]])
        }
        mf[[int]] <- as.factor(mf[[int]])
      }
    }
  }

  mf
}






# combine data from different model components -------------------------------

# This helper functions ensures that data from different model components
# are included in the returned data frame
#
.return_combined_data <- function(x, mf, effects, component, model.terms, is_mv = FALSE, verbose = TRUE) {
  response <- unlist(model.terms$response)

  # save factors attribute
  factors <- attr(mf, "factors", exact = TRUE)

  if (is_mv) {
    fixed.component.data <- switch(component,
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

    random.component.data <- switch(component,
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
    all_elements <- intersect(names(model.terms), .get_elements("fixed", "all"))
    fixed.component.data <- switch(component,
      all = unlist(model.terms[all_elements]),
      conditional = model.terms$conditional,
      zi = ,
      zero_inflated = model.terms$zero_inflated,
      dispersion = model.terms$dispersion
    )

    random.component.data <- switch(component,
      all = c(model.terms$random, model.terms$zero_inflated_random),
      conditional = model.terms$random,
      zi = ,
      zero_inflated = model.terms$zero_inflated_random
    )
  }


  # this is to remove the "1" from intercept-ony-models

  if (!is_empty_object(fixed.component.data)) {
    fixed.component.data <- .remove_values(fixed.component.data, c("1", "0"))
    fixed.component.data <- .remove_values(fixed.component.data, c(1, 0))
  }
  if (!is_empty_object(random.component.data)) {
    random.component.data <- .remove_values(random.component.data, c("1", "0"))
    random.component.data <- .remove_values(random.component.data, c(1, 0))
  }

  weights <- find_weights(x)
  # if (!is.null(weights) && "(weights)" %in% colnames(mf)) {
  #   weights <- c(weights, "(weights)")
  # }

  vars <- switch(effects,
    all = unique(c(response, fixed.component.data, random.component.data, weights)),
    fixed = unique(c(response, fixed.component.data, weights)),
    random = unique(random.component.data)
  )

  # add offset
  vars <- c(vars, find_offset(x))

  still_missing <- setdiff(vars, colnames(mf))
  vars <- intersect(vars, colnames(mf))
  dat <- mf[, vars, drop = FALSE]

  if (is_empty_object(dat)) {
    if (isTRUE(verbose)) {
      warning(format_message(sprintf("Data frame is empty, probably component '%s' does not exist in the %s-part of the model?", component, effects)), call. = FALSE)
    }
    return(NULL)
  }

  if (length(still_missing) && isTRUE(verbose)) {
    warning(format_message(sprintf("Following potential variables could not be found in the data: %s", paste0(still_missing, collapse = " ,"))), call. = FALSE)
  }

  if ("(offset)" %in% colnames(mf) && !("(offset)" %in% colnames(dat))) {
    dat <- cbind(dat, mf[["(offset"]])
  }

  attr(dat, "factors") <- factors
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
      if (object_has_names(x$call, "subset")) {
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
  vars <- find_variables(x, flatten = TRUE, verbose = FALSE)
  x$data[, vars, drop = FALSE]
}




# combine data from count and zi-component -----------------------------------

.return_zeroinf_data <- function(x, component, verbose = TRUE) {
  model.terms <- find_variables(x, effects = "all", component = "all", flatten = FALSE, verbose = FALSE)
  model.terms$offset <- find_offset(x)

  mf <- tryCatch(
    {
      stats::model.frame(x)
    },
    error = function(x) {
      NULL
    }
  )

  mf <- .prepare_get_data(x, mf, verbose = verbose)
  # add variables from other model components
  mf <- .add_zeroinf_data(x, mf, model.terms$zero_inflated)

  fixed.data <- switch(component,
    all = c(model.terms$conditional, model.terms$zero_inflated, model.terms$offset),
    conditional = c(model.terms$conditional, model.terms$offset),
    zi = ,
    zero_inflated = model.terms$zero_inflated
  )

  mf[, unique(c(model.terms$response, fixed.data, find_weights(x))), drop = FALSE]
}




# "clean" model frame and get data -----------------------------------

# here we have a model frame with many variables, so just extract the important ones...
#
.get_data_from_modelframe <- function(x, dat, effects, verbose = TRUE) {
  if (is_empty_object(dat)) {
    warning("Could not get model data.", call. = FALSE)
    return(NULL)
  }
  cn <- clean_names(colnames(dat))

  ft <- switch(effects,
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

  .prepare_get_data(x, mf, effects, verbose = verbose)
}




# find data from the environment -----------------------------------

# return data from a data frame that is in the environment,
# and subset the data, if necessary
.recover_data_from_environment <- function(x) {
  model_call <- get_call(x)
  # first try, parent frame
  dat <- tryCatch(
    {
      eval(model_call$data, envir = parent.frame())
    },
    error = function(e) {
      NULL
    }
  )

  if (is.null(dat)) {
    # second try, global env
    dat <- tryCatch(
      {
        eval(model_call$data, envir = globalenv())
      },
      error = function(e) {
        NULL
      }
    )
  }


  if (!is.null(dat) && object_has_names(model_call, "subset")) {
    dat <- subset(dat, subset = eval(model_call$subset))
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


  if (!is.null(dat) && object_has_names(x@call, "subset")) {
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
      sv <- eval(parse(text = safe_deparse(x@call))[[1]]$start)
      if (is.list(sv)) sv <- sv[["nlpars"]]
      names(sv)
    },
    error = function(e) {
      NULL
    }
  )
}





# backtransform variables -------------------------------

.backtransform <- function(mf, x) {
  tryCatch(
    {
      patterns <- c(
        "scale\\(log", "exp\\(scale", "log\\(log", "log", "log1p",
        "log10", "log2", "sqrt", "exp", "expm1", "scale", "cos", "sin",
        "tan", "acos", "asin", "atan"
      )
      for (i in patterns) {
        mf <- .backtransform_helper(mf, i, x)
      }
      mf
    },
    error = function(e) {
      mf
    }
  )
}


.backtransform_helper <- function(mf, type, model) {
  cn <- .get_transformed_names(colnames(mf), type)
  if (!.is_empty_string(cn)) {
    for (i in cn) {
      if (type == "scale\\(log") {
        mf[[i]] <- exp(.unscale(mf[[i]]))
      } else if (type == "exp\\(scale") {
        mf[[i]] <- .unscale(log(mf[[i]]))
      } else if (type == "log\\(log") {
        mf[[i]] <- exp(exp(mf[[i]]))
      } else if (type == "log") {
        mf[[i]] <- exp(mf[[i]])
      } else if (type == "log1p") {
        mf[[i]] <- expm1(mf[[i]])
      } else if (type == "log10") {
        mf[[i]] <- 10^(mf[[i]])
      } else if (type == "log2") {
        mf[[i]] <- 2^(mf[[i]])
      } else if (type == "sqrt") {
        mf[[i]] <- (mf[[i]])^2
      } else if (type == "exp") {
        mf[[i]] <- log(mf[[i]])
      } else if (type == "expm1") {
        mf[[i]] <- log1p(mf[[i]])
      } else if (type == "scale") {
        mf[[i]] <- .unscale(mf[[i]])
      } else if (type %in% c("cos", "sin", "tan", "acos", "asin", "atan")) {
        mf[[i]] <- .recover_data_from_environment(model)[[i]]
      }
      colnames(mf)[colnames(mf) == i] <- .get_transformed_terms(i, type)
    }
  }
  mf
}



.unscale <- function(x) {
  m <- attr(x, "scaled:center")
  s <- attr(x, "scaled:scale")

  if (is.null(m)) m <- 0
  if (is.null(s)) s <- 1

  s * x + m
}



# find transformed terms, to convert back as raw data --------------------------------

# Find transformed terms inside model formula, and return "clean" term names
.get_transformed_terms <- function(model, type = "all") {
  if (is.character(model)) {
    x <- model
  } else {
    x <- find_terms(model, flatten = TRUE)
  }
  pattern <- sprintf("%s\\(([^,\\+)]*).*", type)
  trim_ws(gsub(pattern, "\\1", x[grepl(pattern, x)]))
}


# get column names of transformed terms
.get_transformed_names <- function(x, type = "all") {
  pattern <- sprintf("%s\\(([^,)]*).*", type)
  x[grepl(pattern, x)]
}


.retrieve_htest_data <- function(x) {
  out <- tryCatch(
    {
      # special handling of survey-objects
      if (grepl("^svy", x$data.name)) {
        if (grepl("pearson's x^2", tolower(x$method), fixed = TRUE)) {
          d <- x$observed
        } else {
          d <- NULL
        }
      } else {
        # split by "and" and "by". E.g., for t.test(1:3, c(1,1:3)), we have
        # x$data.name = "1:3 and c(1, 1:3)"
        data_name <- trim_ws(unlist(strsplit(x$data.name, "(and|by)")))

        # now we may have exceptions, e.g. for friedman.test(wb$x, wb$w, wb$t)
        # x$data.name is "wb$x, wb$w and wb$t" and we now have "wb$x, wb$w" and
        # "wb$t", so we need to split at comma as well. However, the above t-test
        # example returns "1:3" and "c(1, 1:3)", so we only must split at comma
        # when it is not inside parentheses.
        data_comma <- unlist(strsplit(data_name, "(\\([^)]*\\))"))

        # any comma not inside parentheses?
        if (any(grepl(",", data_comma, fixed = TRUE))) {
          data_name <- trim_ws(unlist(strsplit(data_comma, ", ", fixed = TRUE)))
        }

        # exeception: list for kruskal-wallis
        if (grepl("Kruskal-Wallis", x$method, fixed = TRUE) && grepl("^list\\(", data_name)) {
          l <- eval(.str2lang(x$data.name))
          names(l) <- paste0("x", 1:length(l))
          return(l)
        }

        data_call <- lapply(data_name, .str2lang)
        columns <- lapply(data_call, eval)

        # preserve table data for McNemar
        if (!grepl(" (and|by) ", x$data.name) && (grepl("^McNemar", x$method) || (length(columns) == 1 && is.matrix(columns[[1]])))) {
          return(as.table(columns[[1]]))
          # check if data is a list for kruskal-wallis
        } else if (grepl("^Kruskal-Wallis", x$method) && length(columns) == 1 && is.list(columns[[1]])) {
          l <- columns[[1]]
          names(l) <- paste0("x", 1:length(l))
          return(l)
        } else {
          max_len <- max(sapply(columns, length))
          for (i in 1:length(columns)) {
            if (length(columns[[i]]) < max_len) {
              columns[[i]] <- c(columns[[i]], rep(NA, max_len - length(columns[[i]])))
            }
          }
          d <- as.data.frame(columns)
        }

        if (all(grepl("(.*)\\$(.*)", data_name)) && length(data_name) == length(colnames(d))) {
          colnames(d) <- gsub("(.*)\\$(.*)", "\\2", data_name)
        } else if (ncol(d) > 2) {
          colnames(d) <- paste0("x", 1:ncol(d))
        } else if (ncol(d) == 2) {
          colnames(d) <- c("x", "y")
        } else {
          colnames(d) <- "x"
        }
      }

      d
    },
    error = function(e) {
      NULL
    }
  )

  # 2nd try
  if (is.null(out)) {
    for (parent_level in 1:5) {
      out <- tryCatch(
        {
          data_name <- trim_ws(unlist(strsplit(x$data.name, "(and|,|by)")))
          as.table(get(data_name, envir = parent.frame(n = parent_level)))
        },
        error = function(e) {
          NULL
        }
      )
      if (!is.null(out)) break
    }
  }

  out
}
