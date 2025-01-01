# Function that does the most work for preparing and transforming the data,
# to ensure we have a "clean" data frame from the data that was used to fit
# the model. This also means that, unless necessary for further processing,
# variables transformed during model fitting are not included in this data frame
.prepare_get_data <- function(x, mf, effects = "fixed", verbose = TRUE) {
  # check if we have any data yet
  if (is_empty_object(mf)) {
    if (isTRUE(verbose)) {
      format_warning("Could not get model data.")
    }
    return(NULL)
  }

  # we may store model weights here later
  mw <- NULL


  # make sure it's a data frame -----------------------------------------------

  if (!is.data.frame(mf)) {
    mf <- .safe(as.data.frame(mf))
    if (is.null(mf)) {
      if (isTRUE(verbose)) {
        format_warning(
          "Cannot coerce data into a data frame.",
          "No data will be returned."
        )
      }
      return(NULL)
    }
  }


  # offset variables ----------------------------------------------------------

  # do we have an offset, not specified in the formula?

  # This is a bit slower - restore if tests fail...
  # offcol <- grep("^(\\(offset\\)|offset\\((.*)\\))", colnames(mf))

  offcol <- grepl("(offset)", colnames(mf), fixed = TRUE) | grepl("offset(", colnames(mf), fixed = TRUE)
  model_call <- get_call(x)
  if (length(offcol) && object_has_names(model_call, "offset")) {
    colnames(mf)[offcol] <- clean_names(safe_deparse(model_call$offset))
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
  mc <- vapply(mf, is.matrix, TRUE)

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
      colnames(mf)[1:n_of_responses] <- rn_not_combined[1:n_of_responses]
    } else {
      tryCatch(
        {
          trials.data <- as.data.frame(mf[[1]])
          colnames(trials.data) <- rn_not_combined

          # if columns were bound via subtraction, e.g.
          # "cbind(success, total - success)", we need to sum up success and
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
    md <- .safe(eval(model_call$data, environment(stats::formula(x))))

    # in case we have missing data, the data frame in the environment
    # has more rows than the model frame
    if (isTRUE(!is.null(md) && nrow(md) != nrow(mf))) {
      md <- .safe({
        md <- .recover_data_from_environment(x)
        md <- stats::na.omit(md[intersect(
          colnames(md),
          find_variables(x, effects = "all", component = "all", flatten = TRUE)
        )])
      })
    }

    # if data not found in environment,
    # reduce matrix variables into regular vectors
    if (is.null(md)) {
      # we select the non-matrix variables and convert matrix-variables into
      # regular data frames, then binding them together
      mf_matrix <- mf[, which(mc), drop = FALSE]
      mf_nonmatrix <- mf[, -which(mc), drop = FALSE]

      # fix for rms::rcs() functions
      if (any(inherits(mf_matrix[[1]], "rms"))) {
        class(mf_matrix[[1]]) <- "matrix"
      }

      # matrix to data frame, bind to model frame
      mf_list <- lapply(mf_matrix, as.data.frame, stringsAsFactors = FALSE)
      mf_matrix <- do.call(cbind, mf_list)
      mf <- cbind(mf_nonmatrix, mf_matrix)
    } else {
      # fix NA in column names
      if (anyNA(colnames(md))) {
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
        if (!is.null(fw) && all(fw %in% colnames(md))) {
          needed.vars <- c(needed.vars, fw)
        }
      }

      if (inherits(x, c("coxph", "coxme", "coxr")) || any(startsWith(spline.term, "Surv("))) {
        # no further processing for survival models
        mf <- md
      } else {
        # get cleaned variable names for those variables
        # that we still need from the original model frame
        needed.vars <- compact_character(unique(clean_names(needed.vars)))

        # check if data in environment was modified in between?
        if (!all(needed.vars %in% colnames(md))) {
          needed.vars <- intersect(needed.vars, colnames(md))
        }

        # if data recovered from environment does not match current
        # model frame, tell user and skip this step
        if (!length(needed.vars) || nrow(md) != nrow(mf)) {
          if (isTRUE(verbose)) {
            format_warning(
              "Could not find all model variables in the data.",
              "Maybe the original data frame used to fit the model was modified?"
            )
          }
        } else {
          mf <- md[, needed.vars, drop = FALSE]

          # we need this hack to save variable and value label attributes, if any
          value_labels <- lapply(mf, attr, which = "labels", exact = TRUE)
          variable_labels <- lapply(mf, attr, which = "label", exact = TRUE)

          # removing NAs drops all label-attributes
          mf <- stats::na.omit(mf)

          # then set back attributes
          mf <- as.data.frame(Map(function(.d, .l) {
            attr(.d, "labels") <- .l
            .d
          }, mf, value_labels), stringsAsFactors = FALSE)

          mf <- as.data.frame(Map(function(.d, .l) {
            attr(.d, "label") <- .l
            .d
          }, mf, variable_labels), stringsAsFactors = FALSE)
        }
      }

      # add back model weights, if any
      if (!is.null(mw)) mf$`(weights)` <- mw
    }

    # check if we really have all formula terms in our model frame now
    pv <- .safe(find_predictors(x, effects = effects, flatten = TRUE, verbose = verbose))

    # still some undetected matrix-variables?
    if (!is.null(pv) && !all(pv %in% colnames(mf)) && isTRUE(verbose)) {
      format_warning(
        "Some model terms could not be found in model data.",
        "You probably need to load the data into the environment."
      )
    }
  }


  # monotonic predictors ------------------------------------------------------

  # check if we have monotonic variables, included in formula
  # with "mo()"? If yes, remove from model frame
  if (inherits(x, "brmsfit")) {
    mos_eisly <- grepl("^mo\\(([^,)]*).*", colnames(mf))
    if (any(mos_eisly)) {
      mf <- mf[!mos_eisly]
    }
  }


  # strata-variables in coxph() -----------------------------------------------

  strata_columns <- grepl("strata(", colnames(mf), fixed = TRUE)
  if (any(strata_columns)) {
    for (sc in colnames(mf)[strata_columns]) {
      strata_variable <- gsub("strata\\((.*)\\)", "\\1", sc)
      levels(mf[[sc]]) <- gsub(paste0(strata_variable, "="), "", levels(mf[[sc]]), fixed = TRUE)
    }
  }


  # restore original data for factors and logicals -----------------------------

  # are there any factor variables that have been coerced "on-the-fly",
  # using "factor()" or "as.factor()"? if so, get names and convert back
  # to numeric later
  factors <- colnames(mf)[grepl("^(as\\.factor|as_factor|factor|as\\.ordered|ordered)\\((.*)\\)", colnames(mf))]
  logicals <- colnames(mf)[grepl("^(as\\.logical|as_logical|logical)\\((.*)\\)", colnames(mf))]

  ## TODO check! some lines above (see "mos_eisly"). monotonic terms are removed from the model frame

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
  interactions <- NULL
  # add names of 2nd interaction term
  if (any(ints)) {
    if (inherits(x, "brmsfit")) {
      # brms saves original variables, so remove interaction column here
      mf[ints] <- NULL
      cvn <- cvn[!ints]
    } else {
      interactions <- stats::setNames(cvn[ints], trim_ws(gsub("interaction\\((.*),(.*)\\)", "\\2", colnames(mf)[ints])))
      factors <- unique(c(factors, interactions, names(interactions)))
    }
  }


  # as-is variables I() -------------------------------------------------------

  # keep "as is" variable for response variables in data frame
  if (colnames(mf)[1] == rn[1] && grepl("I(", rn[1], fixed = TRUE, ignore.case = FALSE)) {
    md <- .safe({
      tmp <- .recover_data_from_environment(x)[, unique(c(rn_not_combined, cvn)), drop = FALSE]
      tmp[, rn_not_combined, drop = FALSE]
    })

    if (!is.null(md)) {
      mf <- cbind(mf, md)
      cvn <- .remove_pattern_from_names(colnames(mf), ignore_lag = TRUE)
      cvn[1] <- rn[1]
    }
  }


  # fix duplicated colnames ---------------------------------------------------

  # do we have duplicated names?
  dupes <- which(duplicated(cvn))
  if (!.is_empty_string(dupes)) {
    cvn[dupes] <- sprintf("%s.%s", cvn[dupes], seq_along(dupes))
  }

  colnames(mf) <- cvn


  # add weighting variable ----------------------------------------------------

  weighting_var <- find_weights(x)
  if (!is.null(weighting_var) && !all(weighting_var %in% colnames(mf))) {
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
    logicals = logicals,
    interactions = interactions,
    verbose = verbose
  )
}


# add remainng variables with special pattern -------------------------------

.add_remaining_missing_variables <- function(model,
                                             mf,
                                             effects,
                                             component,
                                             factors = NULL,
                                             logicals = NULL,
                                             interactions = NULL,
                                             verbose = TRUE) {
  # check if data argument was used
  model_call <- get_call(model)
  if (is.null(model_call)) {
    data_arg <- NULL
  } else {
    data_arg <- .safe(parse(text = safe_deparse(model_call))[[1]]$data)
  }

  # do we have variable names like "mtcars$mpg"?
  if (is.null(data_arg) && all(grepl("$", colnames(mf), fixed = TRUE))) {
    colnames(mf) <- gsub("(.*)\\$(.*)", "\\2", colnames(mf))
  }

  predictors <- find_predictors(
    model,
    effects = effects,
    component = component,
    flatten = TRUE,
    verbose = FALSE
  )

  # include subset variables
  subset_vars <- .safe(all.vars(model_call$subset))
  missing_vars <- unique(c(setdiff(predictors, colnames(mf)), subset_vars))

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

  # fix interaction terms
  if (!is.null(interactions)) {
    full_data <- .safe(.recover_data_from_environment(model))
    if (!is.null(full_data) && nrow(full_data) == nrow(mf)) {
      mf[c(interactions, names(interactions))] <- NULL
      mf <- cbind(mf, full_data[c(interactions, names(interactions))])
    } else {
      for (i in seq_along(interactions)) {
        int <- interactions[i]
        mf[[names(int)]] <- as.factor(substr(
          as.character(mf[[int]]),
          regexpr("\\.[^\\.]*$", as.character(mf[[int]])) + 1,
          nchar(as.character(mf[[int]]))
        ))
        mf[[int]] <- as.factor(substr(
          as.character(mf[[int]]),
          0,
          regexpr("\\.[^\\.]*$", as.character(mf[[int]])) - 1
        ))
      }
      if (isTRUE(verbose)) {
        format_warning(
          "The data contains variables used 'interaction()'-functions. These are probably not recovered accurately in the returned data frame.",
          "Please check the data frame carefully."
        )
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

  # add attributes for those that were logicals
  if (length(logicals)) {
    logicals <- gsub("^(as\\.logical|as_logical|logical)\\((.*)\\)", "\\2", logicals)
    for (i in logicals) {
      mf[[i]] <- as.numeric(mf[[i]])
      attr(mf[[i]], "logical") <- TRUE
    }
    attr(mf, "logicals") <- logicals
  }

  # add attribute that subset is used
  attr(mf, "is_subset") <- !is.null(subset_vars) && length(subset_vars)

  mf
}


# combine data from different model components -------------------------------

# This helper functions ensures that data from different model components
# are included in the returned data frame
#
.return_combined_data <- function(x, mf, effects, component, model.terms, is_mv = FALSE, verbose = TRUE) {
  response <- unlist(model.terms$response, use.names = FALSE)

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

    fixed.component.data <- unlist(fixed.component.data, use.names = FALSE)
    random.component.data <- unlist(random.component.data, use.names = FALSE)
  } else {
    all_elements <- intersect(names(model.terms), .get_elements("fixed", "all"))
    fixed.component.data <- switch(component,
      all = unlist(model.terms[all_elements], use.names = FALSE),
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

  model_weights <- find_weights(x)
  # if (!is.null(model_weights) && "(weights)" %in% colnames(mf)) {
  #   model_weights <- c(model_weights, "(weights)")
  # }

  vars <- switch(effects,
    all = unique(c(response, fixed.component.data, random.component.data, model_weights)),
    fixed = unique(c(response, fixed.component.data, model_weights)),
    random = unique(random.component.data)
  )

  # add offset
  vars <- c(vars, find_offset(x))

  still_missing <- setdiff(vars, colnames(mf))
  vars <- intersect(vars, colnames(mf))
  dat <- mf[, vars, drop = FALSE]

  if (is_empty_object(dat)) {
    if (isTRUE(verbose)) {
      format_warning(
        sprintf("Data frame is empty, probably component `%s` does not exist in the %s-part of the model?", component, effects)
      )
    }
    return(NULL)
  }

  if (length(still_missing) && isTRUE(verbose)) {
    format_warning(
      sprintf("Following potential variables could not be found in the data: %s", toString(still_missing))
    )
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
      env_data <- .recover_data_from_environment(x)[, tn, drop = FALSE]
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

  mf <- .safe(stats::model.frame(x))
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
    format_warning("Could not get model data.")
    return(NULL)
  }
  cn <- clean_names(colnames(dat))

  ft <- switch(effects,
    fixed = find_variables(x, effects = "fixed", flatten = TRUE),
    all = find_variables(x, flatten = TRUE),
    random = find_random(x, split_nested = TRUE, flatten = TRUE)
  )
  remain <- intersect(c(ft, find_weights(x), find_offset(x)), cn)

  mf <- .safe(dat[, remain, drop = FALSE], dat)
  .prepare_get_data(x, mf, effects, verbose = verbose)
}


# find start vector of nlmer-models from the environment -----------------------------------

# return data from a data frame that is in the environment,
# and subset the data, if necessary
.get_startvector_from_env <- function(x) {
  .safe({
    sv <- eval(parse(text = safe_deparse(x@call))[[1]]$start)
    if (is.list(sv)) sv <- sv[["nlpars"]]
    names(sv)
  })
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

      # to save time, create a full regex and see if we have any
      # transformation at all, instead of always checking for each pattern
      full_pattern <- sprintf("%s\\(([^,)]*).*", paste0("(", paste0(patterns, collapse = "|"), ")"))

      # only backtransform if we have any matches
      if (any(grepl(full_pattern, colnames(mf)))) {
        for (i in patterns) {
          mf <- .backtransform_helper(mf, i, x)
        }
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
      if (type == "scale\\(log") { # nolint
        mf[[i]] <- exp(.unscale(mf[[i]]))
      } else if (type == "exp\\(scale") {
        mf[[i]] <- .unscale(log(mf[[i]]))
      } else if (type == "log\\(log") {
        mf[[i]] <- exp(exp(mf[[i]]))
      } else if (type == "log") {
        # exceptions: log(x+1) or log(1+x)
        plus_minus <- NULL
        # no plus-minus?
        if (grepl("log\\((.*)\\+(.*)\\)", i)) {
          # 1. try: log(x + number)
          plus_minus <- .safe(eval(parse(text = gsub("log\\(([^,\\+)]+)(.*)\\)", "\\2", i))))
          # 2. try: log(number + x)
          if (is.null(plus_minus)) {
            plus_minus <- .safe(eval(parse(text = gsub("log\\(([^,\\+)]+)(.*)\\)", "\\1", i))))
          }
        }
        if (is.null(plus_minus)) {
          mf[[i]] <- exp(mf[[i]])
        } else {
          mf[[i]] <- exp(mf[[i]]) - plus_minus
        }
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
  out <- trim_ws(gsub(pattern, "\\1", grep(pattern, x, value = TRUE)))
  # validation check - when we have something like "log(1+x)" instead "log(x+1)",
  # the regex pattern returns "1" instead of "x3"
  if (!is.na(suppressWarnings(as.numeric(out)))) {
    out <- trim_ws(gsub(pattern, "\\2", grep(pattern, x, value = TRUE)))
  }
  out
}


# get column names of transformed terms
.get_transformed_names <- function(x, type = "all") {
  pattern <- sprintf("%s\\(([^,)]*).*", type)
  grep(pattern, x, value = TRUE)
}


.retrieve_htest_data <- function(x) {
  out <- tryCatch(
    {
      # special handling of survey-objects
      if (startsWith(x$data.name, "svy")) {
        if (grepl("pearson's x^2", tolower(x$method), fixed = TRUE)) {
          d <- x$observed
        } else {
          d <- NULL
        }
      } else {
        # split by "and" and "by". E.g., for t.test(1:3, c(1,1:3)), we have
        # x$data.name = "1:3 and c(1, 1:3)"
        data_name <- trim_ws(unlist(strsplit(x$data.name, "(and|by)"), use.names = FALSE))

        # now we may have exceptions, e.g. for friedman.test(wb$x, wb$w, wb$t)
        # x$data.name is "wb$x, wb$w and wb$t" and we now have "wb$x, wb$w" and
        # "wb$t", so we need to split at comma as well. However, the above t-test
        # example returns "1:3" and "c(1, 1:3)", so we only must split at comma
        # when it is not inside parentheses.
        data_comma <- unlist(strsplit(data_name, "(\\([^)]*\\))"), use.names = FALSE)

        # any comma not inside parentheses?
        if (any(grepl(",", data_comma, fixed = TRUE))) {
          data_name <- trim_ws(unlist(strsplit(data_comma, ", ", fixed = TRUE)))
        }

        # exeception: list for kruskal-wallis
        if (grepl("Kruskal-Wallis", x$method, fixed = TRUE) &&
          (length(data_name) == 1 && startsWith(data_name, "list("))) {
          l <- eval(str2lang(x$data.name))
          names(l) <- paste0("x", seq_along(l))
          return(l)
        }

        data_call <- lapply(data_name, str2lang)
        columns <- lapply(data_call, eval)

        # detect which kind of tests we have -----------------

        # McNemar ============================================================

        if (!grepl(" (and|by) ", x$data.name) && !grepl(x$method, "Paired t-test", fixed = TRUE) &&
          !startsWith(x$method, "Wilcoxon") &&
          (startsWith(x$method, "McNemar") || (length(columns) == 1 && is.matrix(columns[[1]])))) {
          # McNemar: preserve table data for McNemar ----
          return(as.table(columns[[1]]))
        } else if (startsWith(x$method, "Kruskal-Wallis") && length(columns) == 1 && is.list(columns[[1]])) {
          # Kruskal-Wallis: check if data is a list for kruskal-wallis ----
          l <- columns[[1]]
          names(l) <- paste0("x", seq_along(l))
          return(l)
        } else if (grepl("t-test", x$method, fixed = TRUE)) {
          # t-Test: (Welch) Two Sample t-test ----
          if (grepl("Two", x$method, fixed = TRUE)) {
            if (grepl(" and ", x$data.name, fixed = TRUE)) {
              return(.htest_reshape_long(columns))
            } else if (grepl(" by ", x$data.name, fixed = TRUE)) {
              return(.htest_no_reshape(columns))
            }

            # t-Test: Paired t-test
          } else if (startsWith(x$method, "Paired")) {
            if (grepl(" and ", x$data.name, fixed = TRUE)) {
              # t-Test: Paired t-test, two vectors ----
              return(.htest_reshape_long(columns))
            } else if (grepl(" by ", x$data.name, fixed = TRUE)) {
              # t-Test: Paired t-test, formula (no reshape required) ----
              return(.htest_no_reshape(columns))
            } else if (startsWith(x$data.name, "Pair(")) {
              # t-Test: Paired t-test ----
              return(.htest_reshape_matrix(columns))
            }

            # t-Test: One Sample
          } else {
            d <- .htest_other_format(columns)
          }
        } else if (startsWith(x$method, "Wilcoxon rank sum")) {
          # Wilcoxon ========================================================
          if (grepl(" by ", x$data.name, fixed = TRUE)) {
            # Wilcoxon: Paired Wilcoxon, formula (no reshape required) ----
            return(.htest_no_reshape(columns))
          } else {
            return(.htest_reshape_long(columns))
          }
        } else if (startsWith(x$method, "Wilcoxon signed rank")) {
          if (startsWith(x$data.name, "Pair(")) {
            return(.htest_reshape_matrix(columns))
          } else if (grepl(" and ", x$data.name, fixed = TRUE)) {
            # Wilcoxon: Paired Wilcoxon, two vectors ----
            return(.htest_reshape_long(columns))
          } else if (grepl(" by ", x$data.name, fixed = TRUE)) {
            # Wilcoxon: Paired Wilcoxon, formula (no reshape required) ----
            return(.htest_no_reshape(columns))
          } else {
            # Wilcoxon: One sample ----
            d <- .htest_other_format(columns)
          }
        } else {
          # Other htests ======================================================
          d <- .htest_other_format(columns)
        }

        # fix column names
        if (all(grepl("(.*)\\$(.*)", data_name)) && length(data_name) == length(colnames(d))) {
          colnames(d) <- gsub("(.*)\\$(.*)", "\\2", data_name)
        } else if (ncol(d) > 2L) {
          colnames(d) <- paste0("x", seq_len(ncol(d)))
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
      out <- .safe({
        data_name <- trim_ws(unlist(strsplit(x$data.name, "(and|,|by)"), use.names = FALSE))
        as.table(get(data_name, envir = parent.frame(n = parent_level)))
      })
      if (!is.null(out)) break
    }
  }

  out
}


# reshape helpers -------------------

.htest_reshape_long <- function(columns) {
  data.frame(
    x = unlist(columns),
    y = c(
      rep("1", length(columns[[1]])),
      rep("2", length(columns[[2]]))
    ),
    stringsAsFactors = TRUE
  )
}

.htest_no_reshape <- function(columns) {
  data.frame(
    x = columns[[1]],
    y = as.factor(columns[[2]])
  )
}


.htest_reshape_matrix <- function(columns) {
  data.frame(
    x = c(columns[[1]][, 1, drop = TRUE], columns[[1]][, 2, drop = TRUE]),
    y = c(
      rep("1", nrow(columns[[1]])),
      rep("2", nrow(columns[[1]]))
    ),
    stringsAsFactors = TRUE
  )
}


.htest_other_format <- function(columns) {
  max_len <- max(lengths(columns))
  for (i in seq_along(columns)) {
    if (length(columns[[i]]) < max_len) {
      columns[[i]] <- c(columns[[i]], rep(NA, max_len - length(columns[[i]])))
    }
  }
  as.data.frame(columns)
}
