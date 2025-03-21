#' @title Compute intercept-only model for regression models
#' @name null_model
#'
#' @description This function computes the null-model (i.e. `(y ~ 1)`) of
#'   a model. For mixed models, the null-model takes random effects into account.
#'
#' @param model A (mixed effects) model.
#' @param verbose Toggle off warnings.
#' @param ... Arguments passed to or from other methods.
#'
#' @return The null-model of `x`
#'
#' @examplesIf require("lme4", quietly = TRUE)
#' data(sleepstudy)
#' m <- lmer(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)
#' summary(m)
#' summary(null_model(m))
#'
#' @export
null_model <- function(model, ...) {
  UseMethod("null_model")
}


#' @rdname null_model
#' @export
null_model.default <- function(model, verbose = TRUE, ...) {
  # sanity check, if we missed adding a method. rstanarm and brms can both
  # have mixed and non-mixed models, these are captured here as well
  if (is_mixed_model(model)) {
    return(null_model.glmmTMB(model, verbose, ...))
  }

  model_formula <- find_formula(model, verbose = verbose)
  offset_term <- .grep_offset_term(model_formula)

  # get model data and variables
  model_data <- get_data(model, verbose = FALSE)
  update_data <- .prepare_update_data(model, model_data)

  # base arguments for call to `update()`
  base_args <- list(model, ~1, evaluate = FALSE)

  # add data, if any, and select appropriate environment for "eval()"
  if (is.null(update_data)) {
    env <- parent.frame()
  } else {
    base_args$data <- update_data
    env <- NULL
  }

  # add offset, if any
  if (!is.null(offset_term)) {
    base_args$offset <- str2lang(offset_term)
  }

  out <- tryCatch(
    suppressWarnings(do.call(stats::update, base_args)),
    error = function(e) {
      if (verbose && !is.null(offset_term)) {
        format_warning(
          "Model contains offset-terms, which could not be considered in the returned null-model.",
          "Coefficients might be inaccurate."
        )
      }
      base_args$offset <- NULL
      suppressWarnings(do.call(stats::update, base_args))
    }
  )

  suppressWarnings(eval(out, envir = env))
}


#' @export
null_model.multinom <- function(model, verbose = TRUE, ...) {
  model_formula <- find_formula(model, verbose = verbose)
  offset_term <- .grep_offset_term(model_formula)

  # get model data and variables
  model_data <- get_data(model, verbose = FALSE)
  update_data <- .prepare_update_data(model, model_data)

  # base arguments for call to `update()`
  base_args <- list(model, ~1, evaluate = FALSE, trace = FALSE)

  # add data, if any, and select appropriate environment for "eval()"
  if (is.null(update_data)) {
    env <- parent.frame()
  } else {
    base_args$data <- update_data
    env <- NULL
  }

  out <- suppressWarnings(do.call(stats::update, base_args))
  suppressWarnings(eval(out, envir = env))
}


#' @export
null_model.clm2 <- function(model, verbose = TRUE, ...) {
  model_formula <- find_formula(model, verbose = verbose)
  offset_term <- .grep_offset_term(model_formula)

  # get model data and variables
  model_data <- get_data(model, verbose = FALSE)
  update_data <- .prepare_update_data(model, model_data)

  # base arguments for call to `update()`
  base_args <- list(model, ~1, evaluate = FALSE, location = ~1, scale = ~1)

  # add data, if any, and select appropriate environment for "eval()"
  if (is.null(update_data)) {
    env <- parent.frame()
  } else {
    base_args$data <- update_data
    env <- NULL
  }

  out <- suppressWarnings(do.call(stats::update, base_args))
  suppressWarnings(eval(out, envir = env))
}


#' @export
null_model.MixMod <- function(model, verbose = TRUE, ...) {
  model_formula <- find_formula(model, verbose = verbose)
  offset_term <- .grep_offset_term(model_formula)

  # get model data and variables
  model_data <- get_data(model, verbose = FALSE)
  update_data <- .prepare_update_data(model, model_data)

  nullform <- stats::as.formula(paste(find_response(model), "~ 1"))

  # base arguments for call to `update()`
  base_args <- list(model, fixed = nullform, evaluate = FALSE)

  # add data, if any, and select appropriate environment for "eval()"
  if (is.null(update_data)) {
    env <- parent.frame()
  } else {
    base_args$data <- update_data
    env <- NULL
  }

  out <- suppressWarnings(do.call(stats::update, base_args))
  null.model <- suppressWarnings(eval(out, envir = env))
  # fix fixed effects formula
  null.model$call$fixed <- nullform

  null.model
}


#' @export
null_model.cpglmm <- function(model, verbose = TRUE, ...) {
  model_formula <- find_formula(model, verbose = verbose)
  offset_term <- .grep_offset_term(model_formula)

  # get model data and variables
  model_data <- get_data(model, verbose = FALSE)
  update_data <- .prepare_update_data(model, model_data)

  nullform <- model_formula[["random"]]

  # base arguments for call to `update()`
  base_args <- list(model, nullform, evaluate = FALSE)

  # add data, if any, and select appropriate environment for "eval()"
  if (is.null(update_data)) {
    env <- parent.frame()
  } else {
    base_args$data <- update_data
    env <- NULL
  }

  out <- suppressWarnings(do.call(stats::update, base_args))
  suppressWarnings(eval(out, envir = env))
}


#' @export
null_model.glmmTMB <- function(model, verbose = TRUE, ...) {
  # sanity check, some moodels of class glmmTMB may not be mixed models
  if (!is_mixed_model(model)) {
    return(null_model.default(model, verbose, ...))
  }

  model_formula <- find_formula(model, verbose = verbose)
  offset_term <- .grep_offset_term(model_formula)

  # get model data and variables
  model_data <- get_data(model, verbose = FALSE)
  update_data <- .prepare_update_data(model, model_data)

  if (inherits(model, "glmmTMB") && !is.null(model_formula$zero_inflated)) {
    insight::check_if_installed("glmmTMB")
    # for zero-inflated models, we need to create the NULL model for the
    # zero-inflation part as well. Since "update()" won't work here, we need
    # to extract all elements from the call and modify the formulas there
    model_args <- lapply(get_call(model), safe_deparse)[-1]
    formula_args <- endsWith(names(model_args), "formula")
    resp <- find_response(model)
    model_args[formula_args] <- lapply(names(model_args[formula_args]), function(f_names) {
      f <- model_args[[f_names]]
      re_string <- sapply(.findbars(stats::as.formula(f)), safe_deparse)
      if (is_empty_object(re_string)) {
        stats::as.formula("~1")
      } else if (any(startsWith(f_names, c("zi", "disp")))) {
        stats::reformulate(paste0("(", re_string, ")"), response = NULL)
      } else {
        stats::reformulate(paste0("(", re_string, ")"), response = resp)
      }
    })
    model_args[!formula_args] <- lapply(model_args[!formula_args], str2lang)
    # add offset back
    if (!is.null(offset_term)) {
      model_args$offset <- str2lang(offset_term)
    }
    # add corrected data from model frame
    if (!is.null(update_data)) {
      model_args$data <- update_data
    }
    null.model <- do.call(glmmTMB::glmmTMB, model_args)
  } else {
    f <- stats::formula(model)
    resp <- find_response(model)
    # fix for brms models
    if (inherits(model, "brmsfit")) {
      f <- f$formula
    }
    re.terms <- paste0("(", sapply(.findbars(f), safe_deparse), ")")
    nullform <- stats::reformulate(re.terms, response = resp)

    null.model <- tryCatch(
      {
        fun_args <- list(model, formula = nullform, evaluate = FALSE)
        if (!is.null(offset_term)) {
          fun_args$offset <- str2lang(offset_term)
        }
        if (is.null(update_data)) {
          env <- parent.frame()
        } else {
          fun_args$data <- update_data
          env <- NULL
        }
        out <- suppressWarnings(do.call(stats::update, fun_args))
        suppressWarnings(eval(out, envir = env))
      },
      error = function(e) {
        msg <- e$message
        if (verbose) {
          if (grepl("(^object)(.*)(not found$)", msg)) {
            print_color("Can't calculate null-model. Probably the data that was used to fit the model cannot be found.\n", "red") # nolint
          } else if (startsWith(msg, "could not find function")) {
            print_color("Can't calculate null-model. Probably you need to load the package that was used to fit the model.\n", "red") # nolint
          }
        }
        NULL
      }
    )
  }

  null.model
}

#' @export
null_model.merMod <- null_model.glmmTMB

#' @export
null_model.lmerMod <- null_model.glmmTMB

#' @export
null_model.glmerMod <- null_model.glmmTMB

#' @export
null_model.rlmerMod <- null_model.glmmTMB

#' @export
null_model.nlmerMod <- null_model.glmmTMB

#' @export
null_model.cpglmm <- null_model.glmmTMB

#' @export
null_model.mixed <- null_model.glmmTMB

#' @export
null_model.coxme <- null_model.glmmTMB

#' @export
null_model.glmmadmb <- null_model.glmmTMB


# helper -------------------------------


.grep_offset_term <- function(model_formula) {
  tryCatch(
    {
      f <- safe_deparse(model_formula$conditional)
      if (grepl("offset(", f, fixed = TRUE)) {
        out <- gsub("(.*)offset\\((.*)\\)(.*)", "\\2", f)
      } else {
        out <- NULL
      }
      out
    },
    error = function(e) {
      NULL
    }
  )
}


.prepare_update_data <- function(model, model_data) {
  tryCatch(
    {
      model_vars <- find_variables(
        model,
        effects = "all",
        component = "all",
        flatten = TRUE,
        verbose = FALSE
      )
      # offset?
      model_vars <- c(model_vars, find_offset(model))
      # weights?
      model_vars <- c(model_vars, find_weights(model))
      # columns in model and data - we need to pass the filtered data set
      cols <- intersect(model_vars, colnames(model_data))
      model_data[stats::complete.cases(model_data[cols]), cols, drop = FALSE]
    },
    error = function(e) {
      NULL
    }
  )
}
