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
null_model <- function(model, verbose = TRUE, ...) {
  model_formula <- find_formula(model, verbose = verbose)
  offset_term <- tryCatch(
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

  # get model data and variables
  model_data <- get_data(model)
  model_vars <- find_variables(model, effects = "all", component = "all", flatten = TRUE)

  # columns in model and data - we need to pass the filtered data set
  cols <- intersect(model_vars, colnames(model_data))
  update_data <- model_data[stats::complete.cases(model_data[cols]), cols, drop = FALSE]

  if (is_mixed_model(model)) {
    .null_model_mixed(model, offset_term, model_formula, update_data, verbose)
  } else if (inherits(model, "clm2")) {
    out <- stats::update(
      model,
      location = ~1,
      scale = ~1,
      data = update_data,
      evaluate = FALSE
    )
  } else if (inherits(model, "multinom")) {
    out <- stats::update(model, ~1, trace = FALSE, data = update_data, evaluate = FALSE)
  } else if (is.null(offset_term)) {
    # stats::update(model, ~1)
    out <- stats::update(model, ~1, evaluate = FALSE, data = update_data)
  } else {
    tryCatch(
      out <- do.call(
        stats::update,
        list(
          model,
          ~1,
          offset = str2lang(offset_term),
          data = update_data,
          evaluate = FALSE
        )
      ),
      error = function(e) {
        if (verbose) {
          format_warning(
            "Model contains offset-terms, which could not be considered in the returned null-model.",
            "Coefficients might be inaccurate."
          )
        }
        out <- stats::update(model, ~1, data = update_data, evaluate = FALSE)
      }
    )
  }
  eval(out, envir = NULL)
}


.null_model_mixed <- function(model,
                              offset_term = NULL,
                              model_formula = NULL,
                              update_data = NULL,
                              verbose = TRUE) {
  if (inherits(model, "MixMod")) {
    nullform <- stats::as.formula(paste(find_response(model), "~ 1"))
    null.model <- suppressWarnings(stats::update(model, fixed = nullform, data = update_data))
    # fix fixed effects formula
    null.model$call$fixed <- nullform
  } else if (inherits(model, "cpglmm")) {
    nullform <- model_formula[["random"]]
    out <- suppressWarnings(stats::update(
      model,
      nullform,
      data = update_data,
      evaluate = FALSE
    ))
    null.model <- suppressWarnings(eval(out, envir = NULL))
  } else if (inherits(model, "glmmTMB") && !is.null(model_formula$zero_inflated)) {
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
      } else if (startsWith(f_names, "zi")) {
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
    model_args$data <- update_data
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
      if (is.null(offset_term)) {
        out <- suppressWarnings(stats::update(
          model,
          nullform,
          data = update_data,
          evaluate = FALSE
        ))
        suppressWarnings(eval(out, envir = NULL))
      } else {
        out <- suppressWarnings(do.call(
          stats::update,
          list(
            model,
            formula = nullform,
            data = update_data,
            offset = str2lang(offset_term),
            evaluate = FALSE
          )
        ))
        suppressWarnings(eval(out, envir = NULL))
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
