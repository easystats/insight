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
#' @examples
#' if (require("lme4")) {
#'   data(sleepstudy)
#'   m <- lmer(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)
#'   summary(m)
#'   summary(null_model(m))
#' }
#' @export
null_model <- function(model, verbose = TRUE, ...) {
  offset_term <- tryCatch(
    {
      f <- safe_deparse(find_formula(model)$conditional)
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

  if (is_mixed_model(model)) {
    .null_model_mixed(model, offset_term, verbose)
  } else if (inherits(model, "clm2")) {
    stats::update(model, location = ~1, scale = ~1)
  } else if (inherits(model, "multinom")) {
    stats::update(model, ~1, trace = FALSE)
  } else {
    if (!is.null(offset_term)) {
      tryCatch(
        {
          do.call(stats::update, list(model, ~1, offset = .str2lang(offset_term)))
        },
        error = function(e) {
          if (verbose) {
            warning(format_message("Model contains offset-terms, which could not be considered in the returned null-model.",
                                   "Coefficients might be inaccurate."), call. = FALSE)
          }
          stats::update(model, ~1)
        }
      )
    } else {
      # stats::update(model, ~1)
      out <- stats::update(model, ~1, evaluate = FALSE)
      eval(out, envir = parent.frame())
    }
  }
}


.null_model_mixed <- function(model, offset_term = NULL, verbose = TRUE) {
  if (inherits(model, "MixMod")) {
    nullform <- stats::as.formula(paste(find_response(model), "~ 1"))
    null.model <- stats::update(model, fixed = nullform)
  } else if (inherits(model, "cpglmm")) {
    nullform <- find_formula(model, verbose = FALSE)[["random"]]
    null.model <- stats::update(model, nullform)
  } else {
    f <- stats::formula(model)
    resp <- find_response(model)
    re.terms <- paste0("(", sapply(.findbars(f), safe_deparse), ")")
    nullform <- stats::reformulate(re.terms, response = resp)
    null.model <- tryCatch(
      {
        if (!is.null(offset_term)) {
          do.call(stats::update, list(model, formula = nullform, offset = .str2lang(offset_term)))
        } else {
          stats::update(model, nullform)
        }
      },
      error = function(e) {
        msg <- e$message
        if (verbose) {
          if (grepl("(^object)(.*)(not found$)", msg)) {
            print_color("Can't calculate null-model. Probably the data that was used to fit the model cannot be found.\n", "red")
          } else if (grepl("^could not find function", msg)) {
            print_color("Can't calculate null-model. Probably you need to load the package that was used to fit the model.\n", "red")
          }
        }
        return(NULL)
      }
    )
  }

  null.model
}
