#' @title Compute intercept-only model for regression models
#' @name null_model
#'
#' @description This function computes the null-model (i.e. \code{(y ~ 1)}) of
#'   a model. For mixed models, the null-model takes random effects into account.
#'
#' @param model A (mixed effects) model.
#' @param verbose Toggle off warnings.
#' @param ... Arguments passed to or from other methods.
#'
#' @return The null-model of \code{x}
#'
#' @examples
#' if (require("lme4")) {
#'   data(sleepstudy)
#'   m <- lmer(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)
#'   summary(m)
#'   summary(null_model(m))
#' }
#' @importFrom stats as.formula update reformulate
#' @export
null_model <- function(model, verbose = TRUE, ...) {
  if (.is_mixed_model(model)) {
    .null_model_mixed(model, verbose)
  } else if (inherits(model, "clm2")) {
    stats::update(model, location = ~1, scale = ~1)
  } else if (inherits(model, "multinom")) {
    stats::update(model, ~1, trace = FALSE)
  } else {
    stats::update(model, ~1, ...)
  }
}


.null_model_mixed <- function(model, verbose = TRUE) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("Package `lme4` needs to be installed to compute variances for mixed models.", call. = FALSE)
  }

  if (inherits(model, "MixMod")) {
    nullform <- stats::as.formula(paste(find_response(model), "~ 1"))
    null.model <- stats::update(model, fixed = nullform)
  } else if (inherits(model, "cpglmm")) {
    nullform <- find_formula(model)[["random"]]
    null.model <- stats::update(model, nullform)
  } else {
    f <- stats::formula(model)
    resp <- find_response(model)
    re.terms <- paste0("(", sapply(lme4::findbars(f), .safe_deparse), ")")
    nullform <- stats::reformulate(re.terms, response = resp)
    null.model <- tryCatch(
      {
        stats::update(model, nullform)
      },
      error = function(e) {
        msg <- e$message
        if (verbose) {
          if (grepl("(^object)(.*)(not found$)", msg)) {
            insight::print_color("Can't calculate null-model. Probably the data that was used to fit the model cannot be found.\n", "red")
          } else if (grepl("^could not find function", msg)) {
            insight::print_color("Can't calculate null-model. Probably you need to load the package that was used to fit the model.\n", "red")
          }
        }
        return(NULL)
      }
    )
  }

  null.model
}