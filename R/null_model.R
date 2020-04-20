#' @title Compute intercept-only model for mixed models
#' @name null_model
#'
#' @description This function compute the null-model (i.e. \code{(y ~ 1)}) for the
#'   fixed effects part) of a random-intercept model.
#'
#' @param model A mixed effects model.
#' @param verbose Toggle off warnings.
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
null_model <- function(model, verbose = TRUE) {
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
