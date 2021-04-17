#' Model Matrix
#'
#' Creates a design matrix from the description. Any character variables are coerced to factors.
#'
#' @param x An object.
#' @param ... Passed down to other methods (mainly \code{model.matrix()}).
#'
#' @examples
#' data(mtcars)
#'
#' model <- lm(am ~ vs, data = mtcars)
#' get_modelmatrix(model)
#' @export
get_modelmatrix <- function(x, ...) {
  UseMethod("get_modelmatrix")
}


#' @export
get_modelmatrix.default <- function(x, ...) {
  stats::model.matrix(object = x, ...)
}

#' @export
get_modelmatrix.lme <- function(x, ...) {
  # we check the dots for a "data" argument. To make model.matrix work
  # for certain objects, we need to specify the data-argument explicitly,
  # however, if the user provides a data-argument, this should be used instead.

  data_in_dots <- .data_in_dots(..., default = get_data(x))
  stats::model.matrix(object = x, data = data_in_dots, ...)
}

#' @export
get_modelmatrix.gls <- get_modelmatrix.lme

#' @export
get_modelmatrix.clmm <- function(x, ...) {
  # former implementation in "get_variance()"
  # f <- find_formula(x)$conditional
  # stats::model.matrix(object = f, data = x$model, ...)

  # but this seems to work, too
  data_in_dots <- .data_in_dots(..., default = x$model)
  stats::model.matrix(object = x, data = data_in_dots, ...)
}

#' @export
get_modelmatrix.brmsfit <- function(x, ...) {
  formula_rhs <- .safe_deparse(find_formula(x)$conditional[[3]])
  formula_rhs <- stats::as.formula(paste0("~", formula_rhs))
  data_in_dots <- .data_in_dots(..., default = get_data(x))
  stats::model.matrix(formula_rhs, data = data_in_dots, ...)
}

#' @export
get_modelmatrix.cpglmm <- function(x, ...) {
  if (!requireNamespace("cplm", quietly = TRUE)) {
    stop("Package 'cplm' required. Please install it.")
  }
  cplm::model.matrix(x, ...)
}






# helper ----------------

.data_in_dots <- function(..., default = NULL) {
  dot.arguments <- lapply(match.call(expand.dots = FALSE)$`...`, function(x) x)
  if ("data" %in% names(dot.arguments)) {
    eval(dot.arguments[["data"]])
  } else {
    default
  }
}
