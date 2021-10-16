#' Model Matrix
#'
#' Creates a design matrix from the description. Any character variables are coerced to factors.
#'
#' @param x An object.
#' @param ... Passed down to other methods (mainly `model.matrix()`).
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
get_modelmatrix.merMod <- function(x, ...) {
  dots <- list(...)
  if ("data" %in% names(dots)) {
    model_terms <- stats::terms(x)
    mm <- stats::model.matrix(model_terms, ...)
  } else {
    mm <- stats::model.matrix(object = x, ...)
  }

  mm
}

#' @export
get_modelmatrix.lme <- function(x, ...) {
  # we check the dots for a "data" argument. To make model.matrix work
  # for certain objects, we need to specify the data-argument explicitly,
  # however, if the user provides a data-argument, this should be used instead.
  .data_in_dots(..., object = x, default_data = get_data(x))
}

#' @export
get_modelmatrix.gls <- get_modelmatrix.lme

#' @export
get_modelmatrix.clmm <- function(x, ...) {
  # former implementation in "get_variance()"
  # f <- find_formula(x)$conditional
  # stats::model.matrix(object = f, data = x$model, ...)
  .data_in_dots(..., object = x, default_data = x$model)
}

#' @export
get_modelmatrix.brmsfit <- function(x, ...) {
  formula_rhs <- .safe_deparse(find_formula(x)$conditional[[3]])
  formula_rhs <- stats::as.formula(paste0("~", formula_rhs))
  .data_in_dots(..., object = formula_rhs, default_data = get_data(x))
}

#' @export
get_modelmatrix.rlm <- function(x, ...) {
  dots <- list(...)
  # `rlm` objects can inherit to model.matrix.lm, but that function does
  # not accept the `data` argument for `rlm` objects
  if (is.null(dots$data)) {
    mf <- stats::model.frame(x,
      xleve = x$xlevels,
      ...
    )
  } else {
    mf <- stats::model.frame(x,
      xleve = x$xlevels,
      data = dots$data,
      ...
    )
  }
  mm <- stats::model.matrix.default(x,
    data = mf,
    contrasts.arg = x$contrasts
  )
  return(mm)
}

#' @export
get_modelmatrix.cpglmm <- function(x, ...) {
  # installed?
  check_if_installed("cplm")
  cplm::model.matrix(x, ...)
}

#' @export
get_modelmatrix.afex_aov <- function(x, ...) {
  stats::model.matrix(object = x$lm, ...)
}

# helper ----------------

.data_in_dots <- function(..., object = NULL, default_data = NULL) {
  dot.arguments <- lapply(match.call(expand.dots = FALSE)$`...`, function(x) x)
  data_arg <- if ("data" %in% names(dot.arguments)) {
    eval(dot.arguments[["data"]])
  } else {
    default_data
  }
  remaining_dots <- setdiff(names(dot.arguments), "data")
  do.call(stats::model.matrix, c(list(object = object, data = data_arg), remaining_dots))
}
