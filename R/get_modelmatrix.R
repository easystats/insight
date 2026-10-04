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
get_modelmatrix.default <- function(x, data = NULL, ...) {
  if (is.null(data)) {
    mm <- stats::model.matrix(object = x, ...)
  } else {
    mm <- .pad_modelmatrix_unpad(x, data, ...)
  }
  mm
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
get_modelmatrix.coxme <- function(x, ...) {
  model_terms <- stats::terms(x)
  dots <- list(...)
  if ("data" %in% names(dots)) {
    stats::model.matrix(model_terms, data = dots$data, ...)
  } else {
    stats::model.matrix(model_terms, data = get_data(x), ...)
  }
}

#' @export
get_modelmatrix.bracl <- function(x, ...) {
  dots <- list(...)
  if ("data" %in% names(dots)) {
    mm <- stats::model.matrix(object = x, data = dots$data, ...)
  } else {
    format_error("The `data` argument is required to return the model matrix.")
  }

  mm
}

#' @export
get_modelmatrix.serp <- function(x, ...) {
  dots <- list(...)
  if ("data" %in% names(dots)) {
    mm <- stats::model.matrix(object = x$Terms, data = dots$data, ...)
  } else {
    mm <- stats::model.matrix(object = x$Terms, data = get_data(x), ...)
  }

  mm
}

#' @export
get_modelmatrix.iv_robust <- function(x, ...) {
  dots <- list(...)
  model_terms <- stats::terms(x)
  if ("data" %in% names(dots)) {
    # validation check - model matrix needs response vector!
    resp <- find_response(x)
    d <- dots$data
    dots$data <- NULL
    if (!is.null(resp) && !resp %in% names(d)) {
      # fake response
      d[[resp]] <- 0
    }
    mm <- do.call(stats::model.matrix, compact_list(list(model_terms, data = d, dots)))
  } else {
    mm <- stats::model.matrix(model_terms, data = get_data(x, verbose = FALSE), ...)
  }

  mm
}

#' @export
get_modelmatrix.lm_robust <- function(x, ...) {
  dots <- list(...)
  if ("data" %in% names(dots)) {
    # validation check - model matrix needs response vector!
    resp <- find_response(x)
    d <- dots$data
    dots$data <- NULL
    if (!is.null(resp) && !resp %in% names(d)) {
      # fake response
      d[[resp]] <- 0
    }
    mm <- do.call(stats::model.matrix, compact_list(list(x, data = d, dots)))
  } else {
    mm <- stats::model.matrix(x, data = get_data(x, verbose = FALSE), ...)
  }
  mm
}

#' @export
get_modelmatrix.ivreg <- get_modelmatrix.iv_robust


#' @export
get_modelmatrix.lme <- function(x, ...) {
  # we use the terms, not the model object: their "predvars" keep the basis
  # of terms like poly() for new data, and model.matrix() methods for lme
  # objects from other packages (like MuMIn) ignore `data` and `contrasts.arg`
  .modelmatrix_model_contrasts(
    object = stats::terms(x),
    model_data = get_data(x, verbose = FALSE),
    model_contrasts = x$contrasts,
    ...
  )
}

#' @export
get_modelmatrix.gls <- get_modelmatrix.lme

#' @export
get_modelmatrix.clmm <- function(x, ...) {
  # model.matrix() for clmm objects ignores the `data` and `contrasts.arg`
  # arguments, so we use the terms of the fixed effects. The response is
  # removed, because new data may not contain it. Models fitted with
  # `model = FALSE` store no model frame, so we use get_data() instead.
  model_data <- x$model
  if (is.null(model_data)) {
    model_data <- get_data(x, verbose = FALSE)
  }
  .modelmatrix_model_contrasts(
    object = stats::delete.response(stats::terms(x)),
    model_data = model_data,
    model_contrasts = x$contrasts,
    ...
  )
}

#' @export
get_modelmatrix.svyglm <- function(x, ...) {
  dots <- list(...)
  if ("data" %in% names(dots)) {
    model_data <- tryCatch(
      {
        d <- as.data.frame(dots$data)
        response_name <- find_response(x)
        response_variable <- get_response(x, as_proportion = TRUE)
        if (is.factor(response_variable)) {
          d[[response_name]] <- levels(response_variable)[1]
        } else {
          d[[response_name]] <- mean(response_variable)
        }
        d
      },
      error = function(e) {
        dots$data
      }
    )
    model_terms <- stats::terms(x)
    mm <- stats::model.matrix(model_terms, data = model_data)
  } else {
    mm <- stats::model.matrix(object = x, ...)
  }

  mm
}

#' @export
get_modelmatrix.svyolr <- get_modelmatrix.svyglm

#' @export
get_modelmatrix.svycoxph <- get_modelmatrix.svyglm

#' @export
get_modelmatrix.svysurvreg <- get_modelmatrix.svyglm


#' @export
get_modelmatrix.brmsfit <- function(x, ...) {
  conditional_formula <- find_formula(x, verbose = FALSE)$conditional
  formula_rhs <- safe_deparse(conditional_formula[[3]])
  model_data <- get_data(x, verbose = FALSE)
  # exception: for null-models, we need different handling, else `reformulate()`
  # will not work.
  if (identical(formula_rhs, "1")) {
    matrix(1, nrow = nrow(model_data), dimnames = list(NULL, "(Intercept)"))
  } else {
    formula_rhs <- stats::as.formula(paste0("~", formula_rhs))
    intercept <- has_intercept(x, verbose = FALSE)
    predictors <- setdiff(all.vars(formula_rhs), "Intercept")
    # the formula used in model.matrix() is not allowed to have special functions,
    # like brms::mo() and similar. Thus, we reformulate after using "all.vars()",
    # which will only keep the variable names.
    if (!length(predictors)) {
      if (intercept) {
        matrix(1, nrow = nrow(model_data), dimnames = list(NULL, "(Intercept)"))
      } else {
        matrix(nrow = nrow(model_data), ncol = 0)
      }
    } else {
      # brms takes the contrasts from the factors in the data. Re-leveling
      # new data drops this attribute, so we pass the contrasts explicitly.
      # Only predictors are used, because model.matrix() warns about
      # contrasts for variables that are not in the formula.
      model_contrasts <- compact_list(lapply(
        Filter(is.factor, model_data[intersect(predictors, colnames(model_data))]),
        attr,
        which = "contrasts"
      ))
      .modelmatrix_model_contrasts(
        object = stats::reformulate(predictors, intercept = intercept),
        model_data = model_data,
        model_contrasts = model_contrasts,
        ...
      )
    }
  }
}

#' @export
get_modelmatrix.rlm <- function(x, ...) {
  dots <- list(...)
  # `rlm` objects can inherit to model.matrix.lm, but that function does
  # not accept the `data` argument for `rlm` objects
  if (is.null(dots$data)) {
    mf <- stats::model.frame(x, xleve = x$xlevels, ...)
  } else {
    mf <- stats::model.frame(x, xleve = x$xlevels, data = dots$data, ...)
  }
  mm <- stats::model.matrix.default(x, data = mf, contrasts.arg = x$contrasts)
  mm
}


#' @export
get_modelmatrix.betareg <- function(x, ...) {
  dots <- list(...)
  if (is.null(dots$data)) {
    mm <- stats::model.matrix(x, ...)
  } else {
    element_name <- .betareg_mean_element(x)
    # adapted from betareg::predict.betareg()
    # suppress contrasts dropped from factor
    mf <- suppressWarnings(stats::model.frame(
      stats::delete.response(x$terms[["mean"]]),
      dots$data,
      na.action = stats::na.pass,
      xlev = x$levels[["mean"]]
    ))
    mm <- stats::model.matrix(stats::delete.response(x$terms[[element_name]]), mf)
  }
  mm
}


#' @export
get_modelmatrix.cpglmm <- function(x, ...) {
  check_if_installed("cplm")
  cplm::model.matrix(x, ...)
}

#' @export
get_modelmatrix.afex_aov <- function(x, ...) {
  stats::model.matrix(object = x$lm, ...)
}


#' @export
get_modelmatrix.BFBayesFactor <- function(x, ...) {
  check_if_installed("BayesFactor")
  BayesFactor::model.matrix(x, ...)
}


# helper ----------------

# `object` is passed to model.matrix(), `model_data` is the data used when the
# user provides no `data` argument, and `model_contrasts` are the contrasts
# the model was fitted with.
.modelmatrix_model_contrasts <- function(object, model_data, model_contrasts = NULL, ...) {
  dots <- list(...)
  # model.matrix() does not use the contrasts stored in the model object,
  # so we pass them explicitly, unless the user provides own contrasts
  if (!"contrasts.arg" %in% names(dots) && length(model_contrasts)) {
    dots$contrasts.arg <- model_contrasts
  }
  if (is.null(dots$data)) {
    dots$data <- model_data
  } else {
    # new data may not contain all factor levels, which the contrasts
    # require, so we use the factor levels from the model data. Character
    # vectors are converted to factors by model.matrix(), so they need
    # the levels from the model data, too.
    is_categorical <- function(i) is.factor(i) || is.character(i)
    for (i in intersect(names(Filter(is_categorical, model_data)), colnames(dots$data))) {
      dots$data[[i]] <- factor(dots$data[[i]], levels = levels(factor(model_data[[i]])))
    }
  }
  do.call(stats::model.matrix, c(list(object = object), dots))
}


.pad_modelmatrix_unpad <- function(x, data, ...) {
  data <- .pad_modelmatrix(x = x, data = data)
  # replace columns that only contain NA values with 1 - else, model.matrix() fails
  for (n in names(data)) {
    if (all(is.na(data[[n]]))) {
      data[[n]] <- 1
    }
  }
  mm <- stats::model.matrix(object = x, data = data, ...)
  .unpad_modelmatrix(mm = mm, data = data)
}


.pad_modelmatrix <- function(x, data, ...) {
  # recycle to include all factors from all variables
  # min_levels is insufficient when used in stats::model.matrix
  modeldata <- get_data(x, verbose = FALSE)
  fac <- lapply(Filter(is.factor, modeldata), unique)

  # no factor
  if (length(fac) == 0) {
    out <- data
    attr(out, "pad") <- 0
    return(out)
  }

  maxlev <- max(lengths(fac, use.names = FALSE))
  pad <- data[rep(1, maxlev), , drop = FALSE]
  for (n in names(fac)) {
    pad[[n]][seq_along(fac[[n]])] <- fac[[n]]
  }
  out <- rbind(pad, data)
  row.names(out) <- NULL
  attr(out, "pad") <- nrow(pad)
  out
}


.unpad_modelmatrix <- function(mm, data) {
  npad <- attr(data, "pad")
  mm[(npad + 1):nrow(mm), , drop = FALSE]
}
