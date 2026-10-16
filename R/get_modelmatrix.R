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
get_modelmatrix.glmmPQL <- function(x, ...) {
  # the terms of glmmPQL models have the working response "zz" of the PQL
  # fit, which is not in the data. The model matrix does not need the
  # response, so we remove it from the terms.
  .modelmatrix_model_contrasts(
    object = stats::delete.response(stats::terms(x)),
    model_data = get_data(x, verbose = FALSE),
    model_contrasts = x$contrasts,
    ...
  )
}

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
get_modelmatrix.Arima <- function(x, data = NULL, ...) {
  if (!is.null(data)) {
    format_error("New data is not supported for arima models.")
  }
  # in `coef()`, the regression coefficients follow the AR, MA, SAR and SMA
  # coefficients. arima() names the intercept "intercept", the other
  # coefficients take the column names of `xreg`, or the deparsed `xreg`
  # argument when it has no column names.
  params <- stats::coef(x)
  regression_names <- names(params)[seq_along(params) > sum(x$arma[1:4])]
  n_obs <- length(x$residuals)

  # forecast::Arima() fits store `xreg`, stats::arima() fits only the call.
  # We use `[[` to avoid partial matching by `$`.
  xreg <- x[["xreg"]]
  xreg_call <- x$call[["xreg"]]
  xreg_label <- if (is.null(xreg) && !is.null(xreg_call)) {
    safe_deparse(xreg_call)
  } else {
    "xreg"
  }
  if (is.null(xreg) && !is.null(xreg_call)) {
    # first try, parent frame, second try, global env. A function of the
    # same name, like `q()`, is not the regressor. An object that holds NULL
    # means that the model has no regressors.
    candidates <- list()
    for (envir in list(parent.frame(), globalenv())) {
      value <- tryCatch(eval(xreg_call, envir = envir), error = function(e) e)
      if (!inherits(value, "error") && !is.function(value)) {
        candidates[length(candidates) + 1] <- list(value)
      }
    }
    if (!length(candidates)) {
      format_error(paste0(
        "Could not find the regressors `",
        xreg_label,
        "` of the arima model. The object must be visible from where",
        " `get_modelmatrix()` is called, or from the global environment."
      ))
    }
    # the parent frame can hold another object of the same name, for example
    # a local variable of a function that calls `get_modelmatrix()`. So we
    # take the first object whose size matches the model.
    matching <- vapply(
      candidates,
      .arima_xreg_matches,
      logical(1),
      n_obs = n_obs,
      regression_names = regression_names
    )
    xreg <- candidates[[if (any(matching)) which(matching)[1] else 1]]
  }
  if (is.null(xreg)) {
    xreg <- matrix(numeric(0), nrow = n_obs, ncol = 0)
  }
  xreg <- as.matrix(xreg)

  # an object of the same name may hold other values than those used to fit
  # the model, so we check the number of rows and columns
  if (nrow(xreg) != n_obs) {
    format_error(paste0(
      "The regressors `",
      xreg_label,
      "` have ",
      nrow(xreg),
      " rows, but the model has ",
      n_obs,
      " observations."
    ))
  }
  has_intercept <- length(regression_names) == ncol(xreg) + 1 &&
    regression_names[1] == "intercept"
  if (!has_intercept && length(regression_names) != ncol(xreg)) {
    format_error(paste0(
      "The regressors `",
      xreg_label,
      "` have ",
      ncol(xreg),
      " columns, but the model has ",
      length(regression_names),
      " regression coefficients."
    ))
  }
  if (has_intercept) {
    xreg <- cbind(1, xreg)
  }
  matrix(
    as.numeric(xreg),
    nrow = n_obs,
    dimnames = list(NULL, regression_names)
  )
}

# TRUE if `value` has one row per observation and one column per regression
# coefficient, or one column less when the first coefficient is the intercept
.arima_xreg_matches <- function(value, n_obs, regression_names) {
  if (is.null(value)) {
    n_rows <- n_obs
    n_cols <- 0
  } else {
    n_rows <- NROW(value)
    n_cols <- NCOL(value)
  }
  one_per_coef <- n_cols == length(regression_names)
  intercept_and_one_per_coef <- n_cols + 1 == length(regression_names) &&
    regression_names[1] == "intercept"
  n_rows == n_obs && (one_per_coef || intercept_and_one_per_coef)
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
    # the levels from the model data, too. Ordered factors stay ordered,
    # so that they get the polynomial contrasts of the model.
    is_categorical <- function(i) is.factor(i) || is.character(i)
    for (i in intersect(names(Filter(is_categorical, model_data)), colnames(dots$data))) {
      dots$data[[i]] <- factor(
        dots$data[[i]],
        levels = levels(factor(model_data[[i]])),
        ordered = is.ordered(model_data[[i]])
      )
    }
    # model.matrix() drops rows with missing values, which includes levels
    # that the model data does not have. We keep these rows, so that the
    # model matrix has one row per row of the new data.
    dots$data <- stats::model.frame(object, data = dots$data, na.action = stats::na.pass)
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
