# Get SE ------------------------------------------------------------------

get_predicted_se <- function(x,
                             data = NULL,
                             ci_type = "confidence",
                             vcov_estimation = NULL,
                             vcov_type = NULL,
                             vcov_args = NULL) {

  # Matrix-multiply X by the parameter vector B to get the predictions, then
  # extract the variance-covariance matrix V of the parameters and compute XVX'
  # to get the variance-covariance matrix of the predictions. The square-root of
  # the diagonal of this matrix represent the standard errors of the predictions,
  # which are then multiplied by 1.96 for the confidence intervals.

  vcovmat <- .get_predicted_ci_vcov(
    x,
    vcov_estimation = vcov_estimation,
    vcov_type = vcov_type,
    vcov_args = vcov_args
  )
  mm <- .get_predicted_ci_modelmatrix(x, data = data, vcovmat = vcovmat)

  # return NULL for fail
  if (is.null(mm)) {
    return(NULL)
  }

  if (ncol(mm) != ncol(vcovmat)) {
    # last desperate try
    mm_full <- get_modelmatrix(x)
    mm <- tryCatch(
      {
        mm_full[as.numeric(row.names(get_modelmatrix(x, data = data))), , drop = FALSE]
      },
      error = function(e) {
        NULL
      }
    )
    # still no match?
    if (isTRUE(ncol(mm) != ncol(vcovmat))) {
      warning(format_message("Could not compute standard errors or confidence intervals because the model and variance-covariance matrices are non-conformable. This can sometimes happen when the `data` used to make predictions fails to include all the levels of a factor variable or all the interaction components."), call. = FALSE)
      return(NULL)
    }
  }

  # compute vcov for predictions
  # Next line equivalent to: diag(M V M')
  var_diag <- colSums(t(mm %*% vcovmat) * t(mm))

  # add sigma to standard errors, i.e. confidence or prediction intervals
  ci_type <- match.arg(ci_type, c("confidence", "prediction"))
  if (ci_type == "prediction") {
    if (is_mixed_model(x)) {
      se <- sqrt(var_diag + get_variance_residual(x))
    } else {
      se <- sqrt(var_diag + get_sigma(x)^2)
    }
  } else {
    se <- sqrt(var_diag)
  }

  se
}




# Get Variance-covariance Matrix ---------------------------------------------------


.get_predicted_ci_vcov <- function(x,
                                   vcov_estimation = NULL,
                                   vcov_type = NULL,
                                   vcov_args = NULL) {

  # (robust) variance-covariance matrix
  if (!is.null(vcov_estimation) && !is.matrix(vcov_estimation)) {
    # check for existing vcov-prefix
    if (!grepl("^vcov", vcov_estimation)) {
      vcov_estimation <- paste0("vcov", vcov_estimation)
    }

    # set default for clubSandwich
    if (vcov_estimation == "vcovCR" && is.null(vcov_type)) {
      vcov_type <- "CR0"
    }

    if (!is.null(vcov_type) && vcov_type %in% c("CR0", "CR1", "CR1p", "CR1S", "CR2", "CR3")) {
      # installed?
      check_if_installed("clubSandwich")
      robust_package <- "clubSandwich"
      vcov_estimation <- "vcovCR"
    } else {
      # installed?
      check_if_installed("sandwich")
      robust_package <- "sandwich"
    }

    # compute robust standard errors based on vcov
    if (robust_package == "sandwich") {
      vcov_estimation <- get(vcov_estimation, asNamespace("sandwich"))
      vcovmat <- as.matrix(do.call(vcov_estimation, c(list(x = x, type = vcov_type), vcov_args)))
    } else {
      vcov_estimation <- clubSandwich::vcovCR
      vcovmat <- as.matrix(do.call(vcov_estimation, c(list(obj = x, type = vcov_type), vcov_args)))
    }
  } else if (!is.matrix(vcov_estimation)) {
    # get variance-covariance-matrix, depending on model type
    vcovmat <- get_varcov(x, component = "conditional")
  } else {
    vcovmat <- vcov_estimation
  }

  vcovmat
}




# Get Model matrix ------------------------------------------------------------

.get_predicted_ci_modelmatrix <- function(x, data = NULL, vcovmat = NULL, ...) {
  resp <- find_response(x)
  if (is.null(vcovmat)) vcovmat <- .get_predicted_ci_vcov(x, ...)


  if (is.null(data)) {
    mm <- get_modelmatrix(x)
  } else {
    if (!all(resp %in% data)) data[[resp]] <- 0 # fake response
    # else, model.matrix below fails, e.g. for log-terms
    attr(data, "terms") <- NULL

    # In these models we need to drop offset from model_terms. To do this, we
    # must construct the mm by calling `get_modelmatrix` on modified model
    # terms.  When we do not need to drop offset terms, we call get_modelmatrix
    # on the model itself. The latter strategy is safer in cases where `data`
    # does not include all the levels of a factor variable.
    if (inherits(x, c("zeroinfl", "hurdle", "zerotrunc"))) {

      # model terms, required for model matrix
      model_terms <- tryCatch(
        {
          stats::terms(x)
        },
        error = function(e) {
          find_formula(x)$conditional
        }
      )

      all_terms <- find_terms(x)$conditional
      off_terms <- grepl("^offset\\((.*)\\)", all_terms)
      if (any(off_terms)) {
        all_terms <- all_terms[!off_terms]
        # TODO: preserve interactions
        vcov_names <- dimnames(vcovmat)[[1]][grepl(":", dimnames(vcovmat)[[1]], fixed = TRUE)]
        if (length(vcov_names)) {
          vcov_names <- gsub(":", "*", vcov_names, fixed = TRUE)
          all_terms <- unique(c(all_terms, vcov_names))
        }
        off_terms <- grepl("^offset\\((.*)\\)", all_terms)
        model_terms <- stats::reformulate(all_terms[!off_terms], response = find_response(x))
      }

      # check for at least to factor levels, in order to build contrasts
      single_factor_levels <- sapply(data, function(i) is.factor(i) && nlevels(i) == 1)
      if (any(single_factor_levels)) {
        warning(format_message("Some factors in the data have only one level. Cannot compute model matrix for standard errors and confidence intervals."), call. = FALSE)
        return(NULL)
      }

      mm <- get_modelmatrix(model_terms, data = data)
    } else {
      mm <- get_modelmatrix(x, data = data)
    }
  }

  # fix rank deficiency
  if (ncol(vcovmat) < ncol(mm)) {
    mm <- mm[, intersect(colnames(mm), colnames(vcovmat))]
  }

  mm
}
