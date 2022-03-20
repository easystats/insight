# Get SE ------------------------------------------------------------------

get_predicted_se <- function(x,
                             data = NULL,
                             ci_type = "confidence",
                             vcov = NULL,
                             vcov_args = NULL,
                             ...) {

  # Matrix-multiply X by the parameter vector B to get the predictions, then
  # extract the variance-covariance matrix V of the parameters and compute XVX'
  # to get the variance-covariance matrix of the predictions. The square-root of
  # the diagonal of this matrix represent the standard errors of the predictions,
  # which are then multiplied by 1.96 for the confidence intervals.

  vcovmat <- .get_varcov_sandwich(
    x,
    vcov_fun = vcov,
    vcov_args = vcov_args,
    ...
  )
  mm <- .get_predicted_ci_modelmatrix(x, data = data, vcovmat = vcovmat)

  # return NULL for fail
  if (is.null(mm)) {
    return(NULL)
  }

  # last desperate try
  if (ncol(mm) != ncol(vcovmat)) {
    if (ncol(mm) == nrow(mm) && ncol(vcovmat) > ncol(mm) && all(colnames(mm) %in% colnames(vcovmat))) {

      # we might have a correct model matrix, but the vcov matrix contains values
      # from specific model parameters that do not appear in the model matrix
      # we then need to reduce the vcov matrix.

      matching_parameters <- stats::na.omit(match(colnames(vcovmat), colnames(mm)))
      vcovmat <- vcovmat[matching_parameters, matching_parameters, drop = FALSE]
    } else {

      # model matrix rows might mismatch. we need a larger model matrix and
      # then filter those rows that match the vcov matrix.

      mm_full <- get_modelmatrix(x)
      mm <- tryCatch(
        {
          mm_full[as.numeric(row.names(get_modelmatrix(x, data = data))), , drop = FALSE]
        },
        error = function(e) {
          NULL
        }
      )
    }

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





# Get Model matrix ------------------------------------------------------------

.get_predicted_ci_modelmatrix <- function(x, data = NULL, vcovmat = NULL, ...) {
  resp <- find_response(x)
  if (is.null(vcovmat)) vcovmat <- .get_varcov_sandwich(x, ...)


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
    if (inherits(x, c("zeroinfl", "hurdle", "zerotrunc", "MixMod"))) {

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
