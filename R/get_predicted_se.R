# Get SE ------------------------------------------------------------------

get_predicted_se <- function(x,
                             data = NULL,
                             ci_type = "confidence",
                             vcov = NULL,
                             vcov_args = NULL,
                             ci_method = NULL,
                             verbose = TRUE,
                             ...) {
  # Matrix-multiply X by the parameter vector B to get the predictions, then
  # extract the variance-covariance matrix V of the parameters and compute XVX'
  # to get the variance-covariance matrix of the predictions. The square-root of
  # the diagonal of this matrix represent the standard errors of the predictions,
  # which are then multiplied by 1.96 for the confidence intervals.


  ## TODO: what about Satterthwaite? @vincentarelbundock

  # kenward-roger adjusts both the dof and the varcov
  if (isTRUE(ci_method %in% c("kenward-roger", "kenward", "kr"))) {
    if (is.null(vcov) && is.null(vcov_args)) {
      check_if_installed("pbkrtest")
      vcovmat <- as.matrix(pbkrtest::vcovAdj(x))
    } else {
      format_error("The `vcov` argument cannot be used together with the `ci_method=\"kenward-roger\"` argument.")
    }

    # all other varcov types can be supplied manually
  } else {
    vcovmat <- get_varcov(
      x,
      vcov = vcov,
      vcov_args = vcov_args,
      ...
    )
  }

  mm <- .get_predicted_ci_modelmatrix(x, data = data, vcovmat = vcovmat, verbose = verbose)

  # return NULL for fail
  if (is.null(mm)) {
    return(NULL)
  }

  # some tweaking for multinomial / ordinal models
  if (inherits(x, "polr")) {
    keep <- intersect(colnames(mm), colnames(vcovmat))
    vcovmat <- vcovmat[keep, keep, drop = FALSE]
    mm <- mm[, keep, drop = FALSE]
  } else if (inherits(x, c("multinom", "brmultinom", "bracl", "mixor", "fixest"))) {
    ## BUG this currently doesn't work...

    # models like multinom have "level:termname" as column name
    # remove response level to match column names of model matrix
    colnames(vcovmat) <- gsub("^(+.):(.*)", "\\2", colnames(vcovmat))
    keep <- setdiff(intersect(colnames(mm), colnames(vcovmat)), "(Intercept)")
    vcovmat <- vcovmat[colnames(vcovmat) %in% keep, colnames(vcovmat) %in% keep, drop = FALSE]
    mm <- mm[, keep, drop = FALSE]

    # sometimes, model matrix and vcov won't match exactly, so try some hacks here
  } else if (ncol(mm) != ncol(vcovmat)) {
    # last desperate try
    if (ncol(mm) == nrow(mm) && ncol(vcovmat) > ncol(mm) && all(colnames(mm) %in% colnames(vcovmat))) {
      # we might have a correct model matrix, but the vcov matrix contains values
      # from specific model parameters that do not appear in the model matrix
      # we then need to reduce the vcov matrix.
      matching_parameters <- stats::na.omit(match(colnames(vcovmat), colnames(mm)))
      vcovmat <- vcovmat[matching_parameters, matching_parameters, drop = FALSE]
    } else {
      # VAB: below there is some matching code to reduce matrices to make them
      # conformable. I am not sure at all it is a good idea to do that in
      # general, but we may want to revisit eventually. Currently, the code
      # below is commented out because it is dangerous and silently produces incorrect
      # numerical results in some cases. We CANNOT uncomment it before thorough.
      # See example at the bottom of test-get_predicted.R

      # # model matrix rows might mismatch. we need a larger model matrix and
      # # then filter those rows that match the vcov matrix.
      # mm_full <- get_modelmatrix(x)
      # mm <- tryCatch(
      #   mm_full[as.numeric(row.names(get_modelmatrix(x, data = data))), , drop = FALSE],
      #   error = function(e) NULL
      # )
    }

    # still no match?
    if (isTRUE(ncol(mm) != ncol(vcovmat))) {
      if (verbose) {
        format_warning("Could not compute standard errors or confidence intervals because the model and variance-covariance matrices are non-conformable. This can sometimes happen when the `data` used to make predictions fails to include all the levels of a factor variable or all the interaction components.")
      }
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

  ## TODO: check if this is correct. Based on the vcov, we have the same SEs
  # for a predictor for each response level, so we simply repeat the SEs for
  # each level. This might not be appropriate and needs to be checked!

  # make sure we repeat SE for each response level for
  # models with categorical or ordinal response.
  if (inherits(x, c("polr", "multinom", "mixor"))) {
    se <- rep(se, times = n_unique(get_response(x, as_proportion = TRUE)))
  }

  se
}


# Get Model matrix ------------------------------------------------------------

.get_predicted_ci_modelmatrix <- function(x, data = NULL, vcovmat = NULL, verbose = TRUE, ...) {
  resp <- find_response(x, combine = FALSE)
  if (is.null(vcovmat)) vcovmat <- get_varcov(x, ...)


  if (is.null(data)) {
    mm <- get_modelmatrix(x)
  } else {
    if (!all(resp %in% colnames(data))) data[resp] <- 0 # fake response
    # else, model.matrix below fails, e.g. for log-terms
    attr(data, "terms") <- NULL

    # In these models we need to drop offset from model_terms. To do this, we
    # must construct the mm by calling `get_modelmatrix` on modified model
    # terms.  When we do not need to drop offset terms, we call get_modelmatrix
    # on the model itself. The latter strategy is safer in cases where `data`
    # does not include all the levels of a factor variable.
    if (inherits(x, c("zeroinfl", "hurdle", "zerotrunc", "MixMod"))) {
      # model terms, required for model matrix
      model_terms <- tryCatch(stats::terms(x),
        error = function(e) find_formula(x, verbose = FALSE)$conditional
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
        if (verbose) {
          format_warning("Some factors in the data have only one level. Cannot compute model matrix for standard errors and confidence intervals.")
        }
        return(NULL)
      }
      obj <- model_terms

      # extra handling for polr
    } else if (inherits(x, c("polr", "multinom", "brmultinom", "bracl"))) {
      # mixor, fixest?
      # model terms, required for model matrix
      obj <- tryCatch(stats::terms(x), error = function(e) find_formula(x)$conditional)
    } else {
      obj <- x
    }

    # try to get model matrix
    mm <- tryCatch(
      get_modelmatrix(x = obj, data = data),
      error = function(e) e
    )

    # tell user when fails
    if (inherits(mm, "simpleError")) {
      if (verbose) {
        if (grepl("2 or more levels", mm$message, fixed = TRUE)) {
          format_warning("Some factors in the data have only one level. Cannot compute model matrix for standard errors and confidence intervals.")
        } else {
          format_warning("Something went wrong with computing standard errors and confidence intervals for predictions.")
        }
      }
      return(NULL)
    }
  }

  # fix rank deficiency
  if (ncol(vcovmat) < ncol(mm)) {
    mm <- mm[, intersect(colnames(mm), colnames(vcovmat))]
  }

  mm
}
