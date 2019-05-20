#' @title Get clean names of model paramters
#' @name clean_parameters
#'
#' @description This function "cleans" names of model parameters by removing
#' patterns like \code{"r_"} or \code{"b[]"} (mostly applicable to Stan models)
#' and adding columns with information to which group or component parameters
#' belong (i.e. fixed or random, count or zero-inflated...)
#' \cr \cr
#' The main purpose of this function is to easily filter and select model parameters,
#' especially of posterior samples from Stan models, depending on certain
#' characteristics. This might be useful when only selective results should
#' be reported or results from all parameters should be filtered to return only
#' certain results.
#'
#' @param x A fitted model.
#'
#' @return A data frame with "cleaned" parameter names and information on
#'   effects, component and group where parameters belong to. To be consistent
#'   across different models, the returned data frame always has at least three
#'   columns \code{parameter}, \code{effect} and \code{component}. See 'Details'.
#'
#' @details The \code{effects} column indicate if a parameter is a \emph{fixed}
#' or \emph{random} effect. The \code{component} can either be \emph{conditional}
#' or \emph{zero_inflated}. For models with random effects, the \code{group}
#' column indicates the grouping factor of the random effects. For multivariate
#' response models from \pkg{brms} or \pkg{rstanarm}, an additional \emph{response}
#' column is included, to indicate which parameters belong to which response
#' formula. Furthermore, for models from \pkg{brms} or \pkg{rstanarm} a
#' \emph{cleaned_parameter} column is returned that contains "human readable"
#' parameter names.
#'
#' @examples
#' model <- download_model("brms_zi_2")
#' clean_parameters(model)
#' @export
clean_parameters <- function(x, ...) {
  UseMethod("clean_parameters")
}



#' @export
clean_parameters.default <- function(x, ...) {
  pars <- find_parameters(x, effects = "all", component = "all", flatten = FALSE)

  l <- lapply(names(pars), function(i) {
    eff <- if (grepl("random", i, fixed = TRUE))
      "random"
    else
      "fixed"

    com <- if (grepl("zero_inflated", i, fixed = TRUE))
      "zero_inflated"
    else
      "conditional"

    if (eff == "random") {
      rand_eff <- lapply(names(pars[[i]]), function(j) {
        data.frame(
          parameter = pars[[i]][[j]],
          effects = eff,
          component = com,
          group = j,
          stringsAsFactors = FALSE,
          row.names = NULL
        )
      })
      do.call(rbind, rand_eff)
    } else {
      data.frame(
        parameter = pars[[i]],
        effects = eff,
        component = com,
        group = "",
        stringsAsFactors = FALSE,
        row.names = NULL
      )
    }
  })

  out <- do.call(rbind, l)
  .remove_empty_columns_from_pars(out)
}



#' @export
clean_parameters.brmsfit <- function(x, ...) {
  pars <- find_parameters(x, effects = "all", component = "all", flatten = FALSE)
  is_mv <- is_multivariate(pars)

  if (is_mv) {
    l <- do.call(
      rbind,
      lapply(names(pars), function(i) .get_stan_params(pars[[i]], response = i))
    )
  } else {
    l <- .get_stan_params(pars)
  }

  out <- do.call(rbind, l)
  .remove_empty_columns_from_pars(.clean_brms_params(out, is_mv))
}




#' @export
clean_parameters.stanreg <- function(x, ...) {
  pars <- find_parameters(x, effects = "all", component = "all", flatten = FALSE)
  l <- .get_stan_params(pars)

  out <- do.call(rbind, l)
  .remove_empty_columns_from_pars(.clean_stanreg_params(out))
}



#' @export
clean_parameters.stanmvreg <- function(x, ...) {
  pars <- find_parameters(x, effects = "all", component = "all", flatten = FALSE)

  l <- do.call(
    rbind,
    lapply(names(pars), function(i) .get_stan_params(pars[[i]], response = i))
  )

  out <- do.call(rbind, l)
  .remove_empty_columns_from_pars(.clean_stanreg_params(out))
}






#' @keywords internal
.get_stan_params <- function(pars, response = NA) {
  lapply(names(pars), function(i) {
    eff <- if (grepl("random", i, fixed = TRUE))
      "random"
    else
      "fixed"

    com <- if (grepl("zero_inflated", i, fixed = TRUE))
      "zero_inflated"
    else if (grepl("sigma", i, fixed = TRUE))
      "sigma"
    else if (grepl("priors", i, fixed = TRUE))
      "priors"
    else
      "conditional"

    data.frame(
      parameter = pars[[i]],
      effects = eff,
      component = com,
      group = "",
      response = response,
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  })
}



#' @keywords internal
.clean_brms_params <- function(out, is_mv) {

  out$cleaned_parameter <- out$parameter

  # for multivariate response models, remove responses from parameter names

  if (is_mv) {
    resp <- unique(out$response)

    resp_pattern <- sprintf("_%s_(.*)", resp, resp)
    for (i in resp_pattern) {
      out$cleaned_parameter <- gsub(pattern = i, "_\\1", out$cleaned_parameter, perl = TRUE)
    }

    resp_pattern <- sprintf("__%s(.*)", resp, resp)
    for (i in resp_pattern) {
      out$cleaned_parameter <- gsub(pattern = i, "\\1", out$cleaned_parameter, perl = TRUE)
    }

    resp_pattern <- sprintf("__zi_%s(.*)", resp, resp)
    for (i in resp_pattern) {
      out$cleaned_parameter <- gsub(pattern = i, "\\1", out$cleaned_parameter, perl = TRUE)
    }

    resp_pattern <- sprintf("(sigma)(_%s)", resp, resp)
    for (i in resp_pattern) {
      out$cleaned_parameter <- gsub(pattern = i, "\\1", out$cleaned_parameter, perl = TRUE)
    }
  }

  # clean fixed effects, conditional and zero-inflated

  out$cleaned_parameter <- gsub(pattern = "(b_|bs_|bsp_|bcs_)(?!zi_)(.*)", "\\2", out$cleaned_parameter, perl = TRUE)
  out$cleaned_parameter <- gsub(pattern = "(b_zi_|bs_zi_|bsp_zi_|bcs_zi_)(.*)", "\\2", out$cleaned_parameter, perl = TRUE)

  # extract group-names from random effects and clean random effects

  rand_eff <- grepl("r_(.*)\\.(.*)\\.", out$cleaned_parameter)
  if (any(rand_eff)) {
    r_pars <- gsub("r_(.*)\\.(.*)\\.", "\\2", out$cleaned_parameter[rand_eff])
    r_grps <- gsub("r_(.*)\\.(.*)\\.", "\\1", out$cleaned_parameter[rand_eff])
    r_grps <- gsub("__zi", "", r_grps)

    out$cleaned_parameter[rand_eff] <- r_pars
    out$group[rand_eff] <- r_grps
  }

  # clean remaining parameters

  out$cleaned_parameter <- gsub("^simo_", "", out$cleaned_parameter)
  out$cleaned_parameter <- gsub("^sds_", "", out$cleaned_parameter)
  out$cleaned_parameter <- gsub("^prior_", "", out$cleaned_parameter)

  # fix intercept names

  intercepts <- which(out$cleaned_parameter == "Intercept")
  if (!is_empty_object(intercepts))
    out$cleaned_parameter[intercepts] <- "(Intercept)"

  interaction_terms <- which(grepl("\\.", out$cleaned_parameter))
  if (length(interaction_terms)) {
    for (i in interaction_terms) {
      i_terms <- strsplit(out$cleaned_parameter[i], "\\.")
      find_i_terms <- sapply(i_terms, function(j) j %in% out$cleaned_parameter)
      if (all(find_i_terms)) {
        out$cleaned_parameter[i] <- gsub("\\.", ":", out$cleaned_parameter[i])
      }
    }
  }

  out
}



#' @keywords internal
.clean_stanreg_params <- function(out) {

  out$cleaned_parameter <- out$parameter

  # extract group-names from random effects and clean random effects

  rand_intercepts <- grepl("^b\\[\\(Intercept\\)", out$cleaned_parameter)

  if (any(rand_intercepts)) {
    out$group[rand_intercepts] <- gsub(
      "b\\[\\(Intercept\\) (.*)\\]",
      "\\1",
      out$cleaned_parameter[rand_intercepts]
    )
    out$cleaned_parameter[rand_intercepts] <- "(Intercept)"
  }


  # extract group-names from random effects and clean random effects

  rand_effects <- grepl("^b\\[", out$cleaned_parameter)

  if (any(rand_effects)) {
    r_pars <- gsub("b\\[(.*) (.*)\\]", "\\1", out$cleaned_parameter[rand_effects])
    r_grps <- gsub("b\\[(.*) (.*)\\]", "\\2", out$cleaned_parameter[rand_effects])

    out$cleaned_parameter[rand_effects] <- r_pars
    out$group[rand_effects] <- r_grps
  }

  # clean remaining parameters

  smooth <- grepl("^smooth_sd\\[", out$cleaned_parameter)
  if (length(smooth)) {
    out$cleaned_parameter <- gsub("^smooth_sd\\[(.*)\\]", "\\1", out$cleaned_parameter)
    out$component[smooth] <- "smooth_sd"
  }

  out
}




.remove_empty_columns_from_pars <- function(x) {
  if (obj_has_name(x, "response") && all(is.na(x$response))) {
    pos <- which(colnames(x) == "response")
    x <- x[, -pos]
  }

  if (obj_has_name(x, "group") && is_empty_string(x$group)) {
    pos <- which(colnames(x) == "group")
    x <- x[, -pos]
  }

  x
}