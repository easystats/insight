#' @title Get clean names of model parameters
#' @name clean_parameters
#'
#' @description This function "cleans" names of model parameters by removing
#' patterns like \code{"r_"} or \code{"b[]"} (mostly applicable to Stan models)
#' and adding columns with information to which group or component parameters
#' belong (i.e. fixed or random, count or zero-inflated...)
#' \cr \cr
#' The main purpose of this function is to easily filter and select model parameters,
#' in particular of - but not limited to - posterior samples from Stan models,
#' depending on certain characteristics. This might be useful when only selective
#' results should be reported or results from all parameters should be filtered
#' to return only certain results (see \code{\link{print_parameters}}).
#'
#' @param x A fitted model.
#' @inheritParams find_parameters
#'
#' @return A data frame with "cleaned" parameter names and information on
#'   effects, component and group where parameters belong to. To be consistent
#'   across different models, the returned data frame always has at least four
#'   columns \code{Parameter}, \code{Effects}, \code{Component} and
#'   \code{Cleaned_Parameter}. See 'Details'.
#'
#' @details The \code{Effects} column indicate if a parameter is a \emph{fixed}
#' or \emph{random} effect. The \code{Component} can either be \emph{conditional}
#' or \emph{zero_inflated}. For models with random effects, the \code{Group}
#' column indicates the grouping factor of the random effects. For multivariate
#' response models from \pkg{brms} or \pkg{rstanarm}, an additional \emph{Response}
#' column is included, to indicate which parameters belong to which response
#' formula. Furthermore, \emph{Cleaned_Parameter} column is returned that
#' contains "human readable" parameter names (which are mostly identical to
#' \code{Parameter}, except for for models from \pkg{brms} or \pkg{rstanarm},
#' or for specific terms like smooth- or spline-terms).
#'
#' @examples
#' \dontrun{
#' library(brms)
#' model <- download_model("brms_zi_2")
#' clean_parameters(model)
#' }
#' @export
clean_parameters <- function(x, ...) {
  UseMethod("clean_parameters")
}



#' @export
clean_parameters.default <- function(x, group = "", ...) {
  pars <- find_parameters(x,
    effects = "all",
    component = "all",
    flatten = FALSE
  )

  l <- lapply(names(pars), function(i) {
    eff <- if (grepl("random", i, fixed = TRUE)) {
      "random"
    } else {
      "fixed"
    }

    com <- if (grepl("zero_inflated", i, fixed = TRUE)) {
      "zero_inflated"
    } else if (grepl("dispersion", i, fixed = TRUE)) {
      "dispersion"
    } else if (grepl("nonlinear", i, fixed = TRUE)) {
      "nonlinear"
    } else if (grepl("instruments", i, fixed = TRUE)) {
      "instruments"
    } else if (grepl("extra", i, fixed = TRUE)) {
      "extra"
    } else if (grepl("scale", i, fixed = TRUE)) {
      "scale"
    } else if (grepl("marginal", i, fixed = TRUE)) {
      "marginal"
    } else if (grepl("intercept", i, fixed = TRUE)) {
      "intercept"
    } else if (grepl("correlation", i, fixed = TRUE)) {
      "correlation"
    } else {
      "conditional"
    }

    fun <- if (grepl("smooth", i, fixed = TRUE)) {
      "smooth"
    } else {
      ""
    }

    if (eff == "random") {
      rand_eff <- lapply(names(pars[[i]]), function(j) {
        data.frame(
          Parameter = pars[[i]][[j]],
          Effects = eff,
          Component = com,
          Group = j,
          Function = fun,
          Cleaned_Parameter = clean_names(pars[[i]][[j]]),
          stringsAsFactors = FALSE,
          row.names = NULL
        )
      })
      do.call(rbind, rand_eff)
    } else {
      data.frame(
        Parameter = pars[[i]],
        Effects = eff,
        Component = com,
        Group = group,
        Function = fun,
        Cleaned_Parameter = clean_names(pars[[i]]),
        stringsAsFactors = FALSE,
        row.names = NULL
      )
    }
  })

  out <- .remove_backticks_from_parameter_names(do.call(rbind, l))
  out <- .remove_empty_columns_from_pars(out)
  .fix_random_effect_smooth(x, out)
}



#' @export
clean_parameters.emmGrid <- function(x, ...) {
  pars <- find_parameters(x, flatten = FALSE)

  l <- lapply(names(pars), function(i) {
    data.frame(
      Parameter = pars[[i]],
      Component = i,
      Cleaned_Parameter = .clean_names(x = pars[[i]], is_emmeans = TRUE),
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  })

  out <- .remove_backticks_from_parameter_names(do.call(rbind, l))
  out <- .remove_empty_columns_from_pars(out)
  out
}


#' @export
clean_parameters.emm_list <- clean_parameters.emmGrid


#' @export
clean_parameters.BFBayesFactor <- function(x, ...) {
  pars <- find_parameters(x,
    effects = "all",
    component = "all",
    flatten = FALSE
  )

  l <- lapply(names(pars), function(i) {
    eff <- if (grepl("random", i, fixed = TRUE)) {
      "random"
    } else {
      "fixed"
    }

    com <- if (grepl("extra", i, fixed = TRUE)) {
      "extra"
    } else {
      "conditional"
    }

    data.frame(
      Parameter = pars[[i]],
      Effects = eff,
      Component = com,
      Cleaned_Parameter = pars[[i]],
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  })

  out <- .remove_backticks_from_parameter_names(do.call(rbind, l))
  out <- .remove_empty_columns_from_pars(.clean_bfbayesfactor_params(out))
  .fix_random_effect_smooth(x, out)
}



#' @export
clean_parameters.wbm <- function(x, ...) {
  pars <- find_parameters(x,
    effects = "all",
    component = "all",
    flatten = FALSE
  )

  l <- lapply(names(pars), function(i) {
    com <- if (grepl("random", i, fixed = TRUE)) {
      "interactions"
    } else if (grepl("instruments", i, fixed = TRUE)) {
      "instruments"
    } else {
      "conditional"
    }

    fun <- if (grepl("smooth", i, fixed = TRUE)) {
      "smooth"
    } else {
      ""
    }

    data.frame(
      Parameter = pars[[i]],
      Effects = "fixed",
      Component = com,
      Group = "",
      Function = fun,
      Cleaned_Parameter = clean_names(pars[[i]]),
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  })

  out <- .remove_backticks_from_parameter_names(do.call(rbind, l))
  out <- .remove_empty_columns_from_pars(out)
  .fix_random_effect_smooth(x, out)
}


#' @export
clean_parameters.wbgee <- clean_parameters.wbm


#' @export
clean_parameters.merModList <- function(x, ...) {
  clean_parameters.default(x[[1]], ...)
}


#' @export
clean_parameters.model_fit <- function(x, ...) {
  clean_parameters(x$fit, ...)
}


#' @export
clean_parameters.glmm <- function(x, ...) {
  pars <- find_parameters(x,
    effects = "all",
    component = "all",
    flatten = FALSE
  )

  l <- lapply(names(pars), function(i) {
    data.frame(
      Parameter = pars[[i]],
      Effects = i,
      Component = "",
      Group = "",
      Function = "",
      Cleaned_Parameter = clean_names(pars[[i]]),
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  })

  out <- .remove_backticks_from_parameter_names(do.call(rbind, l))
  .remove_empty_columns_from_pars(out)
}


#' @export
clean_parameters.MCMCglmm <- clean_parameters.glmm


#' @export
clean_parameters.lavaan <- function(x, ...) {
  params <- get_parameters(x)

  data.frame(
    Parameter = params$Parameter,
    Component = params$Component,
    Group = "",
    Function = "",
    Cleaned_Parameter = params$Parameter,
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}



#' @export
clean_parameters.blavaan <- function(x, ...) {
  params <- get_parameters(x, summary = TRUE)
  params$Estimate <- NULL
  params$Group <- ""
  params$Function <- ""
  params$Cleaned_Parameter <- params$Parameter
  params
}



#' @export
clean_parameters.brmsfit <- function(x, ...) {
  pars <- find_parameters(x,
    effects = "all",
    component = "all",
    flatten = FALSE
  )

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
  out <- .remove_empty_columns_from_pars(.clean_brms_params(out, is_mv))
  .fix_random_effect_smooth(x, out)
}


#' @export
clean_parameters.stanreg <- function(x, ...) {
  pars <- find_parameters(x,
    effects = "all",
    component = "all",
    flatten = FALSE
  )

  l <- .get_stan_params(pars)

  out <- do.call(rbind, l)
  out <- .remove_empty_columns_from_pars(.clean_stanreg_params(out))
  .fix_random_effect_smooth(x, out)
}



#' @export
clean_parameters.bamlss <- function(x, ...) {
  pars <- find_parameters(x, effects = "all", component = "all", flatten = FALSE)
  l <- .get_stan_params(pars)

  out <- do.call(rbind, l)
  out <- .remove_empty_columns_from_pars(.clean_bamlss_params(out))
  class(out) <- c("clean_parameters", class(out))
  out
}



#' @export
clean_parameters.stanmvreg <- function(x, ...) {
  pars <- find_parameters(x, effects = "all", component = "all", flatten = FALSE)

  l <- do.call(
    rbind,
    lapply(names(pars), function(i) .get_stan_params(pars[[i]], response = i))
  )

  out <- do.call(rbind, l)
  out <- .remove_empty_columns_from_pars(.clean_stanreg_params(out))
  .fix_random_effect_smooth(x, out)
}


#' @export
clean_parameters.stanfit <- clean_parameters.stanreg


#' @export
clean_parameters.aovlist <- function(x, ...) {
  pars <- get_parameters(x)
  clean_parameters.default(x, group = pars$Group)
}


#' @export
clean_parameters.afex_aov <- function(x, ...) {
  if (!is.null(x$aov)) {
    clean_parameters(x$aov, ...)
  } else {
    clean_parameters(x$lm, ...)
  }
}


#' @export
clean_parameters.mlm <- function(x, ...) {
  pars <- find_parameters(x, effects = "all", component = "all", flatten = FALSE)

  l <- lapply(names(pars), function(i) {
    eff <- "fixed"
    com <- "conditional"
    data.frame(
      Parameter = pars[[i]]$conditional,
      Effects = eff,
      Component = com,
      Response = i,
      Cleaned_Parameter = clean_names(pars[[i]]$conditional),
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  })

  out <- .remove_backticks_from_parameter_names(do.call(rbind, l))
  out <- .remove_empty_columns_from_pars(out)
  .fix_random_effect_smooth(x, out)
}



# helper -------------------------


.get_stan_params <- function(pars, response = NA) {
  lapply(names(pars), function(i) {
    eff <- if (grepl("random", i, fixed = TRUE)) {
      "random"
    } else {
      "fixed"
    }

    com <- if (grepl("zero_inflated", i, fixed = TRUE)) {
      "zero_inflated"
    } else if (grepl("sigma", i, fixed = TRUE)) {
      "sigma"
    } else if (grepl("priors", i, fixed = TRUE)) {
      "priors"
    } else if (grepl("smooth_terms", i, fixed = TRUE)) {
      "smooth_terms"
    } else if (grepl("alpha", i, fixed = TRUE)) {
      "distributional"
    } else if (grepl("beta", i, fixed = TRUE)) {
      "distributional"
    } else if (grepl("mix", i, fixed = TRUE)) {
      "distributional"
    } else if (grepl("shiftprop", i, fixed = TRUE)) {
      "distributional"
    } else if (grepl("shape", i, fixed = TRUE)) {
      "distributional"
    } else if (grepl("auxiliary", i, fixed = TRUE)) {
      "distributional"
    } else if (grepl("dispersion", i, fixed = TRUE)) {
      "dispersion"
    } else {
      "conditional"
    }

    fun <- if (grepl("smooth", i, fixed = TRUE)) {
      "smooth"
    } else {
      ""
    }


    data.frame(
      Parameter = pars[[i]],
      Effects = eff,
      Component = com,
      Group = "",
      Response = response,
      Function = fun,
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  })
}



.clean_brms_params <- function(out, is_mv) {
  out$Cleaned_Parameter <- out$Parameter

  # for multivariate response models, remove responses from parameter names

  if (is_mv) {
    resp <- unique(out$Response)

    resp_pattern <- sprintf("_%s_(.*)", resp)
    for (i in resp_pattern) {
      out$Cleaned_Parameter <- gsub(pattern = i, "_\\1", out$Cleaned_Parameter, perl = TRUE)
    }

    resp_pattern <- sprintf("__%s(.*)", resp)
    for (i in resp_pattern) {
      out$Cleaned_Parameter <- gsub(pattern = i, "\\1", out$Cleaned_Parameter, perl = TRUE)
    }

    resp_pattern <- sprintf("__zi_%s(.*)", resp)
    for (i in resp_pattern) {
      out$Cleaned_Parameter <- gsub(pattern = i, "\\1", out$Cleaned_Parameter, perl = TRUE)
    }

    resp_pattern <- sprintf("(sigma)(_%s)", resp)
    for (i in resp_pattern) {
      out$Cleaned_Parameter <- gsub(pattern = i, "\\1", out$Cleaned_Parameter, perl = TRUE)
    }
  }


  smooth_function <- grepl(pattern = "(bs_|bs_zi_)", out$Cleaned_Parameter)
  if (any(smooth_function)) {
    out$Function[smooth_function] <- "smooth"
  }


  # clean fixed effects, conditional and zero-inflated

  out$Cleaned_Parameter <- gsub(pattern = "^b_(?!zi_)(.*)\\.(\\d)\\.$", "\\1[\\2]", out$Cleaned_Parameter, perl = TRUE)
  out$Cleaned_Parameter <- gsub(pattern = "^b_zi_(.*)\\.(\\d)\\.$", "\\1[\\2]", out$Cleaned_Parameter, perl = TRUE)
  out$Cleaned_Parameter <- gsub(pattern = "^(b_|bs_|bsp_|bcs_)(?!zi_)(.*)", "\\2", out$Cleaned_Parameter, perl = TRUE)
  out$Cleaned_Parameter <- gsub(pattern = "^(b_zi_|bs_zi_|bsp_zi_|bcs_zi_)(.*)", "\\2", out$Cleaned_Parameter, perl = TRUE)

  # correlation and sd

  cor_sd <- grepl("(sd_|cor_)(.*)", out$Cleaned_Parameter)
  if (any(cor_sd)) {
    out$Cleaned_Parameter[cor_sd] <- gsub("^(sd_|cor_)(.*?)__(.*)", "\\3", out$Parameter[cor_sd], perl = TRUE)
    out$Group[cor_sd] <- paste("SD/Cor:", gsub("^(sd_|cor_)(.*?)__(.*)", "\\2", out$Parameter[cor_sd], perl = TRUE))
    # replace "__" by "~"
    cor_only <- grepl("^cor_", out$Parameter[cor_sd])
    if (any(cor_only)) {
      out$Cleaned_Parameter[which(cor_sd)[cor_only]] <- sub("__", " ~ ", out$Cleaned_Parameter[which(cor_sd)[cor_only]])
    }
  }

  # extract group-names from random effects and clean random effects

  rand_eff <- grepl("^r_(.*)\\.(.*)\\.", out$Cleaned_Parameter)
  if (any(rand_eff)) {
    r_pars <- gsub("^r_(.*)\\.(.*)\\.", "\\1", out$Cleaned_Parameter[rand_eff])
    r_grps <- gsub("^r_(.*)\\.(.*)\\.", "\\2", out$Cleaned_Parameter[rand_eff])
    r_pars <- gsub("__zi", "", r_pars)
    r_grps <- sprintf("%s: %s", r_grps, gsub("(.*)\\.(.*)", "\\1", r_pars))

    out$Cleaned_Parameter[rand_eff] <- r_pars
    out$Group[rand_eff] <- r_grps
  }

  # clean remaining parameters

  priors <- grepl("^prior_", out$Cleaned_Parameter)
  if (length(priors)) {
    out$Cleaned_Parameter <- gsub("^prior_", "", out$Cleaned_Parameter)
    out$Component[priors] <- "priors"
  }

  simplex <- grepl("^simo_", out$Cleaned_Parameter)
  if (length(simplex)) {
    out$Cleaned_Parameter[simplex] <- gsub("^(simo_|simo_mo)(.*)\\.(\\d)\\.$", "\\2[\\3]", out$Cleaned_Parameter[simplex])
    out$Component[simplex] <- "simplex"
  }

  smooth <- grepl("^sds_", out$Cleaned_Parameter)
  if (length(smooth)) {
    out$Cleaned_Parameter <- gsub("^sds_", "", out$Cleaned_Parameter)
    out$Component[smooth] <- "smooth_sd"
    out$Function[smooth] <- "smooth"
  }

  # fix intercept names

  intercepts <- which(out$Cleaned_Parameter %in% c("Intercept", "zi_Intercept"))

  if (!.is_empty_object(intercepts)) {
    out$Cleaned_Parameter[intercepts] <- "(Intercept)"
  }

  interaction_terms <- which(grepl("\\.", out$Cleaned_Parameter))

  if (length(interaction_terms)) {
    for (i in interaction_terms) {
      i_terms <- strsplit(out$Cleaned_Parameter[i], "\\.")
      find_i_terms <- sapply(i_terms, function(j) j %in% out$Cleaned_Parameter)
      if (all(find_i_terms)) {
        out$Cleaned_Parameter[i] <- gsub("\\.", ":", out$Cleaned_Parameter[i])
      }
    }
  }

  out
}



.clean_stanreg_params <- function(out) {
  out$Cleaned_Parameter <- out$Parameter

  # extract group-names from random effects and clean random effects

  rand_intercepts <- grepl("^b\\[\\(Intercept\\)", out$Cleaned_Parameter)

  if (any(rand_intercepts)) {
    re_grp_level <- gsub("b\\[(.*) (.*):(.*)\\]", "\\2", out$Cleaned_Parameter[rand_intercepts])
    out$Cleaned_Parameter[rand_intercepts] <- gsub(
      "b\\[\\(Intercept\\) (.*)\\]",
      "\\1",
      out$Cleaned_Parameter[rand_intercepts]
    )
    out$Group[rand_intercepts] <- sprintf("Intercept: %s", re_grp_level)
  }


  # correlation and sd

  cor_sd <- grepl("^Sigma\\[(.*)", out$Cleaned_Parameter)

  if (any(cor_sd)) {
    parm1 <- gsub("^Sigma\\[(.*):(.*),(.*)\\]", "\\2", out$Parameter[cor_sd], perl = TRUE)
    parm2 <- gsub("^Sigma\\[(.*):(.*),(.*)\\]", "\\3", out$Parameter[cor_sd], perl = TRUE)
    out$Cleaned_Parameter[which(cor_sd)] <- parm1
    rand_cor <- parm1 != parm2
    if (any(rand_cor)) {
      out$Cleaned_Parameter[which(cor_sd)[rand_cor]] <- paste0(parm1[rand_cor], " ~ ", parm2[rand_cor])
    }
    out$Group[cor_sd] <- paste("SD/Cor:", gsub("^Sigma\\[(.*):(.*),(.*)\\]", "\\1", out$Parameter[cor_sd], perl = TRUE))
  }


  # extract group-names from random effects and clean random effects

  rand_effects <- grepl("^b\\[", out$Cleaned_Parameter)

  if (any(rand_effects)) {
    re_grp_level <- gsub("b\\[(.*) (.*):(.*)\\]", "\\2", out$Cleaned_Parameter[rand_effects])
    r_grps <- gsub("b\\[(.*) (.*)\\]", "\\1", out$Cleaned_Parameter[rand_effects])
    r_pars <- gsub("b\\[(.*) (.*)\\]", "\\2", out$Cleaned_Parameter[rand_effects])

    out$Group[rand_effects] <- sprintf("%s: %s", r_grps, re_grp_level)
    out$Cleaned_Parameter[rand_effects] <- r_pars
  }

  # clean remaining parameters

  smooth <- grepl("^smooth_sd\\[", out$Cleaned_Parameter)

  if (length(smooth)) {
    out$Cleaned_Parameter <- gsub("^smooth_sd\\[(.*)\\]", "\\1", out$Cleaned_Parameter)
    out$Component[smooth] <- "smooth_sd"
    out$Function[smooth] <- "smooth"
  }

  out
}



.clean_bfbayesfactor_params <- function(out) {
  pars <- do.call(rbind, strsplit(out$Parameter, "-", TRUE))

  if (ncol(pars) == 1) {
    return(out)
  }

  out$Cleaned_Parameter <- tryCatch(
    {
      apply(pars, 1, function(i) {
        if (i[1] == i[2]) {
          i[2] <- ""
        } else if (i[1] != i[2] && !grepl(":", i[1], fixed = TRUE)) {
          i[1] <- paste0(i[1], " [", i[2], "]")
          i[2] <- ""
        } else if (grepl(":", i[1], fixed = TRUE)) {
          f <- unlist(strsplit(i[1], ":", fixed = TRUE))
          l <- unlist(strsplit(i[2], ".&.", fixed = TRUE))

          matches <- stats::na.omit(match(f, l))
          l[matches] <- ""
          l[-matches] <- paste0("[", l[-matches], "]")
          i[1] <- paste0(f, l, collapse = " * ")
        }
        as.vector(i[1])
      })
    },
    error = function(e) {
      out$Cleaned_Parameter
    }
  )

  out
}



.clean_bamlss_params <- function(out) {
  out$Cleaned_Parameter <- out$Parameter
  out$Cleaned_Parameter <- gsub("^(mu\\.p\\.|pi\\.p\\.)(.*)", "\\2", out$Cleaned_Parameter)
  out$Cleaned_Parameter <- gsub("^(mu\\.s\\.|pi\\.s\\.)(.*)", "s\\(\\2\\)", out$Cleaned_Parameter)
  out$Cleaned_Parameter <- gsub("^sigma\\.p\\.(.*)", "sigma \\(\\1\\)", out$Cleaned_Parameter)
  out$Cleaned_Parameter <- gsub("..", ".", out$Cleaned_Parameter, fixed = TRUE)
  out$Cleaned_Parameter <- gsub(".)", ")", out$Cleaned_Parameter, fixed = TRUE)
  out$Cleaned_Parameter <- gsub("(.", "(", out$Cleaned_Parameter, fixed = TRUE)
  out$Cleaned_Parameter <- gsub(".Intercept.", "Intercept", out$Cleaned_Parameter, fixed = TRUE)
  out$Cleaned_Parameter <- gsub("(\\.$)", "", out$Cleaned_Parameter)
  out
}



.remove_empty_columns_from_pars <- function(x) {
  if (.obj_has_name(x, "Response") && all(is.na(x$Response))) {
    pos <- which(colnames(x) == "Response")
    x <- x[, -pos]
  }

  if (.obj_has_name(x, "Group") && .is_empty_string(x$Group)) {
    pos <- which(colnames(x) == "Group")
    x <- x[, -pos]
  }

  if (.obj_has_name(x, "Function") && .is_empty_string(x$Function)) {
    pos <- which(colnames(x) == "Function")
    x <- x[, -pos]
  }

  x
}


# Fix random effects assignment for smooth terms
#
# This function checks whether smooth terms were used as random effects,
# (i.e. s(term, by="re")) and if so, the value in the "effecs" column will
# be set to "random".
#
.fix_random_effect_smooth <- function(x, out) {
  if ("Function" %in% colnames(out) && "smooth" %in% out$Function) {
    vars <- find_terms(x)$conditional
    vars <- gsub(" ", "", vars, fixed = TRUE)
    random_smooth_terms <- grepl("^s\\((.*)(bs=\"re\"+)\\)", x = vars)
    if (any(random_smooth_terms)) {
      random_term <- paste0(
        "s(",
        gsub("^s\\(([^,]*)(.*)(bs=\"re\"+)\\)", "\\1", vars[random_smooth_terms]),
        ")"
      )
      out$Effects[which(out$Parameter == random_term)] <- "random"
    }
  }

  class(out) <- c("clean_parameters", class(out))
  out
}
