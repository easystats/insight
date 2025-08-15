#' @title Extract various information from mixed models
#' @name get_mixed_info
#'
#' @description Small helper function that returns essential information on
#' coefficients, model matrix, variance and correlation parameters, as well as
#' random effects parameters of mixed effects models as list. Mainly used for
#' internal purposes.
#'
#' @param model A mixed effects model.
#' @param verbose Toggle off warnings.
#' @param component For `glmmTMB` and `MixMod` models, this argument specifies
#' the component of the model to extract. Possible values are `"conditional"`
#' (default) and `"zero_inflated"` (or `"zi"`).
#' @param ... Not used.
#'
#' @return This function returns a list that has the same structure for any
#' mixed models with the following components:
#' - `beta` (contains fixed effects, as returned by `lme4::fixef(model)`)
#' - `X` (contains the model matrix, as returned by `lme4::getME(model, "X")`)
#' - `vc` (contains the variance and correlation parameters, as returned by
#'   `lme4::VarCorr(model)`)
#' - `re` (random effects parameters, as returned by `lme4::ranef(model)`)
#'
#' @export
get_mixed_info <- function(model, ...) {
  # sanity check
  if (is.null(model)) {
    return(NULL)
  }
  check_if_installed("lme4")

  UseMethod("get_mixed_info")
}


#' @rdname get_mixed_info
#' @export
get_mixed_info.default <- function(model, verbose = TRUE, ...) {
  mixed_effects_info <- list(
    beta = lme4::fixef(model),
    X = lme4::getME(model, "X"),
    vc = lme4::VarCorr(model),
    re = lme4::ranef(model)
  )

  .fix_mm_rank_deficiency(mixed_effects_info)
}


# helper ---------------------


.fix_mm_rank_deficiency <- function(mixed_effects_info) {
  # fix rank deficiency
  rankdef <- is.na(mixed_effects_info$beta)
  if (any(rankdef)) {
    rankdef_names <- names(mixed_effects_info$beta)[rankdef]
    mixed_effects_info$beta <- mixed_effects_info$beta[setdiff(names(mixed_effects_info$beta), rankdef_names)]
  }
  mixed_effects_info
}


# methods ----------------------------


#' @rdname get_mixed_info
#' @export
get_mixed_info.glmmTMB <- function(model, component = "conditional", verbose = TRUE, ...) {
  check_if_installed("glmmTMB")
  component <- validate_argument(component, c("conditional", "zero_inflated", "zi"))

  mixed_effects_info <- list(
    beta = lme4::fixef(model),
    X = lme4::getME(model, "X"),
    vc = lme4::VarCorr(model),
    re = lme4::ranef(model)
  )

  # extract requested component
  if (component == "conditional") {
    mixed_effects_info <- lapply(mixed_effects_info, .collapse_cond)
  } else {
    mixed_effects_info <- lapply(mixed_effects_info, .collapse_zi)
  }

  # for glmmTMB, tell user that dispersion model is ignored
  if (!is.null(find_formula(model, verbose = FALSE)[["dispersion"]]) && verbose) {
    format_warning("Effects of dispersion model are ignored.")
  }

  .fix_mm_rank_deficiency(mixed_effects_info)
}


#' @export
get_mixed_info.stanreg <- function(model, verbose = TRUE, ...) {
  check_if_installed("rstanarm")

  mixed_effects_info <- list(
    beta = lme4::fixef(model),
    X = rstanarm::get_x(model),
    vc = lme4::VarCorr(model),
    re = lme4::ranef(model)
  )

  .fix_mm_rank_deficiency(mixed_effects_info)
}


#' @export
get_mixed_info.lme <- function(model, verbose = TRUE, ...) {
  check_if_installed("nlme")

  re_names <- find_random(model, split_nested = TRUE, flatten = TRUE)
  comp_x <- get_modelmatrix(model)
  rownames(comp_x) <- seq_len(nrow(comp_x))
  if (.is_nested_lme(model)) {
    vals_vc <- .get_nested_lme_varcorr(model)
    vals_re <- lme4::ranef(model)
  } else {
    vals_vc <- list(nlme::getVarCov(model))
    vals_re <- list(lme4::ranef(model))
  }
  mixed_effects_info <- list(
    beta = lme4::fixef(model),
    X = comp_x,
    vc = vals_vc,
    re = vals_re
  )
  names(mixed_effects_info$re) <- re_names
  names(mixed_effects_info$vc) <- re_names

  .fix_mm_rank_deficiency(mixed_effects_info)
}


#' @export
get_mixed_info.MixMod <- function(model, component = "conditional", verbose = TRUE, ...) {
  check_if_installed("GLMMadaptive")
  component <- validate_argument(component, c("conditional", "zero_inflated", "zi"))

  vc1 <- vc2 <- NULL
  re_names <- find_random(model)

  vc_cond <- !startsWith(colnames(model$D), "zi_")
  if (any(vc_cond)) {
    vc1 <- model$D[vc_cond, vc_cond, drop = FALSE]
    attr(vc1, "stddev") <- sqrt(diag(vc1))
    attr(vc1, "correlation") <- stats::cov2cor(model$D[vc_cond, vc_cond, drop = FALSE])
  }

  vc_zi <- startsWith(colnames(model$D), "zi_")
  if (any(vc_zi)) {
    colnames(model$D) <- gsub("^zi_(.*)", "\\1", colnames(model$D))
    rownames(model$D) <- colnames(model$D)
    vc2 <- model$D[vc_zi, vc_zi, drop = FALSE]
    attr(vc2, "stddev") <- sqrt(diag(vc2))
    attr(vc2, "correlation") <- stats::cov2cor(model$D[vc_zi, vc_zi, drop = FALSE])
  }

  model_deviance <- get_deviance(model, verbose = FALSE)
  residual_df <- get_df(model, type = "residual", verbose = FALSE)

  vc1 <- list(vc1)
  names(vc1) <- re_names[[1]]
  attr(vc1, "sc") <- sqrt(abs(model_deviance) / residual_df)
  attr(vc1, "useSc") <- TRUE

  if (!is.null(vc2)) {
    vc2 <- list(vc2)
    names(vc2) <- re_names[[2]]
    attr(vc2, "sc") <- sqrt(abs(model_deviance) / residual_df)
    attr(vc2, "useSc") <- FALSE
  }

  vcorr <- compact_list(list(vc1, vc2))
  names(vcorr) <- c("cond", "zi")[seq_along(vcorr)]

  mixed_effects_info <- list(
    beta = lme4::fixef(model),
    X = get_modelmatrix(model),
    vc = vcorr,
    re = list(lme4::ranef(model))
  )
  names(mixed_effects_info$re) <- model$id_name

  # extract requested component
  if (component == "conditional") {
    mixed_effects_info <- lapply(mixed_effects_info, .collapse_cond)
  } else {
    mixed_effects_info <- lapply(mixed_effects_info, .collapse_zi)
  }

  # for glmmTMB, tell user that dispersion model is ignored
  if (!is.null(find_formula(model, verbose = FALSE)[["dispersion"]]) && verbose) {
    format_warning("Effects of dispersion model are ignored.")
  }

  .fix_mm_rank_deficiency(mixed_effects_info)
}


#' @export
get_mixed_info.mjoint <- function(model, verbose = TRUE, ...) {
  re_names <- find_random(model, flatten = TRUE)
  vcorr <- summary(model)$D
  attr(vcorr, "stddev") <- sqrt(diag(vcorr))
  attr(vcorr, "correlation") <- stats::cov2cor(vcorr)
  vcorr <- list(vcorr)
  names(vcorr) <- re_names[1]
  attr(vcorr, "sc") <- model$coef$sigma2[[1]]
  attr(vcorr, "useSc") <- TRUE

  mixed_effects_info <- list(
    beta = lme4::fixef(model),
    X = matrix(1, nrow = n_obs(model), dimnames = list(NULL, "(Intercept)_1")),
    vc = vcorr,
    re = list(lme4::ranef(model))
  )
  names(mixed_effects_info$re) <- re_names[seq_along(mixed_effects_info$re)]

  .fix_mm_rank_deficiency(mixed_effects_info)
}


#' @export
get_mixed_info.clmm <- function(model, verbose = TRUE, ...) {
  check_if_installed("ordinal")

  mm <- get_modelmatrix(model)
  mixed_effects_info <- list(
    beta = c("(Intercept)" = 1, stats::coef(model)[intersect(names(stats::coef(model)), colnames(mm))]),
    X = mm,
    vc = ordinal::VarCorr(model),
    re = ordinal::ranef(model)
  )
  attr(mixed_effects_info$vc, "useSc") <- FALSE

  .fix_mm_rank_deficiency(mixed_effects_info)
}


#' @export
get_mixed_info.glmmadmb <- function(model, verbose = TRUE, ...) {
  check_if_installed("glmmadmb")

  mixed_effects_info <- list(
    beta = lme4::fixef(model),
    X = get_modelmatrix(model),
    vc = lme4::VarCorr(model),
    re = lme4::ranef(model)
  )

  .fix_mm_rank_deficiency(mixed_effects_info)
}


#' @export
get_mixed_info.brmsfit <- function(model, verbose = TRUE, ...) {
  check_if_installed("brms")

  comp_x <- get_modelmatrix(model)
  rownames(comp_x) <- seq_len(nrow(comp_x))
  vc <- lapply(names(lme4::VarCorr(model)), function(i) {
    element <- lme4::VarCorr(model)[[i]]
    if (i != "residual__") {
      if (is.null(element$cov)) {
        out <- as.matrix(drop(element$sd[, 1])^2)
        colnames(out) <- rownames(out) <- gsub("Intercept", "(Intercept)", rownames(element$sd), fixed = TRUE)
      } else {
        out <- as.matrix(drop(element$cov[, 1, ]))
        colnames(out) <- rownames(out) <- gsub("Intercept", "(Intercept)", rownames(element$cov), fixed = TRUE)
      }
      attr(out, "sttdev") <- element$sd[, 1]
    } else {
      out <- NULL
    }
    out
  })
  vc <- compact_list(vc)
  names(vc) <- setdiff(names(lme4::VarCorr(model)), "residual__")
  attr(vc, "sc") <- lme4::VarCorr(model)$residual__$sd[1, 1]
  fixef_params <- lme4::fixef(model)[, 1]
  # remove sigma parameters
  fixef_params <- fixef_params[!startsWith(names(fixef_params), "sigma_")]
  mixed_effects_info <- list(
    beta = fixef_params,
    X = comp_x,
    vc = vc,
    re = lapply(lme4::ranef(model), function(re) {
      reval <- as.data.frame(drop(re[, 1, ]))
      colnames(reval) <- gsub("Intercept", "(Intercept)", dimnames(re)[[3]], fixed = TRUE)
      reval
    })
  )
  names(mixed_effects_info$beta) <- gsub("Intercept", "(Intercept)", names(mixed_effects_info$beta), fixed = TRUE)

  .fix_mm_rank_deficiency(mixed_effects_info)
}


#' @export
get_mixed_info.cpglmm <- function(model, verbose = TRUE, ...) {
  check_if_installed("cplm")

  mixed_effects_info <- list(
    beta = cplm::fixef(model),
    X = cplm::model.matrix(model),
    vc = cplm::VarCorr(model),
    re = cplm::ranef(model)
  )
  attr(mixed_effects_info$vc, "useSc") <- FALSE

  .fix_mm_rank_deficiency(mixed_effects_info)
}


#' @export
get_mixed_info.coxme <- function(model, verbose = TRUE, ...) {
  check_if_installed("coxme")

  mixed_effects_info <- list(
    beta = coxme::fixef(model),
    X = get_modelmatrix(model),
    vc = coxme::VarCorr(model),
    re = coxme::ranef(model)
  )

  # make sure we always have a matrix
  mixed_effects_info$vc <- lapply(mixed_effects_info$vc, .fix_dimnames)
  mixed_effects_info$re <- lapply(mixed_effects_info$re, .fix_dimnames)

  # need specific class attribute for coxme, because it has a different structure
  class(mixed_effects_info$vc) <- "VarCorr.coxme"

  .fix_mm_rank_deficiency(mixed_effects_info)
}


# helper --------------------

.fix_dimnames <- function(x) {
  if (!is.matrix(x)) {
    x <- as.matrix(x)
  }
  if (is.null(rownames(x))) {
    rownames(x) <- character(nrow(x))
  }
  if (is.null(colnames(x))) {
    colnames(x) <- character(ncol(x))
  }
  rownames(x)[1] <- "(Intercept)"
  colnames(x)[1] <- "(Intercept)"
  x
}
