#' @title Find names of model predictors
#' @name find_predictors
#'
#' @description Returns the names of the predictor variables for the
#'    different parts of a model (like fixed or random effects, zero-inflated
#'    component, ...). Unlike [find_parameters()], the names from
#'    `find_predictors()` match the original variable names from the data
#'    that was used to fit the model.
#'
#' @param x A fitted model.
#' @param effects Should variables for fixed effects, random effects
#'    or both be returned? Only applies to mixed models. May be abbreviated.
#' @param component Should all predictor variables, predictor variables for the
#'   conditional model, the zero-inflated part of the model, the dispersion
#'   term or the instrumental variables be returned? Applies to models
#'   with zero-inflated and/or dispersion formula, or to models with instrumental
#'   variable (so called fixed-effects regressions). May be abbreviated. Note that the
#'   *conditional* component is also called *count* or *mean*
#'   component, depending on the model.
#' @param flatten Logical, if `TRUE`, the values are returned
#'    as character vector, not as list. Duplicated values are removed.
#' @param verbose Toggle warnings.
#' @param ... Currently not used.
#'
#' @return A list of character vectors that represent the name(s) of the
#'    predictor variables. Depending on the combination of the arguments
#'    `effects` and `component`, the returned list has following
#'    elements:
#'    \itemize{
#'      \item `conditional`, the "fixed effects" terms from the model
#'      \item `random`, the "random effects" terms from the model
#'      \item `zero_inflated`, the "fixed effects" terms from the zero-inflation component of the model
#'      \item `zero_inflated_random`, the "random effects" terms from the zero-inflation component of the model
#'      \item `dispersion`, the dispersion terms
#'      \item `instruments`, for fixed-effects regressions like `ivreg`, `felm` or `plm`, the instrumental variables
#'      \item `correlation`, for models with correlation-component like `gls`, the variables used to describe the correlation structure
#'    }
#'
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
#' find_predictors(m)
#' @export
find_predictors <- function(x, ...) {
  UseMethod("find_predictors")
}

#' @rdname find_predictors
#' @export
find_predictors.default <- function(x,
                                    effects = c("fixed", "random", "all"),
                                    component = c("all", "conditional", "zi", "zero_inflated", "dispersion", "instruments", "correlation", "smooth_terms"),
                                    flatten = FALSE,
                                    verbose = TRUE,
                                    ...) {
  effects <- match.arg(effects)
  component <- match.arg(component)

  f <- find_formula(x, verbose = verbose)
  is_mv <- is_multivariate(f)
  elements <- .get_elements(effects, component)


  # filter formulas, depending on requested effects and components
  if (is_mv) {
    f <- lapply(f, function(.x) .prepare_predictors(x, .x, elements))
  } else {
    f <- .prepare_predictors(x, f, elements)
  }

  # random effects are returned as list, so we need to unlist here
  if (is_mv) {
    l <- lapply(f, function(.i) .return_vars(.i, x))
  } else {
    l <- .return_vars(f, x)
  }

  if (.is_empty_object(l) || .is_empty_object(.compact_list(l))) {
    return(NULL)
  }


  # some models, like spatial models, have random slopes that are not defined
  # as fixed effect predictor. In such cases, we have to add the random slope term
  # manually, so other functions like "get_data()" work as expected...

  if (.obj_has_name(l, "random") && effects == "all") {
    random_slope <- unname(unlist(find_random_slopes(x)))
    all_predictors <- unlist(unique(l))
    rs_not_in_pred <- unique(setdiff(random_slope, all_predictors))
    if (length(rs_not_in_pred)) l$random <- c(rs_not_in_pred, l$random)
  }


  if (flatten) {
    unique(unlist(l))
  } else {
    l
  }
}



#' @export
find_predictors.systemfit <- function(x,
                                      flatten = FALSE,
                                      verbose = TRUE,
                                      ...) {

  elements <- .get_elements(effects = "fixed", component = "conditional")

  f <- find_formula(x, verbose = verbose)
  if (is.list(stats::formula(x))) {
    f <- lapply(f, function(.x) .prepare_predictors(x, .x, elements))
  } else {
    f <- .prepare_predictors(x, f, elements)
  }
  l <- .return_vars(f, x)

  if (.is_empty_object(l) || .is_empty_object(.compact_list(l))) {
    return(NULL)
  }

  if (flatten) {
    unique(unlist(l))
  } else {
    l
  }
}



#' @export
find_predictors.afex_aov <- function(x,
                                     effects = c("fixed", "random", "all"),
                                     component = c("all", "conditional", "zi", "zero_inflated", "dispersion", "instruments", "correlation", "smooth_terms"),
                                     flatten = FALSE,
                                     verbose = TRUE,
                                     ...) {
  effects <- match.arg(effects)

  if (effects == "all") effects <- c("fixed", "random")

  l <- list(
    fixed = c(names(attr(x, "between")), names(attr(x, "within"))),
    random = attr(x, "id")
  )[effects]

  if (flatten) {
    unique(unlist(l))
  } else {
    l
  }
}


.return_vars <- function(f, x) {
  l <- lapply(names(f), function(i) {
    if (i %in% c("random", "zero_inflated_random")) {
      unique(paste(unlist(f[[i]])))
    } else if (is.numeric(f[[i]])) {
      f[[i]]
    } else {
      if (is.list(f[[i]])) {
        # this is for multivariate response models, where
        # we have a list of formulas
        lapply(f[[i]], function(j) unique(all.vars(j)))
      } else {
        unique(all.vars(f[[i]]))
      }
    }
  })

  empty_elements <- sapply(l, .is_empty_object)
  l <- .compact_list(l)

  # here we handle special cases for non-linear model in brms
  if (inherits(x, "brmsfit")) {
    nf <- stats::formula(x)
    if (!is.null(attr(nf$formula, "nl", exact = TRUE)) && .obj_has_name(nf, "pforms")) {
      nl_parms <- names(nf$pforms)
      l <- lapply(l, .remove_values, nl_parms)
    }
  }

  # remove constants
  l <- lapply(l, .remove_values, c(".", "pi", "1", "0"))
  l <- lapply(l, .remove_values, c(0, 1))
  l <- lapply(l, function(i) gsub("`", "", i, fixed = TRUE))
  names(l) <- names(f)[!empty_elements]

  l
}


.prepare_predictors <- function(x, f, elements) {
  f <- f[names(f) %in% elements]

  # from conditional model, remove response
  if (.obj_has_name(f, "conditional")) {
    f[["conditional"]] <- f[["conditional"]][[3]]
  }

  # from conditional model, remove response
  if (.obj_has_name(f, "survival")) {
    f[["survival"]] <- f[["survival"]][[3]]
  }

  # if we have random effects, just return grouping variable, not random slopes
  if (.obj_has_name(f, "random")) {
    f[["random"]] <- .get_group_factor(x, f[["random"]])
  }

  # same for zi-random effects
  if (.obj_has_name(f, "zero_inflated_random")) {
    f[["zero_inflated_random"]] <- .get_group_factor(x, f[["zero_inflated_random"]])
  }

  f
}
