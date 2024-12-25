#' @title Find names of model predictors
#' @name find_predictors
#'
#' @description Returns the names of the predictor variables for the different
#' parts of a model (like fixed or random effects, zero-inflated component,
#' ...). Unlike [`find_parameters()`], the names from `find_predictors()` match
#' the original variable names from the data that was used to fit the model.
#'
#' @param x A fitted model.
#' @param effects Should variables for fixed effects (`"fixed"`), random effects
#' (`"random"`) or both (`"all"`) be returned? Only applies to mixed models. May
#' be abbreviated.
#' @param component Which type of parameters to return, such as parameters for
#' the conditional model, the zero-inflated part of the model, the dispersion
#' term, the instrumental variables or marginal effects be returned? Applies to
#' models with zero-inflated and/or dispersion formula, or to models with
#' instrumental variables (so called fixed-effects regressions), or models with
#' marginal effects (from **mfx**). See details in section _Model Components_
#' .May be abbreviated. Note that the *conditional* component also refers to the
#' *count* or *mean* component - names may differ, depending on the modeling
#' package. There are three convenient shortcuts (not applicable to *all* model
#' classes):
#' - `component = "all"` returns all possible parameters.
#' - If `component = "location"`, location parameters such as `conditional`,
#'   `zero_inflated`, `smooth_terms`, or `instruments` are returned (everything
#'   that are fixed or random effects - depending on the `effects` argument -
#'   but no auxiliary parameters).
#' - For `component = "distributional"` (or `"auxiliary"`), components like
#'   `sigma`, `dispersion`, `beta` or `precision` (and other auxiliary
#'   parameters) are returned.
#' @param flatten Logical, if `TRUE`, the values are returned as character
#' vector, not as list. Duplicated values are removed.
#' @param verbose Toggle warnings.
#' @param ... Currently not used.
#'
#' @section Model components:
#' Possible values for the `component` argument depend on the model class.
#' Following are valid options:
#' - `"all"`: returns all model components, applies to all models, but will only
#'   have an effect for models with more than just the conditional model component.
#' - `"conditional"`: only returns the conditional component, i.e. "fixed effects"
#'   terms from the model. Will only have an effect for models with more than
#'   just the conditional model component.
#' - `"smooth_terms"`: returns smooth terms, only applies to GAMs (or similar
#'   models that may contain smooth terms).
#' - `"zero_inflated"` (or `"zi"`): returns the zero-inflation component.
#' - `"dispersion"`: returns the dispersion model component. This is common
#'   for models with zero-inflation or that can model the dispersion parameter.
#' - `"instruments"`: for instrumental-variable or some fixed effects regression,
#'   returns the instruments.
#' - `"nonlinear"`: for non-linear models (like models of class `nlmerMod` or
#'   `nls`), returns staring estimates for the nonlinear parameters.
#' - `"correlation"`: for models with correlation-component, like `gls`, the
#'   variables used to describe the correlation structure are returned.
#' - `"location"`: returns location parameters such as `conditional`,
#'   `zero_inflated`, `smooth_terms`, or `instruments` (everything that are
#'   fixed or random effects - depending on the `effects` argument - but no
#'   auxiliary parameters).
#' - `"distributional"` (or `"auxiliary"`): components like `sigma`, `dispersion`,
#'   `beta` or `precision` (and other auxiliary parameters) are returned.
#'
#' **Special models**
#'
#' Some model classes also allow rather uncommon options. These are:
#' - **mhurdle**: `"infrequent_purchase"`, `"ip"`, and `"auxiliary"`
#' - **BGGM**: `"correlation"` and `"intercept"`
#' - **BFBayesFactor**, **glmx**: `"extra"`
#' - **averaging**:`"conditional"` and `"full"`
#' - **mjoint**: `"survival"`
#' - **mfx**: `"precision"`, `"marginal"`
#' - **betareg**, **DirichletRegModel**: `"precision"`
#' - **mvord**: `"thresholds"` and `"correlation"`
#' - **clm2**: `"scale"`
#' - **selection**: `"selection"`, `"outcome"`, and `"auxiliary"`
#'
#' For models of class `brmsfit` (package **brms**), even more options are
#' possible for the `component` argument, which are not all documented in detail
#' here.
#'
#' @section Parameters, Variables, Predictors and Terms:
#' There are four functions that return information about the variables in a
#' model: `find_predictors()`, `find_variables()`, `find_terms()` and
#' `find_parameters()`. There are some differences between those functions,
#' which are explained using following model. Note that some, but not all of
#' those functions return information about the *dependent* and *independent*
#' variables. In this example, we only show the differences for the independent
#' variables.
#' ```
#' model <- lm(mpg ~ factor(gear), data = mtcars)
#' ```
#' - `find_terms(model)` returns the model terms, i.e. how the variables were
#'   used in the model, e.g. applying transformations like `factor()`, `poly()`
#'   etc. `find_terms()` may return a variable name multiple times in case of
#'   multiple transformations. The return value would be `"factor(gear)"`.
#' - `find_parameters(model)` returns the names of the model parameters
#'   (coefficients). The return value would be `"(Intercept)"`, `"factor(gear)4"`
#'   and `"factor(gear)5"`.
#' - `find_variables()` returns the original variable names. `find_variables()`
#'   returns each variable name only once. The return value would be `"gear"`.
#' - `find_predictors()` is comparable to `find_variables()` and also returns
#'   the original variable names, but excluded the *dependent* (response)
#'   variables. The return value would be `"gear"`.
#'
#' @return A list of character vectors that represent the name(s) of the
#' predictor variables. Depending on the combination of the arguments
#' `effects` and `component`, the returned list can have following elements:
#'
#' - `conditional`, the "fixed effects" terms from the model
#' - `random`, the "random effects" terms from the model
#' - `zero_inflated`, the "fixed effects" terms from the zero-inflation
#'   component of the model
#' - `zero_inflated_random`, the "random effects" terms from the zero-inflation
#'   component of the model
#' - `dispersion`, the dispersion terms
#' - `instruments`, for fixed-effects regressions like `ivreg`, `felm` or `plm`,
#'   the instrumental variables
#' - `correlation`, for models with correlation-component like `gls`, the
#'   variables used to describe the correlation structure
#' - `nonlinear`, for non-linear models (like models of class `nlmerMod` or
#'   `nls`), the staring estimates for the nonlinear parameters
#' - `smooth_terms` returns smooth terms, only applies to GAMs (or similar
#'   models that may contain smooth terms)
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
                                    effects = "fixed",
                                    component = "all",
                                    flatten = FALSE,
                                    verbose = TRUE,
                                    ...) {
  effects <- validate_argument(effects, c("fixed", "random", "all"))
  component <- validate_argument(
    component,
    c(
      "all", "conditional", "zi", "zero_inflated", "dispersion", "instruments",
      "correlation", "smooth_terms", "location"
    )
  )

  f <- find_formula(x, verbose = verbose)
  is_mv <- is_multivariate(f)
  elements <- .get_elements(effects, component, model = x)


  # filter formulas, depending on requested effects and components
  if (is_mv) {
    f <- lapply(f, function(.x) .prepare_predictors(x, .x, elements))
  } else {
    f <- .prepare_predictors(x, f, elements)
  }

  # random effects are returned as list, so we need to unlist here
  if (is_mv) {
    l <- lapply(f, .return_vars, x = x)
  } else {
    l <- .return_vars(f, x)
  }

  if (is_empty_object(l) || is_empty_object(compact_list(l))) {
    return(NULL)
  }


  # some models, like spatial models, have random slopes that are not defined
  # as fixed effect predictor. In such cases, we have to add the random slope term
  # manually, so other functions like "get_data()" work as expected...

  if (object_has_names(l, "random") && effects == "all") {
    random_slope <- unlist(find_random_slopes(x), use.names = FALSE)
    all_predictors <- unlist(unique(l), use.names = FALSE)
    rs_not_in_pred <- unique(setdiff(random_slope, all_predictors))
    if (length(rs_not_in_pred)) l$random <- c(rs_not_in_pred, l$random)
  }


  if (flatten) {
    unique(unlist(l, use.names = FALSE))
  } else {
    l
  }
}


#' @export
find_predictors.selection <- function(x, flatten = FALSE, verbose = TRUE, ...) {
  elements <- .get_elements("all", "all")

  f <- lapply(find_formula(x, verbose = verbose), function(i) {
    .prepare_predictors(x, i, elements = elements)
  })

  l <- lapply(f, .return_vars, x = x)

  if (is_empty_object(l) || is_empty_object(compact_list(l))) {
    return(NULL)
  }

  if (flatten) {
    unique(unlist(l, use.names = FALSE))
  } else {
    l
  }
}


#' @export
find_predictors.logitr <- function(x, flatten = FALSE, ...) {
  l <- find_predictors.default(x)
  l[["cluster"]] <- get_call(x)$obsID
  if (flatten) {
    unique(unlist(l, use.names = FALSE))
  } else {
    l
  }
}


#' @export
find_predictors.fixest <- function(x, flatten = FALSE, ...) {
  response <- find_response(x)
  instruments <- x$iv_inst_names
  endo <- x$iv_endo_fml
  cluster <- x$fixef_vars

  if (!is.null(instruments)) {
    instruments <- all.vars(stats::as.formula(paste0("~", paste(instruments, collapse = "+"))))
  }
  if (!is.null(endo)) {
    endo <- all.vars(endo)
  }
  if (!is.null(cluster)) {
    cluster <- all.vars(stats::as.formula(paste0("~", paste(cluster, collapse = "+"))))
  }

  conditional <- all.vars(stats::formula(x))
  conditional <- setdiff(conditional, c(instruments, cluster, find_response(x)))
  # Catch for interacted fixest::i(f0, i.f2)
  if (any(grepl(", i.", x$fml, fixed = TRUE))) {
    conditional <- gsub("^i\\.", "", conditional)
  }

  l <- compact_list(list(
    conditional = conditional,
    cluster = cluster,
    instruments = instruments,
    endogenous = endo
  ))
  if (flatten) {
    unique(unlist(l, use.names = FALSE))
  } else {
    l
  }
}


#' @export
find_predictors.bfsl <- function(x, flatten = FALSE, verbose = TRUE, ...) {
  l <- list(conditional = "x")
  if (flatten) {
    unique(unlist(l, use.names = FALSE))
  } else {
    l
  }
}


#' @export
find_predictors.afex_aov <- function(x,
                                     effects = "fixed",
                                     flatten = FALSE,
                                     verbose = TRUE,
                                     ...) {
  effects <- validate_argument(effects, c("fixed", "random", "all"))

  if (effects == "all") {
    effects <- c("fixed", "random")
  }

  l <- list(
    fixed = c(names(attr(x, "between")), names(attr(x, "within"))),
    random = attr(x, "id")
  )[effects]

  if (flatten) {
    unique(unlist(l, use.names = FALSE))
  } else {
    l
  }
}


.return_vars <- function(f, x) {
  l <- lapply(names(f), function(i) {
    if (i %in% c("random", "zero_inflated_random")) {
      # random effects, just unlist
      unique(paste(unlist(f[[i]])))
    } else if (is.numeric(f[[i]])) {
      f[[i]]
    } else if (is.list(f[[i]])) {
      # this is for multivariate response models, where
      # we have a list of formulas
      lapply(f[[i]], function(j) unique(all.vars(j)))
    } else {
      vars <- unique(all.vars(f[[i]]))
      # when we have splines, the number of dimensions (argument "k") can be
      # stored in a variable. This variable name is no predictor, so we remove it
      # see #851
      # check if we have splines in the formula
      fs <- paste(as.character(f[[i]]), collapse = " ")
      if (grepl(" s(", fs, fixed = TRUE) || grepl("+s(", fs, fixed = TRUE)) {
        pattern <- "(.*)s\\((.*)k\\s?=(.*)\\)(.*)"
        k_value <- trim_ws(gsub(pattern, "\\3", fs))
        vars <- setdiff(vars, k_value)
      }
      vars
    }
  })

  empty_elements <- vapply(l, is_empty_object, logical(1))
  l <- compact_list(l)

  # here we handle special cases for non-linear model in brms (nl=TRUE)
  if (inherits(x, "brmsfit")) {
    nf <- stats::formula(x)
    at_nl <- attr(nf$formula, "nl", exact = TRUE)
    if (!is.null(at_nl) && !isFALSE(at_nl) && object_has_names(nf, "pforms")) {
      nl_parms <- names(nf$pforms)
      # All variables in the non-linear formulas get dumped in the "non-linear"
      # vector of the list. This may include cluster variables which identify
      # random effects components.
      l_pforms <- unlist(lapply(nf$pforms, all.vars), recursive = TRUE, use.names = FALSE)
      # don't overwrite. maybe this could be smarter
      if (length(l_pforms) > 0 && !"nonlinear" %in% names(l) && !"nonlinear" %in% names(f)) {
        f <- c(f, list(nonlinear = NULL)) # need this for renaming and subsetting later
        l <- c(l, list(nonlinear = unique(l_pforms)))
        l <- lapply(l, .remove_values, nl_parms)
      }
    }
  }


  # remove constants
  l <- lapply(l, .remove_values, c(".", "pi", "1", "0"))
  l <- lapply(l, .remove_values, c(0, 1))
  l <- lapply(l, function(i) gsub("`", "", i, fixed = TRUE))
  # for brms-models, we need to remove "Intercept", which is a special notation
  if (inherits(x, "brmsfit")) {
    l <- lapply(l, .remove_values, "Intercept")
  }
  names(l) <- names(f)[!empty_elements]

  l
}


.prepare_predictors <- function(x, f, elements) {
  f <- f[names(f) %in% elements]

  # from conditional model, remove response
  if (object_has_names(f, "conditional")) {
    f[["conditional"]] <- tryCatch(
      f[["conditional"]][[3]],
      # some models like {logitr} return a one-sided formula
      error = function(e) f[["conditional"]][[2]]
    )

    # for survival models, separate out strata element
    if (inherits(x, "coxph")) {
      f_cond <- safe_deparse(f[["conditional"]])
      # remove namespace prefixes
      f_cond <- .remove_namespace_from_string(f_cond)

      if (grepl("strata(", f_cond, fixed = TRUE)) {
        # create regular expressions to find strata terms
        yes_strata <- ".*strata\\((.*)\\).*"
        # This regular expression pattern in R matches any occurrence of the
        # string "strata" followed by parentheses containing any characters (.*),
        # optionally followed by any amount of whitespace (\\s*), and then followed
        # by any number of +, |, or * characters ([\\+|\\*]*). It also matches
        # any occurrence of +, |, or * characters followed by any amount of
        # whitespace (\\s*), and then followed by the string "strata" followed
        # by parentheses containing any characters (strata\\(.*\\)).
        # Here's a breakdown of the regular expression:
        # strata: Matches the string "strata".
        # \\(: Matches an opening parenthesis.
        # .*: Matches any character zero or more times.
        # \\): Matches a closing parenthesis.
        # \\s*: Matches any amount of whitespace zero or more times.
        # [\\+|\\*]*: Matches any number of +, |, or * characters zero or more times.
        # |: Alternation operator to match either the first or second pattern.
        # [\\+|\\*]: Matches a +, |, or * character.
        # \\s*: Matches any amount of whitespace zero or more times.
        # strata: Matches the string "strata".
        # \\(: Matches an opening parenthesis.
        # .*: Matches any character zero or more times.
        # \\): Matches a closing parenthesis.
        no_strata <- "strata\\(.*\\)\\s*[\\+|\\*]*|[\\+|\\*]\\s*strata\\(.*\\)"

        # find predictors used inside "strata()"
        strata <- gsub(",", "+", gsub(yes_strata, "\\1", f_cond), fixed = TRUE)
        # remove reserved terms from strata formula
        pattern <- "\\b(na\\.group|shortlabel)\\b\\s*=\\s*[^\\(]*"
        strata <- gsub(pattern, "", strata)
        # remove trailing "+"
        strata <- gsub("(.*)\\+$", "\\1", trim_ws(strata))

        # find predictors used outside "strata()"
        non_strata <- trim_ws(gsub("~", "", gsub(no_strata, "\\1", f_cond), fixed = TRUE))

        # create formula parts
        f$strata <- stats::reformulate(strata)
        f$conditional <- stats::reformulate(non_strata)
      }
    }
  }

  # from conditional model, remove response
  if (object_has_names(f, "survival")) {
    f[["survival"]] <- f[["survival"]][[3]]
  }

  # from conditional model, remove response
  if (inherits(x, "selection")) {
    if (object_has_names(f, "selection")) {
      f[["selection"]] <- f[["selection"]][[3]]
    }
    if (object_has_names(f, "outcome")) {
      f[["outcome"]] <- f[["outcome"]][[3]]
    }
  }

  # if we have random effects, just return grouping variable, not random slopes
  if (object_has_names(f, "random")) {
    f[["random"]] <- .get_group_factor(x, f[["random"]])
  }

  # same for zi-random effects
  if (object_has_names(f, "zero_inflated_random")) {
    f[["zero_inflated_random"]] <- .get_group_factor(x, f[["zero_inflated_random"]])
  }

  # same for sigma-random effects
  if (object_has_names(f, "sigma_random")) {
    f[["sigma_random"]] <- .get_group_factor(x, f[["sigma_random"]])
  }

  # same for beta-random effects
  if (object_has_names(f, "beta_random")) {
    f[["beta_random"]] <- .get_group_factor(x, f[["beta_random"]])
  }

  f
}
