#' @title Find model formula
#' @name find_formula
#'
#' @description Returns the formula(s) for the different parts of a model
#'  (like fixed or random effects, zero-inflated component, ...).
#'  `formula_ok()` checks if a model formula has valid syntax
#'  regarding writing `TRUE` instead of `T` inside `poly()`
#'  and that no data names are used (i.e. no `data$variable`, but rather
#'  `variable`).
#'
#' @param verbose Toggle warnings.
#' @param dichotomies Logical, if model is a `nestedLogit` objects, returns
#' the formulas for the dichotomies.
#' @param checks Indicates what kind of checks are conducted when checking
#' the formula notation. Currently, four different formula specification that
#' can result in unexpected behaviour of downstream-functions are checked.
#' `checks` can be one or more of:
#'
#' - `"dollar"`: Check if formula contains data name with "$", e.g. `mtcars$am`.
#' - `"T"`: Check if formula contains poly-term with "raw=T", e.g.
#'   `poly(x, 2, raw=T)`. In this case, `all.vars()` returns `T` as variable,
#'   which is not intended.
#' - `"index"`: Check if formula contains indexed data frames as response
#'   variable (e.g., `df[, 5] ~ x`).
#' - `"name"`: Check if syntactically invalid variable names were used and
#'   quoted in backticks.
#' - `"all"`: Checks all of the above mentioned options.
#'
#' @param action Should a message, warning or error be given for an invalid
#' formula? Must be one of `"message"`, `"warning"` (default) or `"error"`.
#' @param prefix_msg Optional string that will be added to the warning/error
#' message. This can be used to add additional information, e.g. about the
#' specific function that was calling `formula_ok()` and failed.
#' @param ... Currently not used.
#' @inheritParams find_predictors
#'
#' @return A list of formulas that describe the model. For simple models,
#'  only one list-element, `conditional`, is returned. For more complex
#'  models, the returned list may have following elements:
#'
#'  - `conditional`, the "fixed effects" part from the model (in the
#'    context of fixed-effects or instrumental variable regression, also
#'    called *regressors*) . One exception are `DirichletRegModel` models
#'    from **DirichletReg**, which has two or three components,
#'    depending on `model`.
#'
#'  - `random`, the "random effects" part from the model (or the
#'    `id` for gee-models and similar)
#'
#'  - `zero_inflated`, the "fixed effects" part from the
#'    zero-inflation component of the model
#'
#'  - `zero_inflated_random`, the "random effects" part from the
#'    zero-inflation component of the model
#'
#'  - `dispersion`, the dispersion formula
#'
#'  - `instruments`, for fixed-effects or instrumental variable
#'    regressions like `ivreg::ivreg()`, `lfe::felm()` or `plm::plm()`,
#'    the instrumental variables
#'
#'  - `cluster`, for fixed-effects regressions like
#'    `lfe::felm()`, the cluster specification
#'
#'  - `correlation`, for models with correlation-component like
#'    `nlme::gls()`, the formula that describes the correlation structure
#'
#'  - `scale`, for distributional models such as `mgcv::gaulss()` family fitted
#'    with `mgcv::gam()`, the formula that describes the scale parameter
#'
#'  - `slopes`, for fixed-effects individual-slope models like
#'    `feisr::feis()`, the formula for the slope parameters
#'
#'  - `precision`, for `DirichletRegModel` models from
#'    **DirichletReg**, when parametrization (i.e. `model`) is
#'    `"alternative"`.
#'
#'  - `bidrange`, for models of class `oohbchoice` (from package **DCchoice**),
#'    which indicates the right-hand side of the bar (the bid-range).
#'
#' @note For models of class `lme` or `gls` the correlation-component
#'   is only returned, when it is explicitly defined as named argument
#'   (`form`), e.g. `corAR1(form = ~1 | Mare)`
#'
#' @examplesIf require("lme4", quietly = TRUE)
#' data(mtcars)
#' m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
#' find_formula(m)
#'
#' m <- lme4::lmer(Sepal.Length ~ Sepal.Width + (1 | Species), data = iris)
#' f <- find_formula(m)
#' f
#' format(f)
#' @export
find_formula <- function(x, ...) {
  UseMethod("find_formula")
}


#' @rdname find_formula
#' @export
formula_ok <- function(x,
                       checks = "all",
                       action = "warning",
                       prefix_msg = NULL,
                       verbose = TRUE,
                       ...) {
  # if a model, retrieve formula. else, treat x as formula
  if (is_model(x)) {
    f <- find_formula(x, verbose = FALSE)
  } else {
    f <- x
  }

  check_1 <- check_2 <- check_3 <- check_4 <- TRUE
  valid_options <- c("all", "dollar", "T", "index", "name")

  # validate args
  action <- validate_argument(action, c("message", "warning", "error"))

  if (is.null(checks)) {
    checks <- "all"
  }
  if (!all(checks %in% valid_options)) {
    invalid <- setdiff(checks, valid_options)
    format_error(paste0(
      "Argument `checks` contained invalid options: ",
      toString(invalid),
      ". Please use one or more of ",
      toString(valid_options),
      "."
    ))
  }

  # check if formula contains data name with "$". This may
  # result in unexpected behaviour, and we should warn users
  if (all(checks == "all") || "dollar" %in% checks) {
    check_1 <- .check_formula_for_dollar(f, action, prefix_msg, verbose = verbose)
  }

  # check if formula contains poly-term with "raw=T". In this case,
  # all.vars() returns "T" as variable, which is not intended
  if (all(checks == "all") || "T" %in% checks) {
    check_2 <- .check_formula_for_T(f, action, prefix_msg, verbose = verbose)
  }

  # check if formula contains index data frames as response variable
  # this may result in unexpected behaviour, and we should warn users
  if (all(checks == "all") || "index" %in% checks) {
    check_3 <- .check_formula_index_df(f, x, action, prefix_msg, verbose = verbose)
  }

  # check if formula contains non-syntactic variable names and uses backticks
  # this may result in unexpected behaviour, and we should warn users
  if (all(checks == "all") || "name" %in% checks) {
    check_4 <- .check_formula_backticks(f, action, prefix_msg, verbose = verbose)
  }

  all(check_1 && check_2 && check_3 && check_4)
}


# Default method -----------------------------------

#' @rdname find_formula
#' @export
find_formula.default <- function(x, verbose = TRUE, ...) {
  f <- .safe(list(conditional = stats::formula(x)))
  .find_formula_return(f, verbose = verbose)
}


#' @export
find_formula.asym <- function(x, verbose = TRUE, ...) {
  modified_f <- safe_deparse(stats::formula(x))
  # limitation: we can't preserve "*" and ":"
  modified_f <- gsub("*", "+", modified_f, fixed = TRUE)
  modified_f <- gsub(":", "+", modified_f, fixed = TRUE)
  # explanation:
  # - gsub("\\+\\s*minus__[^\\+]+", "", input_string):
  #   This regular expression matches and removes any term that starts with
  #   + minus__ followed by any characters that are not a +.
  # - gsub("\\s*\\+\\s*$", "", output_string):
  #   This removes any trailing plus sign and whitespace that might be left
  #   at the end of the string.
  output_string <- gsub("\\+\\s*minus__[^\\+]+", "", modified_f)
  output_string <- gsub("\\s*(\\+|\\*)\\s*$", "", output_string) # Remove trailing plus sign if any
  # explanation:
  # - gsub("lag_([a-zA-Z]+)_", "lag(\\1)", input_string):
  #   This regular expression matches the pattern "lag_", followed by one or
  #   more letters (captured in a group), followed by "_". It replaces this
  #   pattern with "lag(", the captured group, and ")".
  output_string <- gsub("lag_([a-zA-Z]+)_", "lag(\\1)", output_string)
  output_string <- gsub("plus__", "", output_string, fixed = TRUE)
  f <- .safe(list(conditional = stats::as.formula(output_string)))
  .find_formula_return(f, verbose = verbose)
}


#' @export
find_formula.list <- function(x, verbose = TRUE, ...) {
  if (object_has_names(x, "gam")) {
    if ("mer" %in% names(x)) {
      f.random <- .fix_gamm4_random_effect(find_formula(x$mer)$random)
      if (length(f.random) == 1L) {
        f.random <- f.random[[1]]
      } else if (length(f.random) == 0) {
        f.random <- NULL
      }
    }
    x <- x$gam
    class(x) <- c(class(x), c("glm", "lm"))
    f <- compact_list(list(conditional = stats::formula(x), random = f.random))
  } else {
    f <- find_formula.default(x, ...)
  }
  .find_formula_return(f, verbose = verbose)
}


#' @export
find_formula.data.frame <- function(x, verbose = TRUE, ...) {
  format_error("A data frame is not a valid object for this function.")
}


#' @export
find_formula.aovlist <- function(x, verbose = TRUE, ...) {
  f <- attr(x, "terms", exact = TRUE)
  attributes(f) <- NULL
  .find_formula_return(list(conditional = f), verbose = verbose)
}


#' @export
find_formula.anova <- function(x, verbose = TRUE, ...) {
  format_error("Formulas cannot be retrieved from anova() objects.")
}


# GAM -----------------------------------------------------------

#' @export
find_formula.SemiParBIV <- function(x, verbose = TRUE, ...) {
  f <- stats::formula(x, ...)
  names(f) <- c("Equation 1", "Equation 2", "Equation 3")[seq_along(f)]
  f <- list(conditional = f)
  .find_formula_return(f, verbose = verbose)
}


#' @export
find_formula.gam <- function(x, verbose = TRUE, ...) {
  f <- tryCatch(stats::formula(x), error = function(x) NULL)

  if (!is.null(f)) {
    if (is.list(f)) {
      mi <- .gam_family(x)
      if (!is.null(mi)) {
        f <- switch(mi$family,
          ziplss = list(conditional = f[[1]], zero_inflated = f[[2]]),
          # handle formula for location-scale models
          gaulss = list(conditional = f[[1]], scale = f[[2]]),
          # handle formula for multivariate models, or for multinomial models,
          # which also contain a list of formulas
          multinom = {
            y <- safe_deparse(f[[1]][[2]])
            f <- lapply(f, function(.i) {
              f_cond <- .i
              if (length(f_cond) < 3) {
                f_cond <- stats::as.formula(paste(y, safe_deparse(f_cond)))
              }
              list(conditional = f_cond)
            })
            names(f) <- rep_len(y, length(f))
            attr(f, "is_mv") <- "1"
            f
          },
          `Multivariate normal` = {
            r <- lapply(f, function(.i) safe_deparse(.i[[2]]))
            f <- lapply(f, function(.i) list(conditional = .i))
            names(f) <- r
            attr(f, "is_mv") <- "1"
            f
          }
        )
      }
    } else {
      f <- list(conditional = f)
    }
  }

  .find_formula_return(f, verbose = verbose)
}


#' @export
find_formula.gamlss <- function(x, verbose = TRUE, ...) {
  f <- tryCatch(
    {
      f.cond <- stats::as.formula(.get_fixed_effects(x$mu.formula))

      f.random <- lapply(.findbars(x$mu.formula), function(.x) {
        f <- safe_deparse(.x)
        stats::as.formula(paste0("~", f))
      })

      if (length(f.random) == 1L) {
        f.random <- f.random[[1]]
      } else if (grepl("random\\((.*)\\)", safe_deparse(f.cond))) {
        f.cond <- safe_deparse(f.cond)
        # remove namespace prefixes
        f.cond <- .remove_namespace_from_string(f.cond)
        re <- gsub("(.*)random\\((.*)\\)", "\\2", f.cond)
        f.random <- stats::as.formula(paste0("~1|", re))
        f.cond <- stats::update.formula(
          stats::as.formula(f.cond),
          stats::as.formula(paste0(". ~ . - random(", re, ")"))
        )
      }

      compact_list(list(
        conditional = f.cond,
        random = f.random,
        sigma = x$sigma.formula,
        nu = x$nu.formula,
        tau = x$tau.formula
      ))
    },
    error = function(x) {
      NULL
    }
  )
  .find_formula_return(f, verbose = verbose)
}


#' @export
find_formula.bamlss <- function(x, verbose = TRUE, ...) {
  f <- stats::formula(x)

  if (!is.null(f$mu)) {
    f.cond <- f$mu$formula
  } else if (!is.null(f$pi)) {
    f.cond <- f$pi$formula
  }

  if (!is.null(f$sigma)) {
    f.sigma <- stats::as.formula(paste0("~", as.character(f$sigma$formula)[3]))
  } else if (!is.null(f$pi)) {
    f.sigma <- NULL
  }

  f <- compact_list(list(
    conditional = stats::as.formula(safe_deparse(f.cond)),
    sigma = f.sigma
  ))
  .find_formula_return(f, verbose = verbose)
}


#' @export
find_formula.gamm <- function(x, verbose = TRUE, ...) {
  f <- compact_list(find_formula(x$gam))
  random <- .fix_gamm_random_effect(names(x$lme$groups))

  if (length(random) == 0) {
    f.random <- NULL
  } else if (length(random) > 1L) {
    f.random <- lapply(random, function(r) stats::as.formula(paste0("~1|", r)))
  } else {
    f.random <- stats::as.formula(paste0("~1|", random))
  }

  .find_formula_return(compact_list(c(f, list(random = f.random))))
}


# Meta-Analysis -----------------------

#' @export
find_formula.rma <- function(x, verbose = TRUE, ...) {
  formula.yi <- `attributes<-`(stats::formula(x, type = "yi"), NULL)
  formula.mods <- `attributes<-`(stats::formula(x, type = "mods"), NULL)
  formula.scale <- `attributes<-`(.safe(stats::formula(x, type = "scale")), NULL)

  model_call <- get_call(x)
  if (is.null(formula.yi)) {
    if (is.null(formula.mods)) {
      formula.yi <- stats::as.formula(paste(model_call$yi, "~ 1"))
    } else {
      formula.mods[3] <- formula.mods[2]
      formula.mods[[2]] <- model_call$yi
      formula.yi <- formula.mods
      # TODO: this code line should be identcal to the three lines above, but maybe safer
      # formula.yi <- formula.mods <- stats::as.formula(paste(all.vars(model_call$yi), "~", all.vars(formula.mods)))
    }
  }
  f <- compact_list(list(
    conditional = formula.yi,
    dispersion = formula.mods
  ))
  .find_formula_return(f, verbose = verbose)
}

# TODO: Check these

#' @export
find_formula.meta_random <- function(x, verbose = TRUE, ...) {
  NULL
}

#' @export
find_formula.metaplus <- find_formula.meta_random

#' @export
find_formula.meta_fixed <- find_formula.meta_random

#' @export
find_formula.meta_bma <- find_formula.meta_random

#' @export
find_formula.deltaMethod <- find_formula.meta_random


# Other models ----------------------------------------------


#' @export
find_formula.censReg <- find_formula.default

#' @export
find_formula.maxLik <- find_formula.default

#' @export
find_formula.maxim <- find_formula.default


#' @rdname find_formula
#' @export
find_formula.nestedLogit <- function(x, dichotomies = FALSE, verbose = TRUE, ...) {
  if (isTRUE(dichotomies)) {
    stats::setNames(
      lapply(x$models, function(m) {
        f <- list(conditional = get_call(m)$formula)
        .find_formula_return(f, verbose = verbose)
      }),
      names(x$models)
    )
  } else {
    find_formula.default(x, verbose = verbose, ...)
  }
}


#' @export
find_formula.systemfit <- function(x, verbose = TRUE, ...) {
  f <- stats::formula(x)
  l <- lapply(f, function(i) {
    list(conditional = i)
  })
  f <- compact_list(l)

  if (length(f) > 1L) {
    attr(f, "is_mv") <- "1"
  }
  .find_formula_return(f, verbose = verbose)
}


#' @export
find_formula.marginaleffects <- function(x, verbose = TRUE, ...) {
  find_formula(attributes(x)$model, verbose = verbose, ...)
}


#' @export
find_formula.selection <- function(x, verbose = TRUE, ...) {
  model_call <- parse(text = deparse(get_call(x)))[[1]]
  # 1st pass: formulas directly in the call
  # 2nd pass: formulas as symbols (assigned to an object, which is then used in the call)
  f_selection <- tryCatch(
    stats::as.formula(model_call$selection),
    error = function(e) stats::as.formula(eval(model_call$selection))
  )
  f_outcome <- tryCatch(
    stats::as.formula(model_call$outcome),
    error = function(e) stats::as.formula(eval(model_call$outcome))
  )
  f <- list(conditional = list(
    selection = f_selection,
    outcome = f_outcome
  ))
  attr(f, "two_stage") <- TRUE
  .find_formula_return(f, verbose = verbose)
}


#' @export
find_formula.svy_vglm <- function(x, verbose = TRUE, ...) {
  find_formula(x$fit)
}


#' @export
find_formula.mjoint <- function(x, verbose = TRUE, ...) {
  s <- summary(x)

  f.cond <- s$formLongFixed
  if (length(s$formLongFixed) == 1L) {
    names(f.cond) <- "conditional"
  } else {
    names(f.cond) <- paste0("conditional", seq_along(f.cond))
  }

  f.rand <- s$formLongRandom
  if (length(s$formLongRandom) == 1L) {
    names(f.rand) <- "random"
  } else {
    names(f.rand) <- paste0("random", seq_along(f.rand))
  }

  f <- c(f.cond, f.rand, list(survival = s$formSurv))
  .find_formula_return(f, verbose = verbose)
}


#' @export
find_formula.mvord <- function(x, verbose = TRUE, ...) {
  f <- list(conditional = x$rho$formula)
  .find_formula_return(f, verbose = verbose)
}


#' @export
find_formula.btergm <- function(x, verbose = TRUE, ...) {
  f <- list(conditional = x@formula)
  .find_formula_return(f, verbose = verbose)
}


#' @export
find_formula.mediate <- function(x, verbose = TRUE, ...) {
  f <- list(
    mediator = find_formula(x$model.m),
    outcome = find_formula(x$model.y)
  )
  .find_formula_return(f, verbose = verbose)
}


#' @export
find_formula.averaging <- function(x, verbose = TRUE, ...) {
  f_random <- tryCatch(
    {
      models <- attributes(x)$modelList
      find_formula(models[[1]])
    },
    error = function(e) {
      NULL
    }
  )

  f <- find_formula.default(x)
  if (!object_has_names(f, "random") && object_has_names(f_random, "random")) {
    f$random <- f_random$random
  }

  .find_formula_return(f, verbose = verbose)
}


#' @export
find_formula.glht <- function(x, verbose = TRUE, ...) {
  .find_formula_return(list(conditional = stats::formula(x$model)), verbose = verbose)
}


#' @export
find_formula.joint <- function(x, verbose = TRUE, ...) {
  f <- stats::formula(x)
  .find_formula_return(
    list(conditional = f$lformula, survival = f$sformula),
    verbose = verbose
  )
}


#' @export
find_formula.betareg <- function(x, verbose = TRUE, ...) {
  f <- stats::formula(x)
  fs <- safe_deparse(f)

  if (grepl("|", fs, fixed = TRUE)) {
    fs <- trim_ws(unlist(strsplit(fs, "|", fixed = TRUE), use.names = FALSE))
    f <- list(
      conditional = stats::as.formula(fs[1]),
      precision = stats::as.formula(paste0("~", fs[2]))
    )
  } else {
    f <- list(conditional = f)
  }
  .find_formula_return(f, verbose = verbose)
}


#' @export
find_formula.logitr <- function(x, verbose = TRUE, ...) {
  f <- .safe(list(conditional = stats::formula(x)))
  # formula() for logitr does not include outcome
  # need to paste "outcome" value from call manually to the formula
  f_cond <- trim_ws(safe_deparse(f$conditional))
  if (startsWith(f_cond, "~")) {
    resp <- parse(text = safe_deparse(get_call(x)))[[1]]$outcome
    f$conditional <- stats::as.formula(paste(resp, f_cond))
  }
  # random effects?
  ran_pars <- names(x$parIDs$r)
  ## TODO @vincentarelbundock any ideas?
  .find_formula_return(f, verbose = verbose)
}


#' @export
find_formula.afex_aov <- function(x, verbose = TRUE, ...) {
  if (length(attr(x, "within")) == 0L) {
    fff <- find_formula(x$lm, verbose = verbose, ...)
    fff$conditional[2] <- call(attr(x, "dv")) # need to fix LHS
    fff
  } else {
    d <- get_data(x, shape = "long")

    dv <- attr(x, "dv")
    id <- attr(x, "id")

    within_variables <- names(attr(x, "within"))
    within_variables <- paste(within_variables, collapse = "*")
    within_variables <- paste0("(", within_variables, ")")
    e <- paste0("Error(", id, "/", within_variables, ")")

    between <- names(attr(x, "between"))
    if (length(between) > 0L) {
      tempf <- find_formula(x$lm)[[1]]
      between <- as.character(tempf)[3]
      between <- paste0("(", between, ")")

      within_variables <- paste(c(within_variables, between), collapse = "*")
    }

    out <- list(conditional = stats::formula(paste0(dv, "~", within_variables, "+", e)))
    class(out) <- c("insight_formula", "list")
    out
  }
}


#' @export
find_formula.mira <- function(x, verbose = TRUE, ...) {
  .find_formula_return(find_formula(x$analyses[[1]]), verbose = verbose)
}


#' @export
find_formula.gee <- function(x, verbose = TRUE, ...) {
  f <- tryCatch(
    {
      id <- parse(text = safe_deparse(x$call))[[1]]$id

      # alternative regex-patterns that also work:
      # sub(".*id ?= ?(.*?),.*", "\\1", safe_deparse(x$call), perl = TRUE)
      # sub(".*\\bid\\s*=\\s*([^,]+).*", "\\1", safe_deparse(x$call), perl = TRUE)

      list(
        conditional = stats::formula(x),
        random = stats::as.formula(paste0("~", id))
      )
    },
    error = function(x) {
      NULL
    }
  )
  .find_formula_return(f, verbose = verbose)
}

#' @export
find_formula.glmgee <- find_formula.gee


#' @export
find_formula.MANOVA <- function(x, verbose = TRUE, ...) {
  f <- compact_list(list(
    conditional = x$input$formula,
    random = stats::as.formula(paste0("~", x$input$subject))
  ))
  .find_formula_return(f, verbose = verbose)
}

#' @export
find_formula.RM <- find_formula.MANOVA


#' @export
find_formula.gls <- function(x, verbose = TRUE, ...) {
  ## TODO this is an intermediate fix to return the correlation variables from gls-objects
  fcorr <- x$call$correlation
  if (is.null(fcorr)) {
    f_corr <- NULL
  } else if (inherits(fcorr, "name")) {
    f_corr <- attributes(eval(fcorr))$formula
  } else {
    f_corr <- parse(text = safe_deparse(fcorr))[[1]]
  }
  if (is.symbol(f_corr)) {
    f_corr <- paste("~", safe_deparse(f_corr))
  } else if (!inherits(f_corr, "formula")) {
    f_corr <- f_corr$form
  }

  l <- tryCatch(
    list(
      conditional = stats::formula(x),
      correlation = stats::as.formula(f_corr)
    ),
    error = function(x) {
      NULL
    }
  )

  .find_formula_return(compact_list(l))
}


#' @export
find_formula.LORgee <- function(x, verbose = TRUE, ...) {
  f <- tryCatch(
    {
      id <- parse(text = safe_deparse(x$call))[[1]]$id

      # alternative regex-patterns that also work:
      # sub(".*id ?= ?(.*?),.*", "\\1", safe_deparse(x$call), perl = TRUE)
      # sub(".*\\bid\\s*=\\s*([^,]+).*", "\\1", safe_deparse(x$call), perl = TRUE)

      list(
        conditional = stats::formula(x),
        random = stats::as.formula(paste0("~", id))
      )
    },
    error = function(x) {
      NULL
    }
  )
  .find_formula_return(f, verbose = verbose)
}


#' @export
find_formula.cglm <- function(x, verbose = TRUE, ...) {
  f <- tryCatch(
    {
      id <- parse(text = safe_deparse(x$call))[[1]]$id

      # alternative regex-patterns that also work:
      # sub(".*id ?= ?(.*?),.*", "\\1", safe_deparse(x$call), perl = TRUE)
      # sub(".*\\bid\\s*=\\s*([^,]+).*", "\\1", safe_deparse(x$call), perl = TRUE)

      list(
        conditional = stats::formula(x),
        random = stats::as.formula(paste0("~", id))
      )
    },
    error = function(x) {
      NULL
    }
  )
  .find_formula_return(f, verbose = verbose)
}


# mfx models ---------------------------------------

#' @export
find_formula.betamfx <- find_formula.betareg

#' @export
find_formula.betaor <- find_formula.betareg

#' @export
find_formula.logitmfx <- function(x, verbose = TRUE, ...) {
  find_formula.default(x$fit, ...)
}

#' @export
find_formula.poissonmfx <- find_formula.logitmfx

#' @export
find_formula.negbinmfx <- find_formula.logitmfx

#' @export
find_formula.logitor <- find_formula.logitmfx

#' @export
find_formula.negbinirr <- find_formula.logitmfx

#' @export
find_formula.poissonirr <- find_formula.logitmfx

#' @export
find_formula.probitmfx <- find_formula.logitmfx


# Panel data models ---------------------------------------

#' @export
find_formula.ivreg <- function(x, verbose = TRUE, ...) {
  f <- tryCatch(
    {
      f <- safe_deparse(stats::formula(x))
      cond <- trim_ws(substr(f, start = 0, stop = regexpr("|", f, fixed = TRUE) - 1))
      instr <- trim_ws(substr(f, regexpr("|", f, fixed = TRUE) + 1, stop = 10000L))

      list(
        conditional = stats::as.formula(cond),
        instruments = stats::as.formula(paste0("~", instr))
      )
    },
    error = function(x) {
      NULL
    }
  )
  .find_formula_return(f, verbose = verbose)
}

#' @export
find_formula.iv_robust <- find_formula.ivreg

#' @export
find_formula.ivFixed <- find_formula.ivreg


#' @export
find_formula.plm <- function(x, verbose = TRUE, ...) {
  f <- tryCatch(
    {
      f <- safe_deparse(stats::formula(x))
      bar_pos <- regexpr("|", f, fixed = TRUE)

      if (bar_pos == -1) {
        stop_pos <- nchar(f) + 1
      } else {
        stop_pos <- bar_pos
      }

      cond <- trim_ws(substr(f, start = 0, stop = stop_pos - 1))
      instr <- trim_ws(substr(f, stop_pos + 1, stop = 10000L))

      if (.is_empty_string(instr)) {
        list(conditional = stats::as.formula(cond))
      } else {
        # check if formula starts with dot, and remove it
        instr <- gsub("(^\\.\\s*)(.*)", "\\2", instr)
        list(
          conditional = stats::as.formula(cond),
          instruments = stats::as.formula(paste0("~", instr))
        )
      }
    },
    error = function(x) {
      NULL
    }
  )
  .find_formula_return(f, verbose = verbose)
}

#' @export
find_formula.pgmm <- find_formula.plm


#' @export
find_formula.felm <- function(x, verbose = TRUE, ...) {
  f <- safe_deparse(stats::formula(x))
  f_parts <- trim_ws(unlist(strsplit(f, "(?<!\\()\\|(?![\\w\\s\\+\\(~]*[\\)])", perl = TRUE), use.names = FALSE))

  f.cond <- f_parts[1]

  if (length(f_parts) > 1L) {
    f.rand <- paste0("~", f_parts[2])
  } else {
    f.rand <- NULL
  }

  if (length(f_parts) > 2L) {
    f.instr <- paste0("~", f_parts[3])
  } else {
    f.instr <- NULL
  }

  if (length(f_parts) > 3L) {
    f.clus <- paste0("~", f_parts[4])
  } else {
    f.clus <- NULL
  }

  f <- compact_list(list(
    conditional = stats::as.formula(f.cond),
    random = stats::as.formula(f.rand),
    instruments = stats::as.formula(f.instr),
    cluster = stats::as.formula(f.clus)
  ))
  .find_formula_return(f, verbose = verbose)
}


#' @export
find_formula.mhurdle <- function(x, verbose = TRUE, ...) {
  f <- safe_deparse(stats::formula(x)[[3]])
  f_parts <- trim_ws(unlist(strsplit(f, "(?<!\\()\\|(?![\\w\\s\\+\\(~]*[\\)])", perl = TRUE), use.names = FALSE))

  f.zi <- paste0("~", f_parts[1])

  if (length(f_parts) > 1L) {
    f.cond <- paste0(safe_deparse(stats::formula(x)[[2]]), "~", f_parts[2])
  } else {
    f.cond <- NULL
  }

  if (length(f_parts) > 2L) {
    f.ip <- paste0("~", f_parts[3])
  } else {
    f.ip <- NULL
  }

  # remove "empty" parts
  if (f.zi == "~0") {
    f.zi <- NULL
  }
  if (f.ip == "~0") {
    f.ip <- NULL
  }

  f <- compact_list(list(
    conditional = stats::as.formula(f.cond),
    zero_inflated = stats::as.formula(f.zi),
    infrequent_purchase = stats::as.formula(f.ip)
  ))
  .find_formula_return(f, verbose = verbose)
}


#' @export
find_formula.feglm <- function(x, verbose = TRUE, ...) {
  f <- safe_deparse(stats::formula(x))
  f_parts <- unlist(strsplit(f, "(?<!\\()\\|(?![\\w\\s\\+\\(~]*[\\)])", perl = TRUE), use.names = FALSE)

  f.cond <- trim_ws(f_parts[1])

  if (length(f_parts) > 1L) {
    f.instr <- paste0("~", trim_ws(f_parts[2]))
  } else {
    f.instr <- NULL
  }

  if (length(f_parts) > 2L) {
    f.clus <- paste0("~", trim_ws(f_parts[3]))
  } else {
    f.clus <- NULL
  }

  f <- compact_list(list(
    conditional = stats::as.formula(f.cond),
    instruments = stats::as.formula(f.instr),
    cluster = stats::as.formula(f.clus)
  ))
  .find_formula_return(f, verbose = verbose)
}


#' @export
find_formula.fixest <- function(x, verbose = TRUE, ...) {
  f <- safe_deparse(stats::formula(x))
  f_parts <- unlist(strsplit(f, "(?<!\\()\\|(?![\\w\\s\\+\\(~]*[\\)])", perl = TRUE), use.names = FALSE)

  f.cond <- trim_ws(f_parts[1])

  if (length(f_parts) > 1L) {
    f.clus <- paste0("~", trim_ws(f_parts[2]))
  } else {
    f.clus <- parse(text = deparse(x$call))[[1]]$fixef
    if (!is.null(f.clus)) {
      f.clus <- paste("~", paste(eval(f.clus), collapse = " + "))
    }
  }

  f <- compact_list(list(
    conditional = stats::as.formula(f.cond),
    cluster = stats::as.formula(f.clus)
  ))
  .find_formula_return(f, verbose = verbose)
}


#' @export
find_formula.oohbchoice <- function(x, verbose = TRUE, ...) {
  f <- safe_deparse(stats::formula(x))
  f_parts <- trim_ws(unlist(strsplit(f, "|", fixed = TRUE), use.names = FALSE))

  f.cond <- f_parts[1]
  f.bidrange <- f_parts[2]

  f <- compact_list(list(
    conditional = stats::as.formula(f.cond),
    bidrange = stats::as.formula(paste("~", f.bidrange))
  ))
  .find_formula_return(f, verbose = verbose)
}


#' @export
find_formula.feis <- function(x, verbose = TRUE, ...) {
  f <- safe_deparse(stats::formula(x))
  f_parts <- unlist(strsplit(f, "(?<!\\()\\|(?![\\w\\s\\+\\(~]*[\\)])", perl = TRUE), use.names = FALSE)

  f.cond <- trim_ws(f_parts[1])
  id <- parse(text = safe_deparse(x$call))[[1]]$id

  # alternative regex-patterns that also work:
  # sub(".*id ?= ?(.*?),.*", "\\1", safe_deparse(x$call), perl = TRUE)
  # sub(".*\\bid\\s*=\\s*([^,]+).*", "\\1", safe_deparse(x$call), perl = TRUE)

  if (length(f_parts) > 1L) {
    f.slopes <- paste0("~", trim_ws(f_parts[2]))
  } else {
    f.slopes <- NULL
  }

  f <- compact_list(list(
    conditional = stats::as.formula(f.cond),
    slopes = stats::as.formula(f.slopes),
    random = stats::as.formula(paste0("~", id))
  ))
  .find_formula_return(f, verbose = verbose)
}


#' @export
find_formula.bife <- function(x, verbose = TRUE, ...) {
  f <- safe_deparse(stats::formula(x))
  f_parts <- unlist(strsplit(f, "|", fixed = TRUE), use.names = FALSE)

  f.cond <- trim_ws(f_parts[1])

  if (length(f_parts) > 1L) {
    f.rand <- paste0("~", trim_ws(f_parts[2]))
  } else {
    f.rand <- NULL
  }

  f <- compact_list(list(
    conditional = stats::as.formula(f.cond),
    random = stats::as.formula(f.rand)
  ))
  .find_formula_return(f, verbose = verbose)
}


#' @export
find_formula.ivprobit <- function(x, verbose = TRUE, ...) {
  NULL
}


#' @export
find_formula.wbm <- function(x, verbose = TRUE, ...) {
  f <- safe_deparse(stats::formula(x))
  f_parts <- unlist(strsplit(f, "(?<!\\()\\|(?![\\w\\s\\+\\(~]*[\\)])", perl = TRUE), use.names = FALSE)

  f.cond <- trim_ws(f_parts[1])

  if (length(f_parts) > 1L) {
    f.instr <- paste0("~", trim_ws(f_parts[2]))
  } else {
    f.instr <- NULL
  }

  if (length(f_parts) > 2L) {
    f_parts[3] <- trim_ws(f_parts[3])
    if (grepl("\\((.+)\\|(.+)\\)", f_parts[3])) {
      # we have multiple random effects, which we can better extract
      # via ".findbars()"
      if (length(gregexpr("|", f_parts[3], fixed = TRUE)[[1]]) > 1L) {
        f.rand <- .findbars(stats::as.formula(paste("~", f_parts[3])))
      } else {
        f.rand <- gsub("(\\(|\\))", "", f_parts[3])
        f.rand <- stats::as.formula(paste0("~", trim_ws(f.rand)))
      }
      f.clint <- NULL
    } else {
      ## TODO dangerous fix to convert cross-level interactions
      # into random effects...
      f.clint <- f_parts[3]
      f.clint <- paste0("~", trim_ws(f.clint))
      f.rand <- NULL
    }
  } else {
    f.rand <- NULL
    f.clint <- NULL
  }

  f <- compact_list(list(
    conditional = stats::as.formula(f.cond),
    instruments = stats::as.formula(f.instr),
    interactions = stats::as.formula(f.clint),
    random = f.rand
  ))
  .find_formula_return(f, verbose = verbose)
}

#' @export
find_formula.wbgee <- find_formula.wbm


#' @export
find_formula.glimML <- function(x, verbose = TRUE, ...) {
  f <- compact_list(list(
    conditional = x@formula,
    random = x@random
  ))
  .find_formula_return(f, verbose = verbose)
}


#' @export
find_formula.tobit <- function(x, verbose = TRUE, ...) {
  f <- tryCatch(
    list(conditional = parse(text = safe_deparse(x$call))[[1]]$formula),
    error = function(x) NULL
  )
  .find_formula_return(f, verbose = verbose)
}


# Zero inflated models --------------------------------------

#' @export
find_formula.hurdle <- function(x, verbose = TRUE, ...) {
  .zeroinf_formula(x, verbose = verbose)
}

#' @export
find_formula.zeroinfl <- find_formula.hurdle

#' @export
find_formula.zerotrunc <- find_formula.hurdle


#' @export
find_formula.zcpglm <- function(x, verbose = TRUE, ...) {
  .zeroinf_formula(x, separator = "\\|\\|", verbose = verbose)
}


# Ordinal models  --------------------------------------

#' @export
find_formula.clmm2 <- function(x, verbose = TRUE, ...) {
  f <- compact_list(list(
    conditional = stats::as.formula(safe_deparse(attr(x$location, "terms", exact = TRUE))),
    scale = stats::as.formula(safe_deparse(attr(x$scale, "terms", exact = TRUE))),
    random = stats::as.formula(paste0("~", parse(text = safe_deparse(x$call))[[1]]$random))
  ))
  .find_formula_return(f, verbose = verbose)
}


#' @export
find_formula.clm2 <- function(x, verbose = TRUE, ...) {
  f <- compact_list(list(
    conditional = stats::formula(attr(x$location, "terms", exact = TRUE)),
    scale = stats::formula(attr(x$scale, "terms", exact = TRUE))
  ))
  .find_formula_return(f, verbose = verbose)
}


#' @export
find_formula.clm <- function(x, verbose = TRUE, ...) {
  f <- compact_list(list(
    conditional = stats::formula(x),
    scale = x$formulas$scale,
    nominal = x$formulas$nominal
  ))
  .find_formula_return(f, verbose = verbose)
}


#' @export
find_formula.DirichletRegModel <- function(x, verbose = TRUE, ...) {
  f <- safe_deparse(stats::formula(x))
  f_parts <- unlist(strsplit(f, "(?<!\\()\\|(?![\\w\\s\\+\\(~]*[\\)])", perl = TRUE), use.names = FALSE)

  f.cond <- trim_ws(f_parts[1])

  if (length(f_parts) > 1L) {
    f.cond2 <- paste0("~", trim_ws(f_parts[2]))
  } else {
    f.cond2 <- NULL
  }

  if (length(f_parts) > 2L) {
    f.cond3 <- paste0("~", trim_ws(f_parts[3]))
  } else {
    f.cond3 <- NULL
  }

  out <- compact_list(list(
    conditional = stats::as.formula(f.cond),
    conditional2 = stats::as.formula(f.cond2),
    conditional3 = stats::as.formula(f.cond3)
  ))

  if (x$parametrization == "alternative" && length(out) == 2L) {
    names(out)[2] <- "precision"
  }

  .find_formula_return(out, verbose = verbose)
}


# Mixed models -----------------------

#' @export
find_formula.glmmTMB <- function(x, verbose = TRUE, ...) {
  f.cond <- stats::formula(x)
  f.zi <- stats::formula(x, component = "zi")
  f.disp <- stats::formula(x, component = "disp")

  # check for "empty" formulas
  if (identical(safe_deparse(f.zi), "~0") || identical(safe_deparse(f.zi), "~1")) {
    f.zi <- NULL
  }

  if (identical(safe_deparse(f.disp), "~0") || identical(safe_deparse(f.disp), "~1")) {
    f.disp <- NULL
  }

  # extract random parts of formula
  f.random <- lapply(.findbars(f.cond), function(.x) {
    f <- safe_deparse(.x)
    stats::as.formula(paste0("~", f))
  })

  if (length(f.random) == 1L) {
    f.random <- f.random[[1]]
  }

  f.zirandom <- lapply(.findbars(f.zi), function(.x) {
    f <- safe_deparse(.x)
    if (f == "NULL") {
      return(NULL)
    }
    stats::as.formula(paste0("~", f))
  })

  if (length(f.zirandom) == 1L) {
    f.zirandom <- f.zirandom[[1]]
  }

  f.disprandom <- lapply(.findbars(f.disp), function(.x) {
    f <- safe_deparse(.x)
    if (f == "NULL") {
      return(NULL)
    }
    stats::as.formula(paste0("~", f))
  })

  if (length(f.disprandom) == 1L) {
    f.disprandom <- f.disprandom[[1]]
  }

  # extract fixed effects parts
  f.cond <- stats::as.formula(.get_fixed_effects(f.cond))
  if (!is.null(f.zi)) f.zi <- stats::as.formula(.get_fixed_effects(f.zi))
  if (!is.null(f.disp)) f.disp <- stats::as.formula(.get_fixed_effects(f.disp))

  f <- compact_list(list(
    conditional = f.cond,
    random = f.random,
    zero_inflated = f.zi,
    zero_inflated_random = f.zirandom,
    dispersion = f.disp,
    dispersion_random = f.disprandom
  ))
  .find_formula_return(f, verbose = verbose)
}


#' @export
find_formula.nlmerMod <- function(x, verbose = TRUE, ...) {
  f.random <- lapply(.findbars(stats::formula(x)), function(.x) {
    f <- safe_deparse(.x)
    stats::as.formula(paste0("~", f))
  })

  if (length(f.random) == 1L) {
    f.random <- f.random[[1]]
  }

  f.cond <- .nobars(stats::as.formula(gsub("(.*)(~)(.*)~(.*)", "\\1\\2\\4", safe_deparse(stats::formula(x)))))
  f.nonlin <- stats::as.formula(paste0("~", trim_ws(gsub("(.*)~(.*)~(.*)", "\\2", safe_deparse(stats::formula(x))))))

  f <- compact_list(list(
    conditional = f.cond,
    nonlinear = f.nonlin,
    random = f.random
  ))
  .find_formula_return(f, verbose = verbose)
}


#' @export
find_formula.hglm <- function(x, verbose = TRUE, ...) {
  mc <- get_call(x)
  f.cond <- mc$fixed
  f.random <- mc$random
  f.disp <- mc$disp

  f <- compact_list(list(conditional = f.cond, random = f.random, dispersion = f.disp))
  .find_formula_return(f, verbose = verbose)
}


#' @export
find_formula.merMod <- function(x, verbose = TRUE, ...) {
  f.cond <- stats::formula(x)
  f.random <- lapply(.findbars(f.cond), function(.x) {
    f <- safe_deparse(.x)
    stats::as.formula(paste0("~", f))
  })

  if (length(f.random) == 1L) {
    f.random <- f.random[[1]]
  }

  f.cond <- stats::as.formula(.get_fixed_effects(f.cond))
  f <- compact_list(list(conditional = f.cond, random = f.random))
  .find_formula_return(f, verbose = verbose)
}

#' @export
find_formula.rlmerMod <- find_formula.merMod

#' @export
find_formula.mmrm <- find_formula.merMod

#' @export
find_formula.mmrm_fit <- find_formula.merMod

#' @export
find_formula.mmrm_tmb <- find_formula.merMod

#' @export
find_formula.cpglmm <- find_formula.merMod

#' @export
find_formula.glmmadmb <- find_formula.merMod

#' @export
find_formula.mixed <- find_formula.merMod

#' @export
find_formula.clmm <- find_formula.merMod

#' @export
find_formula.cgamm <- find_formula.merMod

#' @export
find_formula.coxme <- find_formula.merMod

#' @export
find_formula.svy2lme <- find_formula.merMod

#' @export
find_formula.HLfit <- find_formula.merMod

#' @export
find_formula.merModList <- function(x, verbose = TRUE, ...) {
  find_formula(x[[1]], ...)
}


#' @export
find_formula.sem <- function(x, verbose = TRUE, ...) {
  if (!.is_semLme(x)) {
    return(NULL)
  }

  f.cond <- x$formula
  f.random <- lapply(.findbars(f.cond), function(.x) {
    f <- safe_deparse(.x)
    stats::as.formula(paste0("~", f))
  })

  if (length(f.random) == 1L) {
    f.random <- f.random[[1]]
  }

  f.cond <- stats::as.formula(.get_fixed_effects(f.cond))
  f <- compact_list(list(conditional = f.cond, random = f.random))
  .find_formula_return(f, verbose = verbose)
}


#' @export
find_formula.lme <- function(x, verbose = TRUE, ...) {
  fm <- stats::formula(x$terms)
  .find_formula_nlme(x, fm, verbose = verbose, ...)
}

#' @export
find_formula.glmmPQL <- function(x, verbose = TRUE, ...) {
  fm <- stats::formula(x)
  .find_formula_nlme(x, fm, verbose = verbose, ...)
}

.find_formula_nlme <- function(x, fm, verbose = TRUE, ...) {
  fmr <- eval(x$call$random)
  if (!is.null(fmr) && safe_deparse(fmr)[1] == "~1") {
    check_if_installed("nlme")
    fmr <- stats::as.formula(paste("~1 |", all.vars(nlme::getGroupsFormula(x))))
  }
  ## TODO this is an intermediate fix to return the correlation variables from lme-objects
  fcorr <- x$call$correlation
  if (is.null(fcorr)) {
    fc <- NULL
  } else if (inherits(fcorr, "name")) {
    fc <- attributes(eval(fcorr))$formula
  } else {
    fc <- parse(text = safe_deparse(fcorr))[[1]]$form
  }

  f <- compact_list(list(
    conditional = fm,
    random = fmr,
    correlation = stats::as.formula(fc)
  ))
  .find_formula_return(f, verbose = verbose)
}


#' @export
find_formula.lqmm <- function(x, verbose = TRUE, ...) {
  fm <- eval(x$call$fixed)
  fmr <- safe_deparse(x$call$random)
  fmg <- safe_deparse(x$call$group)

  f <- compact_list(list(
    conditional = fm,
    random = stats::as.formula(paste0(fmr, "|", fmg))
  ))
  .find_formula_return(f, verbose = verbose)
}


#' @export
find_formula.mixor <- function(x, verbose = TRUE, ...) {
  fm <- x$call$formula

  f_id <- deparse(x$call$id)
  f_rs <- x$call$which.random.slope

  if (is.null(f_rs)) {
    fmr <- f_id
  } else {
    f_rs <- trim_ws(unlist(strsplit(safe_deparse(x$call$formula[[3]]), "+", fixed = TRUE), use.names = FALSE))[f_rs]
    fmr <- paste(f_rs, "|", f_id)
  }

  fmr <- stats::as.formula(paste("~", fmr))

  f <- compact_list(list(
    conditional = fm,
    random = fmr
  ))
  .find_formula_return(f, verbose = verbose)
}


#' @export
find_formula.MixMod <- function(x, verbose = TRUE, ...) {
  f.cond <- stats::formula(x)
  f.zi <- stats::formula(x, type = "zi_fixed")
  f.random <- stats::formula(x, type = "random")
  f.zirandom <- stats::formula(x, type = "zi_random")

  f <- compact_list(list(
    conditional = f.cond,
    random = f.random,
    zero_inflated = f.zi,
    zero_inflated_random = f.zirandom
  ))
  .find_formula_return(f, verbose = verbose)
}


#' @export
find_formula.BBmm <- function(x, verbose = TRUE, ...) {
  f.cond <- parse(text = safe_deparse(x$call))[[1]]$fixed.formula
  f.rand <- parse(text = safe_deparse(x$call))[[1]]$random.formula

  f <- compact_list(list(
    conditional = stats::as.formula(f.cond),
    random = stats::as.formula(f.rand)
  ))
  .find_formula_return(f, verbose = verbose)
}


#' @export
find_formula.mmclogit <- function(x, verbose = TRUE, ...) {
  f <- tryCatch(
    list(
      conditional = stats::formula(x),
      random = stats::as.formula(parse(text = safe_deparse(x$call))[[1]]$random)
    ),
    error = function(x) {
      NULL
    }
  )
  .find_formula_return(f, verbose = verbose)
}


#' @export
find_formula.glmm <- function(x, verbose = TRUE, ...) {
  f.cond <- stats::as.formula(x$fixedcall)
  f.random <- lapply(x$randcall, function(.x) {
    av <- all.vars(.x)
    stats::as.formula(paste0("~1|", av[length(av)]))
  })

  if (length(f.random) == 1L) {
    f.random <- f.random[[1]]
  }

  f <- compact_list(list(conditional = f.cond, random = f.random))
  .find_formula_return(f, verbose = verbose)
}


# Bayesian models --------------------------------

#' @export
find_formula.BGGM <- function(x, verbose = TRUE, ...) {
  list(conditional = x$formula)
}


#' @export
find_formula.mcmc.list <- function(x, verbose = TRUE, ...) {
  NULL
}


#' @export
find_formula.stanreg <- function(x, verbose = TRUE, ...) {
  if (inherits(x, "nlmerMod")) {
    find_formula.nlmerMod(x, ...)
  } else {
    f.cond <- stats::formula(x)
    # special handling for stan_gamm4
    if (inherits(x, "gamm4")) {
      f.random <- tryCatch(
        lapply(.findbars(stats::formula(x$glmod)), function(.x) {
          f <- safe_deparse(.x)
          stats::as.formula(paste0("~", f))
        }),
        error = function(e) {
          NULL
        }
      )
    } else {
      f.random <- lapply(.findbars(f.cond), function(.x) {
        f <- safe_deparse(.x)
        stats::as.formula(paste0("~", f))
      })
    }

    if (length(f.random) == 1L) {
      f.random <- f.random[[1]]
    }

    f.cond <- stats::as.formula(.get_fixed_effects(f.cond))
    f <- compact_list(list(conditional = f.cond, random = f.random))
    .find_formula_return(f, verbose = verbose)
  }
}


#' @export
find_formula.brmsfit <- function(x, verbose = TRUE, ...) {
  f <- stats::formula(x)

  if (object_has_names(f, "forms")) {
    mv_formula <- lapply(f$forms, .get_brms_formula)
    attr(mv_formula, "is_mv") <- "1"
    f <- mv_formula
  } else {
    f <- .get_brms_formula(f)
  }
  .find_formula_return(f, verbose = verbose)
}


#' @export
find_formula.stanmvreg <- function(x, verbose = TRUE, ...) {
  f <- stats::formula(x)
  mv_formula <- lapply(f, .get_stanmv_formula)
  attr(mv_formula, "is_mv") <- "1"
  .find_formula_return(mv_formula, verbose = verbose)
}


#' @export
find_formula.MCMCglmm <- function(x, verbose = TRUE, ...) {
  fm <- x$Fixed$formula
  fmr <- x$Random$formula

  f <- compact_list(list(conditional = fm, random = fmr))
  .find_formula_return(f, verbose = verbose)
}


#' @export
find_formula.BFBayesFactor <- function(x, verbose = TRUE, ...) {
  if (.classify_BFBayesFactor(x) == "linear") {
    fcond <- utils::tail(x@numerator, 1)[[1]]@identifier$formula
    dat_types <- utils::tail(x@numerator, 1)[[1]]@dataTypes
    frand <- names(dat_types)[which(dat_types == "random")]

    if (is_empty_object(frand)) {
      f.random <- NULL
      f.cond <- stats::as.formula(fcond)
    } else {
      f.random <- stats::as.formula(paste0("~", paste(frand, collapse = " + ")))
      for (i in frand) {
        fcond <- sub(i, "", fcond, fixed = TRUE)
      }
      while (grepl("(\\+)\\s+\\1", trim_ws(fcond))) {
        fcond <- gsub("(\\+)\\s+\\1", "\\1", trim_ws(fcond))
      }
      while (endsWith(trim_ws(fcond), "+")) {
        fcond <- gsub("(.*)\\+$", "\\1", trim_ws(fcond))
      }
      # random effects only?
      if (endsWith(trim_ws(fcond), "~")) {
        fcond <- paste(fcond, "1")
      }
      f.cond <- stats::as.formula(trim_ws(fcond))
    }
  } else if (.classify_BFBayesFactor(x) %in% c("ttest1", "ttest2")) {
    f.cond <- .safe(stats::as.formula(x@numerator[[1]]@identifier$formula))
    f.random <- NULL
  } else {
    return(NULL)
  }

  f <- compact_list(list(
    conditional = f.cond,
    random = f.random
  ))
  .find_formula_return(f, verbose = verbose)
}


# tidymodels --------------------------------------------------------------

#' @export
find_formula.model_fit <- function(x, verbose = TRUE, ...) {
  find_formula(x$fit, ...)
}


# helper ---------------------------

.get_brms_formula <- function(f) {
  f_cond <- f$formula
  f_random <- lapply(.findbars(f_cond), function(.x) {
    fm <- safe_deparse(.x)
    stats::as.formula(paste0("~", fm))
  })

  if (length(f_random) == 1L) {
    f_random <- f_random[[1]]
  }

  f_cond <- stats::as.formula(.get_fixed_effects(f_cond))

  f_zi <- f$pforms$zi
  f_zirandom <- NULL

  # auxiliary
  f_sigma <- f$pforms$sigma
  f_mu <- f$pforms$mu
  f_nu <- f$pforms$nu
  f_shape <- f$pforms$shape
  f_alpha <- f$pforms$alpha
  f_beta <- f$pforms$beta
  f_phi <- f$pforms$phi
  f_xi <- f$pforms$xi
  f_hu <- f$pforms$hu
  f_ndt <- f$pforms$ndt
  f_zoi <- f$pforms$zoi
  f_coi <- f$pforms$coi
  f_kappa <- f$pforms$kappa
  f_bias <- f$pforms$bias
  f_bs <- f$pforms$bs

  # brms formulas can also have custom names, based on variable names, e.g.:
  # brm(
  #   bf(carb ~ gear * vs) + lf(disc ~ 0 + mo(cyl)),
  #   data = mtcars,
  #   family = cumulative("probit"),
  # )
  # the lf() part is in "f$pforms" with name "disc".
  #
  # we therefore need to check whether we have additional names not yet covered
  # by the above exceptions.

  # auxiliary names
  auxiliary_names <- .brms_aux_elements()

  # check if any further pforms exist
  if (all(names(f$pforms) %in% auxiliary_names)) {
    f_custom <- NULL
  } else {
    custom_names <- setdiff(names(f$pforms), auxiliary_names)
    if (length(custom_names)) {
      f_custom <- f$pforms[custom_names]
    }
  }

  f_sigmarandom <- NULL
  f_betarandom <- NULL


  # split zero-inflated fixed from zero-inflated random

  if (!is_empty_object(f_zi)) {
    f_zirandom <- lapply(.findbars(f_zi), function(.x) {
      f <- safe_deparse(.x)
      stats::as.formula(paste0("~", f))
    })

    if (length(f_zirandom) == 1L) {
      f_zirandom <- f_zirandom[[1]]
    }

    f_zi <- stats::as.formula(paste0("~", safe_deparse(f_zi[[3L]])))
    f_zi <- stats::as.formula(.get_fixed_effects(f_zi))
  }


  # split sigma fixed from sigma random

  if (!is_empty_object(f_sigma)) {
    f_sigmarandom <- lapply(.findbars(f_sigma), function(.x) {
      f <- safe_deparse(.x)
      stats::as.formula(paste0("~", f))
    })

    if (length(f_sigmarandom) == 1L) {
      f_sigmarandom <- f_sigmarandom[[1]]
    }

    f_sigma <- stats::as.formula(paste0("~", safe_deparse(f_sigma[[3L]])))
    f_sigma <- stats::as.formula(.get_fixed_effects(f_sigma))
  }


  # split beta fixed from beta random

  if (!is_empty_object(f_beta)) {
    f_betarandom <- lapply(.findbars(f_beta), function(.x) {
      f <- safe_deparse(.x)
      stats::as.formula(paste0("~", f))
    })

    if (length(f_betarandom) == 1L) {
      f_betarandom <- f_betarandom[[1]]
    }

    f_beta <- stats::as.formula(paste0("~", safe_deparse(f_beta[[3L]])))
    f_beta <- stats::as.formula(.get_fixed_effects(f_beta))
  }


  compact_list(c(list(
    conditional = f_cond,
    random = f_random,
    zero_inflated = f_zi,
    zero_inflated_random = f_zirandom,
    sigma = f_sigma,
    sigma_random = f_sigmarandom,
    beta = f_beta,
    beta_random = f_betarandom,
    shape = f_shape,
    phi = f_phi,
    hurdle = f_hu,
    mu = f_mu,
    nu = f_nu,
    ndt = f_ndt,
    bs = f_bs,
    bias = f_bias,
    zero_one_inflated = f_zoi,
    conditional_one_inflated = f_coi,
    kappa = f_kappa
  ), f_custom))
}


.get_stanmv_formula <- function(f) {
  f_cond <- f
  f_random <- lapply(.findbars(f_cond), function(.x) {
    fm <- safe_deparse(.x)
    stats::as.formula(paste0("~", fm))
  })

  if (length(f_random) == 1L) {
    f_random <- f_random[[1]]
  }

  f_cond <- stats::as.formula(.get_fixed_effects(f_cond))

  compact_list(list(
    conditional = f_cond,
    random = f_random
  ))
}


# Find formula for zero-inflated regressions, where
# zero-inflated part is separated by | from count part
.zeroinf_formula <- function(x, separator = "\\|", verbose = TRUE) {
  f <- tryCatch(stats::formula(x), error = function(x) NULL)

  if (is.null(f)) {
    return(NULL)
  }

  f <- trim_ws(unlist(strsplit(safe_deparse(f), separator), use.names = FALSE))

  c.form <- stats::as.formula(f[1])
  if (length(f) == 2) {
    zi.form <- stats::as.formula(paste0("~", f[2]))
  } else {
    zi.form <- NULL
  }

  ## TODO could be extended to all find_formula()

  # fix dot-formulas
  c.form <- .dot_formula(f = c.form, model = x)

  # fix dot-formulas
  zi.form <- tryCatch(
    {
      if (as.character(zi.form[2]) == ".") {
        resp <- safe_deparse(c.form[2])
        pred <- setdiff(colnames(.recover_data_from_environment(x)), resp)
        zi.form <- stats::as.formula(paste(resp, "~", paste(pred, collapse = " + ")))
      }
      zi.form
    },
    error = function(e) {
      zi.form
    }
  )


  f <- compact_list(list(conditional = c.form, zero_inflated = zi.form))
  .find_formula_return(f, verbose = verbose)
}


# try to guess "full" formula for dot-abbreviation, e.g.
# lm(mpg ~., data = mtcars)
.dot_formula <- function(f, model) {
  # fix dot-formulas
  tryCatch(
    {
      if (as.character(f[[3]])[1] == ".") {
        resp <- safe_deparse(f[[2]])
        pred <- setdiff(colnames(.recover_data_from_environment(model)), resp)
        f <- stats::as.formula(paste(resp, "~", paste(pred, collapse = " + ")))
      }
      f
    },
    error = function(e) {
      f
    }
  )
}


.fix_gamm_random_effect <- function(x) {
  g_in_terms <- length(x) > 1 && x[length(x)] == "g"
  xr_in_terms <- length(x) > 1 && x[length(x)] == "Xr"
  x <- x[!(grepl("(Xr\\.\\d|g\\.\\d)", x) | x %in% c("Xr", "g"))]
  # exceptions, if random effect is named g
  if (!length(x) && isTRUE(g_in_terms)) {
    x <- "g"
  }
  if (!length(x) && isTRUE(xr_in_terms)) {
    x <- "Xr"
  }
  x
}


.fix_gamm4_random_effect <- function(f) {
  if (inherits(f, "formula")) {
    f <- list(f)
  }
  len <- length(f)
  keep <- vapply(f, function(i) {
    i <- gsub("(~1| | \\|)", "", deparse(i))
    !any(grepl("(Xr\\.\\d|g\\.\\d)", i) | i %in% c("Xr", "g"))
  }, TRUE)
  f <- compact_list(f[keep])
  # exceptions, if random effect is named Xr
  if (!length(f) && len > 1L) {
    f <- list(stats::as.formula("~1 | Xr"))
  }
  f
}


# Helpers and Methods -----------------------------------------------------

.find_formula_return <- function(f, verbose = TRUE) {
  if (is.null(f)) {
    return(NULL)
  }

  formula_ok(f, verbose = verbose)
  class(f) <- c("insight_formula", class(f))
  f
}


.check_formula_for_T <- function(f, action = "warning", prefix_msg = NULL, verbose = TRUE) {
  f <- safe_deparse(f[[1]])

  if (is_empty_object(f)) {
    return(TRUE)
  }

  if (grepl("(.*)poly\\((.*),\\s*raw\\s*=\\s*T\\)", f)) {
    if (verbose) {
      msg <- c(
        prefix_msg,
        "Looks like you are using `poly()` with \"raw = T\". This results in unexpected behaviour, because `all.vars()` considers `T` as variable.", # nolint
        "Please use \"raw = TRUE\"."
      )
      format_alert(msg, type = action)
    }
    return(FALSE)
  }
  return(TRUE)
}


# formulas with $, like "lm(mtcars$mpg ~ mtcars$hp), may cause problems
# in various functions throughout the easystats packages. We warn the user
# here...

.check_formula_for_dollar <- function(f, action = "warning", prefix_msg = NULL, verbose = TRUE) {
  if (is_empty_object(f)) {
    return(TRUE)
  }

  if (any(grepl("$", safe_deparse(f[[1]]), fixed = TRUE))) {
    fc <- try(.formula_clean(f[[1]]), silent = TRUE)
    if (inherits(fc, "try-error")) {
      format_error(attributes(fc)$condition$message)
    }
    if (verbose) {
      msg <- c(
        prefix_msg,
        paste0(
          "Using `$` in model formulas can produce unexpected results. Specify your model using the `data` argument instead.", # nolint
          "\n  Try: ", fc$formula, ", data = ", fc$data
        )
      )
      format_alert(msg, type = action)
    }
    return(FALSE)
  }
  return(TRUE)
}


# formulas with an index data frame, like "lm(mtcars[, "mpg"] ~ mtcars$hp), may
# cause problems in various functions throughout the easystats packages. We
# warn the user here...

.check_formula_index_df <- function(f, x, action = "warning", prefix_msg = NULL, verbose = TRUE) {
  if (is_empty_object(f)) {
    return(TRUE)
  }
  resp <- .safe(safe_deparse(f$conditional[[2]]))
  if (!is.null(resp) && any(grepl("\\b\\w+\\[.*?,.*?\\]", resp))) {
    if (verbose) {
      msg <- c(
        prefix_msg,
        "Using indexed data frames, such as `df[, 5]`, as model response can produce unexpected results. Specify your model using the literal name of the response variable instead." # nolint
      )
      format_alert(msg, type = action)
    }
    return(FALSE)
  }
  return(TRUE)
}


# formulas with non-syntactic names, where backticks are used, may cause
# problems. warn user here

.check_formula_backticks <- function(f, action = "warning", prefix_msg = NULL, verbose = TRUE) {
  if (is_empty_object(f)) {
    return(TRUE)
  }
  resp <- .safe(safe_deparse(f$conditional))
  if (!is.null(resp) && any(grepl("`", resp, fixed = TRUE))) {
    if (verbose) {
      bad_name <- gsub("(.*)`(.*)`(.*)", "\\2", resp)
      msg <- c(
        prefix_msg,
        paste0(
          "Looks like you are using syntactically invalid variable names, quoted in backticks: `",
          bad_name,
          "`. This may result in unexpected behaviour. Please rename your variables (e.g., `",
          make.names(bad_name),
          "` instead of `",
          bad_name,
          "`) and fit the model again."
        )
      )
      format_alert(msg, type = action)
    }
    return(FALSE)
  }
  return(TRUE)
}


.formula_clean <- function(f) {
  fc <- as.character(f)
  LHS <- fc[2]
  RHS <- fc[3]

  pattern <- "[\\s*+:()|^,\\-\\/]" # was: "[\\s\\*\\+:\\-\\|/\\(\\)\\^,]"

  parts <- trim_ws(unlist(strsplit(split = pattern, x = LHS, perl = TRUE), use.names = FALSE))
  d_LHS <- unique(gsub("(.*)\\$(.*)", "\\1", grep("(.*)\\$(.*)", parts, value = TRUE)))

  parts <- trim_ws(unlist(strsplit(split = pattern, x = RHS, perl = TRUE), use.names = FALSE))
  d_RHS <- unique(gsub("(.*)\\$(.*)", "\\1", grep("(.*)\\$(.*)", parts, value = TRUE)))

  if (n_unique(c(d_LHS, d_RHS)) > 1L) {
    format_error("Multiple data objects present in formula. Specify your model using the `data` argument instead.")
  } else {
    d <- unique(d_RHS)
  }
  LHS_clean <- gsub(paste0(d_LHS, "\\$"), "", LHS)
  RHS_clean <- gsub(paste0(d_RHS, "\\$"), "", RHS)

  list(data = d, formula = paste(LHS_clean, fc[1], RHS_clean))
}


# methods -------------------------

#' @export
format.insight_formula <- function(x, what = c("conditional", "random"), ...) {
  # The purpose of this function is to flatten the formula

  # Start by first part (conditional by default)
  ft <- format(x[[1]])

  # Wrap random in brackets
  if ("random" %in% names(x)) {
    x[["random"]] <- paste0("(", format(x[["random"]]), ")")
  }

  # Add all the components
  for (part in what[-1]) {
    if (part %in% names(x)) {
      ft <- paste0(ft, " + ", format(x[[part]]))
    }
  }

  ft
}
