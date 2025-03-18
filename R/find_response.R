#' @title Find name of the response variable
#' @name find_response
#'
#' @description Returns the name(s) of the response variable(s) from a model object.
#'
#' @param x A fitted model.
#' @param combine Logical, if `TRUE` and the response is a matrix-column,
#'   the name of the response matches the notation in formula, and would for
#'   instance also contain patterns like `"cbind(...)"`. Else, the original
#'   variable names from the matrix-column are returned. See 'Examples'.
#' @param component Character, if `x` is a joint model, this argument can be
#'   used to specify which component to return. Possible values are
#'  `"conditional"`, `"survival"` or `"all"`.
#' @param ... Currently not used.
#'
#' @return The name(s) of the response variable(s) from `x` as character
#'   vector, or `NULL` if response variable could not be found.
#'
#' @examplesIf require("lme4", quietly = TRUE)
#' data(cbpp, package = "lme4")
#' cbpp$trials <- cbpp$size - cbpp$incidence
#' m <- glm(cbind(incidence, trials) ~ period, data = cbpp, family = binomial)
#'
#' find_response(m, combine = TRUE)
#' find_response(m, combine = FALSE)
#' @export
find_response <- function(x, combine = TRUE, ...) {
  UseMethod("find_response")
}


#' @export
find_response.default <- function(x, combine = TRUE, ...) {
  f <- find_formula(x, verbose = FALSE)

  if (is.null(f)) {
    return(NULL)
  }

  # this is for multivariate response models,
  # where we have a list of formulas
  if (is_multivariate(f)) {
    resp <- unlist(lapply(f, function(i) safe_deparse(i$conditional[[2L]])))
  } else {
    resp <- safe_deparse(f$conditional[[2L]])
  }

  check_cbind(resp, combine, model = x)
}


#' @export
find_response.brmsfit <- function(x, combine = TRUE, ...) {
  f <- find_formula(x, verbose = FALSE)

  if (is.null(f)) {
    return(NULL)
  }

  # this is for multivariate response models,
  # where we have a list of formulas
  if (is_multivariate(f)) {
    resp <- unlist(lapply(f, function(i) {
      resp_formula <- safe_deparse(i$conditional[[2L]])
      if (grepl("|", resp_formula, fixed = TRUE)) {
        resp_formula <- all.vars(i$conditional[[2L]])
      }
      resp_formula
    }))
  } else {
    resp <- safe_deparse(f$conditional[[2L]])
    if (grepl("|", resp, fixed = TRUE)) {
      resp <- all.vars(f$conditional[[2L]])
    }
  }

  check_cbind(resp, combine, model = x)
}


#' @export
find_response.logitr <- function(x, ...) {
  get_call(x)$outcome
}


#' @export
find_response.model_fit <- function(x, combine = TRUE, ...) {
  find_response(x$fit, combine = combine, ...)
}


#' @export
find_response.bfsl <- function(x, combine = TRUE, ...) {
  resp <- find_response.default(x, combine = combine)
  if (is.null(resp)) {
    resp <- "y"
  }
  resp
}


#' @export
find_response.selection <- function(x, combine = TRUE, ...) {
  f <- find_formula(x, verbose = FALSE)
  resp <- c(
    safe_deparse(f$conditional$selection[[2L]]),
    safe_deparse(f$conditional$outcome[[2L]])
  )
  check_cbind(resp, combine, model = x)
}


#' @export
find_response.mediate <- function(x, combine = TRUE, ...) {
  f <- find_formula(x, verbose = FALSE)

  if (is.null(f)) {
    return(NULL)
  }

  resp <- c(safe_deparse(f$mediator$conditional[[2L]]), safe_deparse(f$outcome$conditional[[2L]]))
  check_cbind(resp, combine, model = x)
}


#' @export
find_response.tune_results <- function(x, combine = TRUE, ...) {
  att <- attributes(x)
  if (any(names(att) == "outcomes")) {
    resp <- att$outcomes
  } else {
    return(NULL)
  }
  check_cbind(resp, combine, model = x)
}


#' @export
find_response.workflow <- function(x, combine = TRUE, ...) {
  insight::check_if_installed("tune")
  resp <- tune::outcome_names(x)
  check_cbind(resp, combine, model = x)
}


#' @export
find_response.mjoint <- function(x, combine = TRUE, component = "conditional", ...) {
  component <- validate_argument(component, c("conditional", "survival", "all"))
  f <- find_formula(x, verbose = FALSE)

  if (is.null(f)) {
    return(NULL)
  }

  conditional <- unlist(lapply(f[startsWith(names(f), "conditional")], function(i) safe_deparse(i[[2L]])))
  survial <- safe_deparse(f$survival[[2L]])

  resp <- switch(component,
    conditional = conditional,
    survial = survial,
    all = c(conditional, survial)
  )

  unlist(lapply(resp, check_cbind, combine = combine, model = x))
}


#' @rdname find_response
#' @export
find_response.joint <- function(x, combine = TRUE, component = "conditional", ...) {
  component <- validate_argument(component, c("conditional", "survival", "all"))
  f <- find_formula(x, verbose = FALSE)

  if (is.null(f)) {
    return(NULL)
  }

  conditional <- safe_deparse(f$conditional[[2L]])
  survial <- safe_deparse(f$survival[[2L]])

  resp <- switch(component,
    conditional = conditional,
    survial = survial,
    all = c(conditional, survial)
  )

  unlist(lapply(resp, check_cbind, combine = combine, model = x))
}


# utils ---------------------


# should not be called for brms-models!
check_cbind <- function(resp, combine, model) {
  if (any(.string_contains("|", resp))) {
    # check for brms Additional Response Information
    r1 <- trim_ws(sub("(.*)\\|(.*)", "\\1", resp))
    r2 <- trim_ws(sub("(.*)\\|(.*)\\(([^,)]*).*", "\\3", resp))
    # check for "resp_thres" and similar patterns
    r_resp <- trim_ws(unlist(strsplit(resp, "|", fixed = TRUE))[2])
    if (grepl("^(resp_thres|thres|resp_weights|weights|resp_se|se|resp_cens|cens)", r_resp)) {
      r3 <- trim_ws(sub("=", "", sub("(.*)\\(([^=)]*)(.*)\\)", "\\3", r_resp), fixed = TRUE))
      numeric_values <- suppressWarnings(as.numeric(r2))
      r2 <- r2[is.na(numeric_values)]
      if (is.na(suppressWarnings(as.numeric(r3)))) {
        if (length(r2)) {
          r2 <- c(r2, r3)
        } else {
          r2 <- r3
        }
      }
      resp <- compact_character(c(r1, r2))
    } else if (grepl("^(resp_trunc|trunc|resp_mi|mi)", r_resp)) {
      # for models with "trunc()", "mi()" etc. in response, which cannot have
      # variables, omit that part (see #779)
      resp <- r1
    } else if (grepl("^(resp_trials|trials|resp_cat|cat|resp_dec|dec)", r_resp)) {
      if (is.na(suppressWarnings(as.numeric(r2)))) {
        # if we have a variable, add it
        resp <- compact_character(c(r1, r2))
      } else {
        # else, if we have a constant (like "trials(1)"), omit it
        resp <- r1
      }
    } else {
      resp <- c(r1, r2)
    }
  } else if (inherits(model, "DirichletRegModel")) {
    resp <- model$varnames
  } else {
    # if we have more than one string for the response, paste them together
    # "all.vars()" will take care of extracting the correct variables.
    resp_combined_string <- paste(resp, collapse = "+")
    # create an expression, so all.vars() works similar like for formulas
    resp_combined <- tryCatch(all.vars(str2lang(resp_combined_string)),
      error = function(e) resp_combined_string
    )
    # if we do not want to combine, or if we just have one variable as
    # response, we want to return the bare name
    if (!combine || length(resp_combined) == 1) {
      # if a named vector (e.g. for multivariate response), add back names
      if (!is.null(names(resp)) && length(names(resp)) == length(resp_combined)) {
        resp_combined <- stats::setNames(resp_combined, names(resp))
      }
      resp <- resp_combined
    }
  }

  # exception
  if (inherits(model, "clogit") && startsWith(resp[1], "rep(") && length(resp) == 3L) {
    resp <- c(paste0(resp[1], resp[2]), resp[3])
  }

  resp
}
