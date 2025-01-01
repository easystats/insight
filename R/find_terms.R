#' @title Find all model terms
#' @name find_terms
#'
#' @description Returns a list with the names of all terms, including response
#'   value and random effects, "as is". This means, on-the-fly tranformations
#'   or arithmetic expressions like `log()`, `I()`, `as.factor()` etc. are
#'   preserved.
#'
#' @param as_term_labels Logical, if `TRUE`, extracts model formula and tries to
#'   access the `"term.labels"` attribute. This should better mimic the `terms()`
#'   behaviour even for those models that do not have such a method, but may be
#'   insufficient, e.g. for mixed models.
#' @inheritParams find_formula
#' @inheritParams find_predictors
#'
#' @return A list with (depending on the model) following elements (character
#' vectors):
#'
#' - `response`, the name of the response variable
#' - `conditional`, the names of the predictor variables from the *conditional*
#'    model (as opposed to the zero-inflated part of a model)
#' - `random`, the names of the random effects (grouping factors)
#' - `zero_inflated`, the names of the predictor variables from the *zero-inflated* part of the model
#' - `zero_inflated_random`, the names of the random effects (grouping factors)
#' - `dispersion`, the name of the dispersion terms
#' - `instruments`, the names of instrumental variables
#'
#' Returns `NULL` if no terms could be found (for instance, due to
#' problems in accessing the formula).
#'
#' @inheritSection find_predictors Parameters, Variables, Predictors and Terms
#'
#' @note The difference to [`find_variables()`] is that `find_terms()`
#'   may return a variable multiple times in case of multiple transformations
#'   (see examples below), while `find_variables()` returns each variable
#'   name only once.
#'
#' @examplesIf require("lme4", quietly = TRUE)
#' data(sleepstudy, package = "lme4")
#' m <- suppressWarnings(lme4::lmer(
#'   log(Reaction) ~ Days + I(Days^2) + (1 + Days + exp(Days) | Subject),
#'   data = sleepstudy
#' ))
#'
#' find_terms(m)
#'
#' # sometimes, it is necessary to retrieve terms from "term.labels" attribute
#' m <- lm(mpg ~ hp * (am + cyl), data = mtcars)
#' find_terms(m, as_term_labels = TRUE)
#' @export
find_terms <- function(x, ...) {
  UseMethod("find_terms")
}

#' @rdname find_terms
#' @export
find_terms.default <- function(x,
                               flatten = FALSE,
                               as_term_labels = FALSE,
                               verbose = TRUE,
                               ...) {
  f <- find_formula(x, verbose = verbose)

  if (is.null(f)) {
    return(NULL)
  }

  # mimics original "terms()" behaviour, leads to slightly different results
  if (isTRUE(as_term_labels)) {
    return(lapply(f, function(i) attr(stats::terms(i), "term.labels")))
  }

  resp <- find_response(x, verbose = FALSE)

  if (is_multivariate(f) || isTRUE(attributes(f)$two_stage)) {
    l <- lapply(f, .get_variables_list, resp = resp, model = x)
  } else {
    l <- .get_variables_list(f, resp, model = x)
  }

  if (flatten) {
    unique(unlist(l, use.names = FALSE))
  } else {
    l
  }
}


.find_terms <- function(f, response) {
  out <- lapply(f, function(i) {
    if (is.list(i)) {
      .find_terms(i, response = NULL)
    } else {
      f_terms <- unname(attr(stats::terms(i), "term.labels"))
      sub("(.*)::(.*)", "\\2", f_terms)
    }
  })

  compact_list(c(list(response = response), out))
}


#' @export
find_terms.aovlist <- function(x, flatten = FALSE, verbose = TRUE, ...) {
  resp <- find_response(x, verbose = FALSE)
  f <- find_formula(x, verbose = verbose)[[1]]

  l <- .get_variables_list_aovlist(f, resp)
  if (flatten) {
    unique(unlist(l, use.names = FALSE))
  } else {
    l
  }
}


#' @export
find_terms.afex_aov <- function(x, flatten = FALSE, verbose = TRUE, ...) {
  resp <- find_response(x, verbose = FALSE)

  if (length(attr(x, "within")) == 0L) {
    l <- find_terms(x$lm, flatten = FALSE, verbose = TRUE, ...)
    l$response <- resp
  } else {
    f <- find_formula(x, verbose = verbose)[[1]]
    l <- .get_variables_list_aovlist(f, resp)
  }

  if (flatten) {
    unique(unlist(l, use.names = FALSE))
  } else {
    l
  }
}


#' @export
find_terms.bfsl <- function(x, flatten = FALSE, verbose = TRUE, ...) {
  resp <- find_response(x, verbose = FALSE)
  f <- find_formula(x, verbose = verbose)

  if (is.null(f)) {
    fx <- "x"
  } else {
    fx <- f[[1]][[3]]
  }
  l <- list(conditional = c(resp, fx))

  if (flatten) {
    unique(unlist(l, use.names = FALSE))
  } else {
    l
  }
}


# unsupported ------------------


#' @export
find_terms.mipo <- function(x, flatten = FALSE, ...) {
  l <- list(conditional = unique(as.vector(summary(x)$term)))
  if (flatten) {
    unique(unlist(l, use.names = FALSE))
  } else {
    l
  }
}


# helper -----------------------


.get_variables_list <- function(f, resp = NULL, model = NULL) {
  # exception for formula w/o response
  if (is.null(resp) || !is_empty_object(resp)) {
    f$response <- sub("(.*)::(.*)", "\\2", safe_deparse(f$conditional[[2L]]))
    f$conditional <- safe_deparse(f$conditional[[3L]])
  } else {
    f$conditional <- safe_deparse(f$conditional[[2L]])
  }

  f <- lapply(f, function(.x) {
    if (is.list(.x)) {
      .x <- vapply(.x, .formula_to_string, character(1))
    } else if (!is.character(.x)) {
      .x <- safe_deparse(.x)
    }
    .x
  })

  # save original response
  original_response <- f$response

  # protect "-1"
  f$conditional <- gsub("(-1|- 1)(?![^(]*\\))", "#1", f$conditional, perl = TRUE)
  f$response <- gsub("(-1|- 1)(?![^(]*\\))", "#1", f$response, perl = TRUE)

  # This regular expression matches any of the characters *, +, :, |, -, or /,
  # unless they are preceded by a ^ and followed by a closing parenthesis ).
  f <- lapply(f, function(.x) {
    pattern <- "(?<!\\^)[*+:|\\-\\/](?![^(]*\\))" # was: "[\\*\\+:\\-\\|/](?![^(]*\\))"
    f_parts <- gsub("~", "", trim_ws(unlist(
      strsplit(split = pattern, x = .x, perl = TRUE),
      use.names = FALSE
    )), fixed = TRUE)
    # if user has used namespace in formula-functions, these are returned
    # as empty elements. remove those here
    if (!all(nzchar(f_parts, keepNA = TRUE))) {
      f_parts <- f_parts[-which(!nzchar(f_parts, keepNA = TRUE))]
    }
    text_remove_backticks(unique(f_parts))
  })

  # exceptions where we want to preserve the response value come here
  # - lm(1 / Sepal.Length ~ Species, data = iris)
  if (!is.null(original_response) && !is_empty_object(original_response) && startsWith(original_response, "1/")) { # nolint
    f$response <- original_response
  }

  # for brms-models, we need to remove "Intercept", which is a special notation
  if (inherits(model, "brmsfit")) {
    f <- lapply(f, function(i) {
      compact_character(gsub("\\QIntercept\\E", "", i))
    })
  }

  # remove "1" and "0" from variables in random effects

  if (object_has_names(f, "random")) {
    pos <- which(f$random %in% c("1", "0"))
    if (length(pos)) f$random <- f$random[-pos]
  }

  if (object_has_names(f, "zero_inflated_random")) {
    pos <- which(f$zero_inflated_random %in% c("1", "0"))
    if (length(pos)) f$zero_inflated_random <- f$zero_inflated_random[-pos]
  }

  # restore -1
  need_split <- endsWith(f$conditional, "#1")
  if (any(need_split)) {
    f$conditional <- c(
      f$conditional[!need_split],
      trim_ws(unlist(strsplit(f$conditional[need_split], " ", fixed = TRUE), use.names = FALSE))
    )
  }

  f$conditional <- gsub("#1", "-1", f$conditional, fixed = TRUE)
  f$response <- gsub("#1", "-1", f$response, fixed = TRUE)

  # reorder, so response is first
  compact_list(f[c(length(f), 1:(length(f) - 1))])
}


.get_variables_list_aovlist <- function(f, resp = NULL) {
  i <- vapply(f[[3]], function(x) {
    x <- as.character(x)
    x[1] == "Error" && length(x) > 1
  }, TRUE)
  error <- utils::capture.output(print(f[[3]][i][[1]]))
  f[[3]][i] <- NULL
  f[[3]] <- f[[3]][[2]]
  f[[3]] <- as.name(paste(attr(stats::terms.formula(f), "term.labels"), collapse = "+"))

  l <- .get_variables_list(f, resp)
  names(l) <- c("response", "conditional")
  l$error <- error
  l
}

.formula_to_string <- function(f) {
  if (!is.character(f)) f <- safe_deparse(f)
  f
}
