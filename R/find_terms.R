#' @title Find all model terms
#' @name find_terms
#'
#' @description Returns a list with the names of all terms, including
#'   response value and random effects, "as is". This means, on-the-fly
#'   tranformations or arithmetic expressions like \code{log()}, \code{I()},
#'   \code{as.factor()} etc. are preserved.
#'
#' @inheritParams find_formula
#' @inheritParams find_predictors
#'
#' @return A list with (depending on the model) following elements (character
#'    vectors):
#'    \itemize{
#'      \item \code{response}, the name of the response variable
#'      \item \code{conditional}, the names of the predictor variables from the \emph{conditional} model (as opposed to the zero-inflated part of a model)
#'      \item \code{random}, the names of the random effects (grouping factors)
#'      \item \code{zero_inflated}, the names of the predictor variables from the \emph{zero-inflated} part of the model
#'      \item \code{zero_inflated_random}, the names of the random effects (grouping factors)
#'      \item \code{dispersion}, the name of the dispersion terms
#'      \item \code{instruments}, the names of instrumental variables
#'    }
#'    Returns \code{NULL} if no terms could be found (for instance, due to
#'    problems in accessing the formula).
#'
#' @note The difference to \code{\link{find_variables}} is that \code{find_terms()}
#'   may return a variable multiple times in case of multiple transformations
#'   (see examples below), while \code{find_variables()} returns each variable
#'   name only once.
#'
#' @examples
#' if (require("lme4")) {
#'   data(sleepstudy)
#'   m <- lmer(
#'     log(Reaction) ~ Days + I(Days^2) + (1 + Days + exp(Days) | Subject),
#'     data = sleepstudy
#'   )
#'
#'   find_terms(m)
#' }
#' @export
find_terms <- function(x, flatten = FALSE, verbose = TRUE, ...) {
  UseMethod("find_terms")
}

#' @export
find_terms.default <- function(x, flatten = FALSE, verbose = TRUE, ...) {
  f <- find_formula(x, verbose = verbose)

  if (is.null(f)) {
    return(NULL)
  }

  resp <- find_response(x, verbose = FALSE)

  if (is_multivariate(f) || isTRUE(attributes(f)$two_stage)) {
    l <- lapply(f, .get_variables_list, resp = resp)
  } else {
    l <- .get_variables_list(f, resp)
  }

  if (flatten) {
    unique(unlist(l))
  } else {
    l
  }
}


#' @export
find_terms.aovlist <- function(x, flatten = FALSE, verbose = TRUE, ...) {
  resp <- find_response(x, verbose = FALSE)
  f <- find_formula(x, verbose = verbose)[[1]]

  l <- .get_variables_list_aovlist(f, resp)
  if (flatten) {
    unlist(l)
  } else {
    l
  }
}

#' @export
find_terms.afex_aov <- function(x, flatten = FALSE, verbose = TRUE, ...) {
  resp <- find_response(x, verbose = FALSE)

  if (length(attr(x, "within"))==0L) {
    l <- find_terms(x$lm, flatten = FALSE, verbose = TRUE, ...)
    l$response <- resp
  } else {
    f <- find_formula(x, verbose = verbose)[[1]]
    l <- .get_variables_list_aovlist(f, resp)
  }

  if (flatten) {
    unlist(l)
  } else {
    l
  }
}


.get_variables_list <- function(f, resp = NULL) {
  # exception for formula w/o response
  if (is.null(resp) || !.is_empty_object(resp)) {
    f$response <- sub("(.*)::(.*)", "\\2", .safe_deparse(f$conditional[[2L]]))
    f$conditional <- .safe_deparse(f$conditional[[3L]])
  } else {
    f$conditional <- .safe_deparse(f$conditional[[2L]])
  }

  f <- lapply(f, function(.x) {
    if (is.list(.x)) {
      .x <- sapply(.x, .formula_to_string)
    } else {
      if (!is.character(.x)) .x <- .safe_deparse(.x)
    }
    .x
  })

  # protect "-1"
  f$conditional <- gsub("(-1|- 1)(?![^(]*\\))", "#1", f$conditional, perl = TRUE)

  f <- lapply(f, function(.x) {
    pattern <- "[*+:|\\-\\/](?![^(]*\\))" # was: "[\\*\\+:\\-\\|/](?![^(]*\\))"
    f_parts <- gsub("~", "", .trim(unlist(strsplit(split = pattern, x = .x, perl = TRUE))))
    # if user has used namespace in formula-functions, these are returned
    # as empty elements. remove those here
    if (any(nchar(f_parts) == 0)) {
      f_parts <- f_parts[-which(nchar(f_parts) == 0)]
    }
    .remove_backticks_from_string(unique(f_parts))
  })


  # remove "1" and "0" from variables in random effects

  if (.obj_has_name(f, "random")) {
    pos <- which(f$random %in% c("1", "0"))
    if (length(pos)) f$random <- f$random[-pos]
  }

  if (.obj_has_name(f, "zero_inflated_random")) {
    pos <- which(f$zero_inflated_random %in% c("1", "0"))
    if (length(pos)) f$zero_inflated_random <- f$zero_inflated_random[-pos]
  }

  # restore -1
  need_split <- grepl("#1$", f$conditional)
  if (any(need_split)) {
    f$conditional <- c(
      f$conditional[!need_split],
      .trim(unlist(strsplit(f$conditional[need_split], " ", fixed = TRUE)))
    )
  }
  f$conditional <- gsub("#1", "-1", f$conditional, fixed = TRUE)

  # reorder, so response is first
  .compact_list(f[c(length(f), 1:(length(f) - 1))])
}

.get_variables_list_aovlist <- function(f, resp = NULL) {
  i <- sapply(f[[3]], function(x) {
    x <- as.character(x)
    x[1] == "Error" && length(x) > 1
  })
  error <- utils::capture.output(print(f[[3]][i][[1]]))
  f[[3]][i] <- NULL

  list(
    response = resp,
    conditional = attr(stats::terms.formula(f), "term.labels"),
    error =  error
  )
}

.formula_to_string <- function(f) {
  if (!is.character(f)) f <- .safe_deparse(f)
  f
}
