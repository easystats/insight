#' @title Find names of all variables
#' @name find_variables
#'
#' @description Returns a list with the names of all variables, including
#'   response value and random effects, "as is". This means, on-the-fly
#'   tranformations like \code{log()}, \code{I()}, \code{as.factor()} etc.
#'   are preserved.
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
#'
#' @note The difference to \code{\link{find_terms}} is that \code{find_variables()}
#'   may return a variable multiple times in case of multiple transformations
#'   (see examples below), while \code{find_terms()} returns each term only
#'   once.
#'
#' @examples
#' library(lme4)
#' data(sleepstudy)
#' m <- lmer(
#'   log(Reaction) ~ Days + I(Days^2) + (1 + Days + exp(Days) | Subject),
#'   data = sleepstudy
#' )
#'
#' find_variables(m)
#'
#' @export
find_variables <- function(x, flatten = FALSE, ...) {
  f <- find_formula(x)
  if (is_multivariate(f)) {
    l <- lapply(f, get_variables_list)
  } else {
    l <- get_variables_list(f)
  }

  if (flatten) {
    unique(unlist(l))
  } else {
    l
  }
}

get_variables_list <- function(f) {
  f$response <- .safe_deparse(f$conditional[[2L]])
  f$conditional <- .safe_deparse(f$conditional[[3L]])

  f <- lapply(f, function(.x) {
    if (is.list(.x)) {
      .x <- sapply(.x, .formula_to_string)
    } else {
      if (!is.character(.x)) .x <- .safe_deparse(.x)
    }
    .x
  })

  f <- lapply(f, function(.x) {
    f_parts <- gsub("~", "", trim(unlist(strsplit(split = "[\\*\\+\\:\\-\\|](?![^(]*\\))", x = .x, perl = TRUE))))
    # if user has used namespace in formula-functions, these are returned
    # as empty elements. rempove those here
    if (any(nchar(f_parts) == 0)) {
      f_parts <- f_parts[-which(nchar(f_parts) == 0)]
    }
    f_parts
  })


  # remove "1" and "0" from variables in random effects

  if (obj_has_name(f, "random")) {
    pos <- which(f$random %in% c("1", "0"))
    if (length(pos)) f$random <- f$random[-pos]
  }

  if (obj_has_name(f, "zero_inflated_random")) {
    pos <- which(f$zero_inflated_random %in% c("1", "0"))
    if (length(pos)) f$zero_inflated_random <- f$zero_inflated_random[-pos]
  }


  # reorder, so response is first
  compact_list(f[c(length(f), 1:(length(f) - 1))])
}


.formula_to_string <- function(f) {
  if (!is.character(f)) f <- .safe_deparse(f)
  f
}
