#' @title Find names of all variables
#' @name find_variables
#'
#' @description Returns a list with the names of all variables, including
#'   response value and random effects, "as is". This means, on-the-fly
#'   tranformations like \code{log()}, \code{I()}, \code{as.factor()} etc.
#'   are preserved.
#'
#' @inheritParams find_formula
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
find_variables <- function(x, ...) {
  f <- find_formula(x)
  if (is_multivariate(f)) {
    lapply(f, get_variables_list)
  } else {
    get_variables_list(f)
  }
}

get_variables_list <- function(f) {
  f$response <- deparse(f$conditional[[2L]], width.cutoff = 500L)
  f$conditional <- deparse(f$conditional[[3L]], width.cutoff = 500L)

  f <- lapply(f, function(.x) {
    if (!is.character(.x)) .x <- deparse(.x, width.cutoff = 500L)
    .x
  })

  f <- lapply(f, function(.x) {
    gsub("~", "", trim(unlist(strsplit(split = "[\\*\\+\\:\\-\\|](?![^(]*\\))", x = .x, perl = TRUE))))
  })

  # reorder, so response is first
  compact_list(f[c(length(f), 1:(length(f) - 1))])
}