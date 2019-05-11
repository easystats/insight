#' @title Get clean names of model terms
#' @name clean_names
#'
#' @description This function "cleans" names of model terms (or a character
#'   vector with such names) by removing patterns like \code{log()} or
#'   \code{as.factor()} etc.
#'
#' @param x A fitted model, or a character vector.
#'
#' @return The "cleaned" variable names as character vector, i.e. pattern
#'   like \code{s()} for splines or \code{log()} are removed from
#'   the model terms.
#'
#' @note If \code{x} is a regression model, this function is equal to calling
#'   \code{find_terms()}.
#'
#' @examples
#' # example from ?stats::glm
#' counts <- c(18, 17, 15, 20, 10, 20, 25, 13, 12)
#' outcome <- c(gl(3, 1, 9))
#' treatment <- gl(3, 3)
#' m <- glm(counts ~ log(outcome) + as.factor(treatment), family = poisson())
#' clean_names(m)
#' @export
clean_names <- function(x) {
  UseMethod("clean_names")
}


#' @export
clean_names.default <- function(x) {
  cleaned <- unname(find_terms(x, flatten = TRUE))
  .remove_values(cleaned, c("1", "0"))
}


#' @export
clean_names.character <- function(x) {
  .remove_pattern_from_names(x)
}


.remove_pattern_from_names <- function(x, ignore_asis = FALSE) {
  # return if x is empty
  if (is_empty_string(x)) return("")

  # for gam-smoothers/loess, remove s()- and lo()-function in column name
  # for survival, remove strata(), and so on...
  pattern <- c(
    "as.factor", "factor", "offset", "log1p", "log10", "log2", "log-log",
    "log", "lag", "diff", "pspline", "poly", "catg", "asis", "matrx", "pol",
    "strata", "strat", "scale", "scored", "interaction", "sqrt", "lsp", "rcs",
    "pb", "lo", "bs", "ns", "t2", "te", "ti", "tt", "mi", "mo", "gp", "s", "I"
  )

  # do we have a "log()" pattern here? if yes, get capture region
  # which matches the "cleaned" variable name
  cleaned <- sapply(1:length(x), function(i) {
    for (j in 1:length(pattern)) {
      # remove possible  namespace
      x[i] <- sub("(.*)::(.*)", "\\2", x[i])
      if (pattern[j] == "offset") {
        x[i] <- trim(unique(sub("^offset\\(([^-+ )]*).*", "\\1", x[i])))
      } else if (pattern[j] == "I") {
        if (!ignore_asis) x[i] <- trim(unique(sub("I\\((\\w*).*", "\\1", x[i])))
      } else if (pattern[j] == "asis") {
        if (!ignore_asis) x[i] <- trim(unique(sub("asis\\((\\w*).*", "\\1", x[i])))
      } else if (pattern[j] == "log-log") {
        x[i] <- trim(unique(sub("^log\\(log\\(([^,)]*)).*", "\\1", x[i])))
      } else {
        p <- paste0("^", pattern[j], "\\(([^,)]*).*")
        x[i] <- unique(sub(p, "\\1", x[i]))
      }
    }
    # for coxme-models, remove random-effect things...
    trim(sub("^(.*)\\|(.*)", "\\2", x[i]))
  })

  # remove for random intercept only models
  .remove_values(cleaned, c("1", "0"))
}
