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
#' @note Typically, this method is intended to work on character vectors,
#'   in order to remove patterns that obscure the variable names. For
#'   convenience reasons it is also possible to call \code{clean_names()}
#'   also on a model object. If \code{x} is a regression model, this
#'   function is (almost) equal to calling \code{find_variables()}. The
#'   main difference is that \code{clean_names()} always returns a character
#'   vector, while \code{find_variables()} returns a list of character
#'   vectors, unless \code{flatten = TRUE}. See 'Examples'.
#'
#' @examples
#' # example from ?stats::glm
#' counts <- c(18, 17, 15, 20, 10, 20, 25, 13, 12)
#' outcome <- c(gl(3, 1, 9))
#' treatment <- gl(3, 3)
#' m <- glm(counts ~ log(outcome) + as.factor(treatment), family = poisson())
#' clean_names(m)
#'
#' # difference "clean_names()" and "find_variables()"
#' library(lme4)
#' m <- glmer(
#'   cbind(incidence, size - incidence) ~ period + (1 | herd),
#'   data = cbpp,
#'   family = binomial
#' )
#'
#' clean_names(m)
#' find_variables(m)
#' find_variables(m, flatten = TRUE)
#' @export
clean_names <- function(x) {
  UseMethod("clean_names")
}


#' @export
clean_names.default <- function(x) {
  cleaned <- unname(find_variables(x, flatten = TRUE))
  .remove_values(cleaned, c("1", "0"))
}


#' @export
clean_names.character <- function(x) {
  .remove_pattern_from_names(x)
}


.remove_pattern_from_names <- function(x, ignore_asis = FALSE, ignore_lag = FALSE) {
  # return if x is empty
  if (.is_empty_string(x)) {
    return("")
  }

  # for gam-smoothers/loess, remove s()- and lo()-function in column name
  # for survival, remove strata(), and so on...
  pattern <- c(
    "as.factor", "as.numeric", "factor", "imean", "offset", "log1p", "log10",
    "log2", "log-log", "log", "lag", "diff", "pspline", "poly", "catg", "asis",
    "matrx", "pol", "strata", "strat", "scale", "scored", "interaction",
    "sqrt", "lsp", "rcs", "pb", "lo", "bs", "ns", "t2", "te", "ti", "tt", # need to be fixed first "mmc", "mm",
    "mi", "mo", "gp", "s", "I"
  )

  # sometimes needed for panelr models, where we need to preserve "lag()"
  if (ignore_lag) {
    lag_pattern <- which(pattern == "lag")
    if (length(lag_pattern)) pattern <- pattern[-lag_pattern]
  }

  # do we have a "log()" pattern here? if yes, get capture region
  # which matches the "cleaned" variable name
  cleaned <- sapply(1:length(x), function(i) {
    for (j in 1:length(pattern)) {
      # remove possible  namespace
      x[i] <- sub("(.*)::(.*)", "\\2", x[i])
      if (pattern[j] == "offset") {
        x[i] <- .trim(unique(sub("^offset\\(([^-+ )]*).*", "\\1", x[i])))
      } else if (pattern[j] == "I") {
        if (!ignore_asis) x[i] <- .trim(unique(sub("I\\(((\\w|\\.)*).*", "\\1", x[i])))
      } else if (pattern[j] == "asis") {
        if (!ignore_asis) x[i] <- .trim(unique(sub("asis\\(((\\w|\\.)*).*", "\\1", x[i])))
      } else if (pattern[j] == "log-log") {
        x[i] <- .trim(unique(sub("^log\\(log\\(((\\w|\\.)*).*", "\\1", x[i])))
      } else if (pattern[j] %in% c("mmc", "mm")) {
        ## TODO multimembership-models need to be fixed
        p <- paste0("^", pattern[j], "\\((.*)\\).*")
        g <- .trim(sub(p, "\\1", x[i]))
        x[i] <- .trim(unlist(strsplit(g, ",")))
      } else {
        # p <- paste0("^", pattern[j], "\\(([^,/)]*).*")
        # this one should be more generic...
        p <- paste0("^", pattern[j], "\\(((\\w|\\.)*).*")
        x[i] <- unique(sub(p, "\\1", x[i]))
      }
    }
    # for coxme-models, remove random-effect things...
    .trim(sub("^(.*)\\|(.*)", "\\2", x[i]))
  })

  # remove for random intercept only models
  .remove_values(cleaned, c("1", "0"))
}



## TODO multimembership-models may also have weights, this does not work yet

.clean_brms_mm <- function(x) {
  # only clean for mm() / mmc() functions, else return x
  if (!grepl("^(mmc|mm)\\(", x)) {
    return(x)
  }

  # extract terms from mm() / mmc() functions, i.e. get
  # multimembership-terms
  unname(.compact_character(unlist(sapply(c("mmc", "mm"), function(j) {
    if (grepl(paste0("^", j, "\\("), x = x)) {
      p <- paste0("^", j, "\\((.*)\\).*")
      g <- .trim(sub(p, "\\1", x))
      .trim(unlist(strsplit(g, ",")))
    } else {
      ""
    }
  }, simplify = FALSE))))
}
