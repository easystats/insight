#' @title Find name of a model's response variable
#' @name find_response
#'
#' @description Returns the name(s) of the response variable(s) from a model object.
#'
#' @param x A fitted model.
#' @param combine Logical, if \code{TRUE} and the response is a matrix-column,
#'    the name of the response matches the notation in formula, and would for
#'    instance also contain patterns like \code{"cbind(...)"}. Else, the original
#'    variable names from the matrix-column are returned. See 'Examples'.
#'
#' @return The name(s) of the response variable(s) from \code{x} as character vector.
#'
#' @examples
#' library(lme4)
#' data(cbpp)
#' cbpp$trials <- cbpp$size - cbpp$incidence
#' m <- glm(cbind(incidence, trials) ~ period, data = cbpp, family = binomial)
#' 
#' find_response(m, combine = TRUE)
#' find_response(m, combine = FALSE)
#' @export
find_response <- function(x, combine = TRUE) {
  f <- find_formula(x)

  # this is for multivariate response models, where
  # we have a list of formulas
  if (is_multivariate(f)) {
    resp <- unlist(lapply(f, function(i) deparse(i$conditional[[2L]], width.cutoff = 500L)))
  } else {
    resp <- deparse(f$conditional[[2L]], width.cutoff = 500L)
  }

  check_cbind(resp, combine)
}


# should not be called for brms-models!
check_cbind <- function(resp, combine) {
  if (!combine && grepl("cbind\\((.*)\\)", resp)) {
    resp <- sub("cbind\\(([^,].*)([\\)].*)", "\\1", resp)
    resp <- strsplit(resp, split = ",", fixed = TRUE)
    resp <- trim(unlist(resp))
    if (any(string_contains("-", resp[2]))) {
      resp[2] <- trim(sub("(.*)(\\-)(.*)", "\\1", resp[2]))
    }
  } else if (!combine && grepl("Surv\\((.*)\\)", resp)) {
    resp <- sub("Surv\\(([^,].*)([\\)].*)", "\\1", resp)
    resp <- strsplit(resp, split = ",", fixed = TRUE)
    resp <- trim(unlist(resp))
    if (any(string_contains("-", resp[2]))) {
      resp[2] <- trim(sub("(.*)(\\-)(.*)", "\\1", resp[2]))
    }
  } else if (any(string_contains("|", resp))) {
    # check for brms Additional Response Information
    r1 <- trim(sub("(.*)\\|(.*)", "\\1", resp))
    r2 <- trim(sub("(.*)\\|(.*)\\(([^,)]*).*", "\\3", resp))
    resp <- c(r1, r2)
  }

  clean_names(resp)
}
