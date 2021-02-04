#' @title Find name of the response variable
#' @name find_response
#'
#' @description Returns the name(s) of the response variable(s) from a model object.
#'
#' @param x A fitted model.
#' @param combine Logical, if `TRUE` and the response is a matrix-column,
#'    the name of the response matches the notation in formula, and would for
#'    instance also contain patterns like `"cbind(...)"`. Else, the original
#'    variable names from the matrix-column are returned. See 'Examples'.
#'
#' @return The name(s) of the response variable(s) from `x` as character
#'   vector, or `NULL` if response variable could not be found.
#'
#' @examples
#' if (require("lme4")) {
#'   data(cbpp)
#'   cbpp$trials <- cbpp$size - cbpp$incidence
#'   m <- glm(cbind(incidence, trials) ~ period, data = cbpp, family = binomial)
#'
#'   find_response(m, combine = TRUE)
#'   find_response(m, combine = FALSE)
#' }
#' @export
find_response <- function(x, combine = TRUE) {
  f <- find_formula(x)

  if (is.null(f)) {
    return(NULL)
  }

  # this is for multivariate response models, where
  # we have a list of formulas
  if (inherits(x, "mediate")) {
    resp <- c(.safe_deparse(f$mediator$conditional[[2L]]), .safe_deparse(f$outcome$conditional[[2L]]))
  } else if (is_multivariate(f)) {
    resp <- unlist(lapply(f, function(i) .safe_deparse(i$conditional[[2L]])))
  } else {
    resp <- .safe_deparse(f$conditional[[2L]])
  }

  check_cbind(resp, combine, model = x)
}


# should not be called for brms-models!
check_cbind <- function(resp, combine, model) {
  if (!combine && inherits(model, "DirichletRegModel")) {
    resp <- model$varnames
  } else if (!combine && any(grepl("cbind\\((.*)\\)", resp))) {
    resp <- .extract_combined_response(resp, "cbind")
  } else if (!combine && any(grepl("Surv\\((.*)\\)", resp))) {
    resp <- .extract_combined_response(resp, "Surv")
  } else if (!combine && any(grepl("Hist\\((.*)\\)", resp))) {
    resp <- .extract_combined_response(resp, "Hist")
  } else if (!combine && any(grepl("Event\\((.*)\\)", resp))) {
    resp <- .extract_combined_response(resp, "Event")
  } else if (!combine && any(grepl("Curv\\((.*)\\)", resp))) {
    resp <- .extract_combined_response(resp, "Curv")
  } else if (!combine && any(grepl("/", resp, fixed = TRUE))) {
    resp <- strsplit(resp, split = "/", fixed = TRUE)
    resp <- gsub("(I|\\(|\\))", "", .trim(unlist(resp)))
  } else if (any(.string_contains("|", resp))) {
    # check for brms Additional Response Information
    r1 <- .trim(sub("(.*)\\|(.*)", "\\1", resp))
    r2 <- .trim(sub("(.*)\\|(.*)\\(([^,)]*).*", "\\3", resp))
    resp <- c(r1, r2)
  }

  # exception
  if (inherits(model, "clogit") && grepl("^rep\\(", resp[1]) && length(resp) == 3) {
    resp <- c(paste0(resp[1], resp[2]), resp[3])
  }

  .remove_pattern_from_names(resp, ignore_asis = TRUE)
}


.extract_combined_response <- function(resp, pattern) {
  resp <- sub(sprintf("%s\\(([^,].*)([\\)].*)", pattern), "\\1", resp)
  resp <- strsplit(resp, split = ",", fixed = TRUE)
  resp <- .trim(unlist(resp))

  if (any(.string_contains("-", resp[2]))) {
    resp[2] <- .trim(sub("(.*)(\\-)(.*)", "\\1", resp[2]))
  }

  resp
}
