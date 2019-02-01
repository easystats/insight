#' @title Get name of a model's response variable
#' @name find_response
#'
#' @description Returns the name(s) of the response variable(s) from a model object.
#'
#' @param x A fitted model.
#' @param combine Logical, if \code{TRUE} and the response is a matrix-column,
#'    the name of the response matches the notation in formula, and would for
#'    instance also contain patterns like \code{"cbind(...)"}. Else, the original
#'    variable names from the matrix-column are returned. See 'Examples'.
#' @param ... Currently not used.
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
#'
#' @export
find_response <- function(x, ...) {
  UseMethod("find_response")
}

#' @rdname find_response
#' @importFrom stats formula
#' @export
find_response.default <- function(x, combine = TRUE, ...) {
  resp <- deparse(stats::formula(x)[[2L]], width.cutoff = 500L)
  check_cbind(resp, combine)
}


#' @export
find_response.MCMCglmm <- function(x, combine = TRUE, ...) {
  resp <- all.vars(x$Fixed$formula[[2L]])
  check_cbind(resp, combine)
}


#' @export
find_response.felm <- function(x, combine = TRUE, ...) {
  resp <- x$lhs
  check_cbind(resp, combine)
}


#' @export
find_response.clm2 <- function(x, combine = TRUE, ...) {
  resp <- all.vars(attr(x$location, "terms", exact = TRUE)[[2L]])
  check_cbind(resp, combine)
}


#' @importFrom stats formula
#' @export
find_response.gam <- function(x, combine = TRUE, ...) {
  f <- stats::formula(x)

  if (is.list(f))
    resp <- deparse(stats::formula(x)[[1]][[2L]], width.cutoff = 500L)
  else
    resp <- deparse(stats::formula(x)[[2L]], width.cutoff = 500L)

  check_cbind(resp, combine)
}


#' @importFrom stats formula
#' @export
find_response.brmsfit <- function(x, combine = TRUE, ...) {
  f <- stats::formula(x)

  if (is.null(f$responses)) {
    resp <- deparse(f$formula[[2L]], width.cutoff = 500L)
    # check for brms Additional Response Information
    if (any(string_contains("|", resp))) {
      r1 <- trim(sub("(.*)\\|(.*)", "\\1", resp))
      r2 <- trim(sub("(.*)\\|(.*)\\(([^,)]*).*", "\\3", resp))
      resp <- c(r1, r2)
    }
  } else {
    resp <- unlist(
      lapply(x$formula$forms, function(.x) all.vars(stats::formula(.x)[[2L]]))
    )
  }

  clean_names(resp)
}


#' @importFrom stats formula
#' @export
find_response.stanmvreg <- function(x, combine = TRUE, ...) {
  clean_names(unlist(lapply(stats::formula(x), function(.x) deparse(.x[[2L]], width.cutoff = 500L))))
}


#' @importFrom stats formula
#' @export
find_response.aovlist <- function(x, combine = TRUE, ...) {
  resp <- all.vars(attr(x, "terms")[[2L]])
  check_cbind(resp, combine)
}


# should not be called for brms-models!
check_cbind <- function(resp, combine) {
  if (!combine && grepl("cbind\\((.*)\\)", resp)) {
    resp <- sub("cbind\\(([^,].*)([\\)].*)" ,"\\1", resp)
    resp <- strsplit(resp, split  = ",", fixed = TRUE)
    resp <- trim(unlist(resp))
    if (any(string_contains("-", resp[2])))
      resp[2] <- trim(sub("(.*)(\\-)(.*)", "\\1", resp[2]))
  }

  clean_names(resp)
}
