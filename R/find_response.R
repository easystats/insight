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
#' @param ... Currently not used.
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
    resp <- unlist(lapply(f, function(i) .safe_deparse(i$conditional[[2L]])))
  } else {
    resp <- .safe_deparse(f$conditional[[2L]])
  }

  check_cbind(resp, combine, model = x)
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
    .safe_deparse(f$conditional$selection[[2L]]),
    .safe_deparse(f$conditional$outcome[[2L]])
  )
  check_cbind(resp, combine, model = x)
}




#' @export
find_response.mediate <- function(x, combine = TRUE, ...) {
  f <- find_formula(x, verbose = FALSE)

  if (is.null(f)) {
    return(NULL)
  }

  resp <- c(.safe_deparse(f$mediator$conditional[[2L]]), .safe_deparse(f$outcome$conditional[[2L]]))
  check_cbind(resp, combine, model = x)
}




#' @export
find_response.mjoint <- function(x, combine = TRUE, component = c("conditional", "survival", "all"), ...) {
  component <- match.arg(component)
  f <- find_formula(x, verbose = FALSE)

  if (is.null(f)) {
    return(NULL)
  }

  conditional <- unlist(lapply(f[grepl("^conditional", names(f))], function(i) .safe_deparse(i[[2L]])))
  survial <- .safe_deparse(f$survival[[2L]])

  resp <- switch(component,
    "conditional" = conditional,
    "survial" = survial,
    "all" = c(conditional, survial)
  )

  unlist(lapply(resp, check_cbind, combine = combine, model = x))
}




#' @export
find_response.joint <- function(x,
                                combine = TRUE,
                                component = c("conditional", "survival", "all"),
                                ...) {
  component <- match.arg(component)
  f <- find_formula(x, verbose = FALSE)

  if (is.null(f)) {
    return(NULL)
  }

  conditional <- .safe_deparse(f$conditional[[2L]])
  survial <- .safe_deparse(f$survival[[2L]])

  resp <- switch(component,
    "conditional" = conditional,
    "survial" = survial,
    "all" = c(conditional, survial)
  )

  unlist(lapply(resp, check_cbind, combine = combine, model = x))
}




# utils ---------------------


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
  } else if (!combine && any(grepl("MMO\\((.*)\\)", resp))) {
    resp <- .extract_combined_response(resp, "MMO")
  } else if (!combine && any(grepl("MMO2\\((.*)\\)", resp))) {
    resp <- .extract_combined_response(resp, "MMO2")
  } else if (!combine && any(grepl("/", resp, fixed = TRUE))) {
    resp <- strsplit(resp, split = "/", fixed = TRUE)
    resp <- gsub("(I|\\(|\\))", "", .trim(unlist(resp)))
  } else if (any(.string_contains("|", resp))) {
    # check for brms Additional Response Information
    r1 <- .trim(sub("(.*)\\|(.*)", "\\1", resp))
    r2 <- .trim(sub("(.*)\\|(.*)\\(([^,)]*).*", "\\3", resp))
    # check for "resp_thres" pattern
    r_resp <- .trim(unlist(strsplit(resp, "|", fixed = TRUE))[2])
    if (grepl("^resp_thres", r_resp)) {
      r3 <- .trim(sub("=", "", sub("(.*)\\(([^=)]*)(.*)\\)", "\\3", r_resp)))
      names(r3) <- r3
      numeric_values <- suppressWarnings(as.numeric(r2))
      r2 <- r2[is.na(numeric_values)]
      if (length(r2)) {
        r2 <- c(r2, r3)
      } else {
        r2 <- r3
      }
    }
    resp <- c(r1, r2)
  } else if (!combine && any(grepl("+", resp, fixed = TRUE))) {
    resp <- strsplit(resp, split = "+", fixed = TRUE)
    resp <- gsub("(I|\\(|\\))", "", .trim(unlist(resp)))
  }

  # exception
  if (inherits(model, "clogit") && grepl("^rep\\(", resp[1]) && length(resp) == 3) {
    resp <- c(paste0(resp[1], resp[2]), resp[3])
  }

  .remove_pattern_from_names(resp, ignore_asis = TRUE)
}


.extract_combined_response <- function(resp, pattern) {
  if (pattern %in% c("MMO", "MMO2") && !grepl(paste0("^", pattern, "\\((.*),(.*)\\)"), resp)) {
    resp <- gsub(paste0("^", pattern, "\\((.*)\\)"), "\\1", resp)
  } else {
    resp <- sub(sprintf("%s\\(([^,].*)([\\)].*)", pattern), "\\1", resp)
    resp <- strsplit(resp, split = ",", fixed = TRUE)
    resp <- .trim(unlist(resp))
  }

  if (any(.string_contains("-", resp[2]))) {
    resp[2] <- .trim(sub("(.*)(\\-)(.*)", "\\1", resp[2]))
  }

  resp
}
