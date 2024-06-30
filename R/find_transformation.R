#' @title Find possible transformation of response variables
#' @name find_transformation
#'
#' @description This functions checks whether any transformation, such as log-
#'   or exp-transforming, was applied to the response variable (dependent
#'   variable) in a regression formula. Currently, following patterns are
#'   detected: `log`, `log1p`, `log2`, `log10`, `exp`, `expm1`, `sqrt`,
#'   `log(x+<number>)`, `log-log`, `power` (to 2nd power, like `I(x^2)`), and
#'   `inverse` (like `1/y`).
#'
#' @param x A regression model or a character string of the response value.
#' @param ... Currently not used.
#'
#' @return A string, with the name of the function of the applied transformation.
#'   Returns `"identity"` for no transformation, and e.g. `"log(x+3)"` when
#'   a specific values was added to the response variables before
#'   log-transforming. For unknown transformations, returns `NULL`.
#'
#' @examples
#' # identity, no transformation
#' model <- lm(Sepal.Length ~ Species, data = iris)
#' find_transformation(model)
#'
#' # log-transformation
#' model <- lm(log(Sepal.Length) ~ Species, data = iris)
#' find_transformation(model)
#'
#' # log+2
#' model <- lm(log(Sepal.Length + 2) ~ Species, data = iris)
#' find_transformation(model)
#'
#' # inverse, response provided as character string
#' find_transformation("1 / y")
#' @export
find_transformation <- function(x, ...) {
  UseMethod("find_transformation")
}


#' @export
find_transformation.default <- function(x, ...) {
  # validation check
  if (is.null(x) || is.data.frame(x) || !is_model(x)) {
    return(NULL)
  }

  # sanity check for multivariate models
  if (is_multivariate(x)) {
    result <- lapply(find_terms(x), function(i) {
      find_transformation(i[["response"]])
    })
    unlist(result)
  } else {
    rv <- find_terms(x)[["response"]]
    find_transformation(rv)
  }
}


#' @export
find_transformation.character <- function(x, ...) {
  transform_fun <- "identity"

  # remove whitespaces
  x <- gsub(" ", "", x, fixed = TRUE)

  # log-transformation
  if (any(grepl("log\\((.*)\\)", x))) {
    # do we have log-log models?
    if (grepl("log\\(log\\((.*)\\)\\)", x)) {
      transform_fun <- "log-log"
    } else {
      # 1. try: log(x + number)
      plus_minus <- .safe(
        eval(parse(text = gsub("log\\(([^,\\+)]*)(.*)\\)", "\\2", x)))
      )
      # 2. try: log(number + x)
      if (is.null(plus_minus)) {
        plus_minus <- .safe(
          eval(parse(text = gsub("log\\(([^,\\+)]*)(.*)\\)", "\\1", x)))
        )
      }
      if (is.null(plus_minus) || is.function(plus_minus)) {
        transform_fun <- "log"
      } else {
        transform_fun <- paste0("log(x+", plus_minus, ")")
      }
    }
  } else if (any(grepl("log1p\\((.*)\\)", x))) {
    # log1p-transformation
    transform_fun <- "log1p"
  } else if (any(grepl("expm1\\((.*)\\)", x))) {
    # expm1-transformation
    transform_fun <- "expm1"
  } else if (any(grepl("log2\\((.*)\\)", x))) {
    # log2/log10-transformation
    transform_fun <- "log2"
  } else if (any(grepl("log10\\((.*)\\)", x))) {
    transform_fun <- "log10"
  } else if (any(grepl("exp\\((.*)\\)", x))) {
    # exp-transformation
    transform_fun <- "exp"
  } else if (any(grepl("sqrt\\((.*)\\)", x))) {
    # sqrt-transformation
    plus_minus <- eval(parse(text = gsub("sqrt\\(([^,\\+)]*)(.*)\\)", "\\2", x)))
    if (is.null(plus_minus) || is.function(plus_minus)) {
      transform_fun <- "sqrt"
    } else {
      transform_fun <- paste0("sqrt(x+", plus_minus, ")")
    }
  } else if (any(startsWith(x, "1/"))) {
    # inverse-transformation
    transform_fun <- "inverse"
  } else if (any(grepl("(.*)(\\^|\\*\\*)\\s?-?(\\d+|[()])", x))) {
    # power-transformation
    transform_fun <- "power"
  } else if (any(grepl("I\\((.*)\\)", x))) {
    # (unknown) I-transformation
    transform_fun <- NULL
  }

  transform_fun
}
