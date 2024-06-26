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
  rv <- find_terms(x)[["response"]]
  find_transformation(rv)
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
      if (is.null(plus_minus)) {
        transform_fun <- "log"
      } else {
        transform_fun <- paste0("log(x+", plus_minus, ")")
      }
    }
  }


  # log1p-transformation

  if (any(grepl("log1p\\((.*)\\)", x))) {
    transform_fun <- "log1p"
  }


  # expm1-transformation

  if (any(grepl("expm1\\((.*)\\)", x))) {
    transform_fun <- "expm1"
  }


  # log2/log10-transformation

  if (any(grepl("log2\\((.*)\\)", x))) {
    transform_fun <- "log2"
  }

  if (any(grepl("log10\\((.*)\\)", x))) {
    transform_fun <- "log10"
  }


  # exp-transformation

  if (any(grepl("exp\\((.*)\\)", x))) {
    transform_fun <- "exp"
  }


  # sqrt-transformation

  if (any(grepl("sqrt\\((.*)\\)", x))) {
    plus_minus <- eval(parse(text = gsub("sqrt\\(([^,\\+)]*)(.*)\\)", "\\2", x)))
    if (is.null(plus_minus)) {
      transform_fun <- "sqrt"
    } else {
      transform_fun <- paste0("sqrt(x+", plus_minus, ")")
    }
  }


  # inverse-transformation

  if (any(startsWith(x, "1/"))) {
    transform_fun <- "inverse"
  }


  # (unknown) I-transformation

  if (any(grepl("I\\((.*)\\)", x))) {
    transform_fun <- NULL
  }


  # power-transformation

  if (any(grepl("(.*)(\\^|\\*\\*)\\s?-?(\\d+|[()])", x))) {
    transform_fun <- "power"
  }

  transform_fun
}
