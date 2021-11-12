#' @title Find possible transformation of response variables
#' @name find_transformation
#'
#' @description This functions checks whether any transformation, such as log-
#'   or exp-transforming, was applied to the response variable (dependent
#'   variable) in a regression formula. Currently, following patterns are
#'   detected: `log`, `log1p`, `exp`, `expm1`, `sqrt`, `log(x+<number>)` and
#'   `log-log`.
#'
#' @param x A regression model.
#' @return A string, with the name of the function of the applied transformation.
#'   Returns `"identity"` for no transformation, and e.g. `"log(x+3)"` when
#'   a specific values was added to the response variables before
#'   log-transforming.
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
#' @export
find_transformation <- function(x) {
  rv <- find_terms(x)[["response"]]
  transform_fun <- "identity"


  # log-transformation

  if (any(grepl("log\\((.*)\\)", rv))) {
    # do we have log-log models?
    if (grepl("log\\(log\\((.*)\\)\\)", rv)) {
      transform_fun <- "log-log"
    } else {
      plus_minus <- eval(parse(text = gsub("log\\(([^,\\+)]*)(.*)\\)", "\\2", rv)))
      if (is.null(plus_minus)) {
        transform_fun <- "log"
      } else {
        transform_fun <- paste0("log(x+", plus_minus, ")")
      }
    }
  }


  # log1p-transformation

  if (any(grepl("log1p\\((.*)\\)", rv))) {
    transform_fun <- "log1p"
  }


  # expm1-transformation

  if (any(grepl("expm1\\((.*)\\)", rv))) {
    transform_fun <- "expm1"
  }


  # exp-transformation

  if (any(grepl("exp\\((.*)\\)", rv))) {
    transform_fun <- "exp"
  }


  # sqrt-transformation

  if (any(grepl("sqrt\\((.*)\\)", rv))) {
    plus_minus <- eval(parse(text = gsub("sqrt\\(([^,\\+)]*)(.*)\\)", "\\2", rv)))
    if (is.null(plus_minus)) {
      transform_fun <- "sqrt"
    } else {
      transform_fun <- paste0("sqrt(x+", plus_minus, ")")
    }
  }

  transform_fun
}
