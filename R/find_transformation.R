#' @title Find possible transformation of model variables
#' @name find_transformation
#'
#' @description This functions checks whether any transformation, such as log-
#'   or exp-transforming, was applied to the response variable (dependent
#'   variable) in a regression formula. Optionally, all model terms can also be
#'   checked for any such transformation. Currently, following patterns are
#'   detected: `log`, `log1p`, `log2`, `log10`, `exp`, `expm1`, `sqrt`,
#'   `log(y+<number>)`, `log-log`, `log(y,base=<number>)`, `power` (e.g. to 2nd
#'   power, like `I(y^2)`), `inverse` (like `1/y`), `scale` (e.g., `y/3`), and
#'   `box-cox` (e.g., `(y^lambda - 1) / lambda`).
#'
#' @param x A regression model or a character string of the formulation of the
#' (response) variable.
#' @param include_all Logical, if `TRUE`, does not only check the response
#' variable, but all model terms.
#' @param ... Currently not used.
#'
#' @return A string, with the name of the function of the applied transformation.
#'   Returns `"identity"` for no transformation, and e.g. `"log(y+3)"` when
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
#' # find transformation for all model terms
#' model <- lm(mpg ~ log(wt) + I(gear^2) + exp(am), data = mtcars)
#' find_transformation(model, include_all = TRUE)
#'
#' # inverse, response provided as character string
#' find_transformation("1 / y")
#' @export
find_transformation <- function(x, ...) {
  UseMethod("find_transformation")
}


#' @rdname find_transformation
#' @export
find_transformation.default <- function(x, include_all = FALSE, ...) {
  # validation check
  if (is.null(x) || is.data.frame(x) || !is_model(x)) {
    return(NULL)
  }

  # sanity check for multivariate models
  if (is_multivariate(x)) {
    result <- lapply(find_terms(x, verbose = FALSE), function(i) {
      find_transformation(i[["response"]])
    })
    unlist(result)
  } else if (include_all) {
    lapply(find_terms(x, verbose = FALSE), function(i) {
      stats::setNames(
        unlist(lapply(i, find_transformation), use.names = FALSE),
        clean_names(i)
      )
    })
  } else {
    # "raw" response
    rv <- find_terms(x, verbose = FALSE)[["response"]]
    # for divisions, like x/3, `find_response()` returns a character vector
    # of length 2, one with the nominator and the denominator. In this case,
    # check against original response
    original_response <- safe_deparse(find_formula(x, verbose = FALSE)$conditional[[2]])
    # check if we have the pattern (x/<number)
    if (.is_division(original_response)) {
      # if so, check if the pattern really match
      nominator <- gsub("/.*", "\\1", original_response)
      denominator <- gsub(".*\\/(.*)", "\\1", original_response)
      # and if so again, then reconstruct division string
      if (all(rv == c(nominator, denominator))) {
        rv <- paste(nominator, denominator, sep = "/") # nolint
      }
    }
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
      plus_minus <- NULL
      base_value <- NULL
      # make sure we definitly have a "+" in the log-transformation
      if (grepl("+", x, fixed = TRUE)) {
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
      } else if (grepl(",", x, fixed = TRUE)) {
        # check if we have log() with base-definition (e.g. `log(x, base = 5)`)
        base_value <- trim_ws(gsub(",", "", gsub("log\\(([^,\\+)]*)(.*)\\)", "\\2", x), fixed = TRUE))
        if (startsWith(base_value, "base")) {
          base_value <- suppressWarnings(as.numeric(trim_ws(
            gsub("base", "", gsub("=", "", base_value, fixed = TRUE), fixed = TRUE)
          )))
        }
      }
      if ((is.null(plus_minus) || is.function(plus_minus)) && (is.null(base_value) || is.na(base_value))) {
        transform_fun <- "log"
      } else if (!is.null(base_value) && !is.na(base_value)) {
        transform_fun <- paste0("log(x,base=", base_value, ")")
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
  } else if (.is_division(x)) {
    # scale or Box-Cox transformation
    if (.is_box_cox(x)) {
      transform_fun <- "box-cox"
    } else {
      transform_fun <- "scale"
    }
  } else if (any(grepl("(.*)(\\^|\\*\\*)\\s?-?(\\d+|[()])", x))) {
    # power-transformation
    transform_fun <- "power"
  } else if (any(grepl("I\\((.*)\\)", x))) {
    # (unknown) I-transformation
    transform_fun <- NULL
  }

  transform_fun
}


# helper -----------------------------

.is_division <- function(x) {
  any(grepl("(.*)/([0-9\\.\\+\\-]+)(\\)*)$", x)) && !any(grepl("(.*)(\\^|\\*\\*)\\((.*)/(.*)\\)", x))
}

.is_box_cox <- function(x) {
  any(grepl("\\((.*)\\^[0-9\\.\\+\\-]+-1\\)", x))
}
