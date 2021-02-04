#' @title Find names of model weights
#' @name find_weights
#'
#' @description Returns the name of the variable that describes the weights of a model.
#'
#' @param x A fitted model.
#' @param ... Currently not used.
#'
#' @return The name of the weighting variable as character vector, or `NULL`
#'   if no weights were specified.
#'
#' @examples
#' data(mtcars)
#' mtcars$weight <- rnorm(nrow(mtcars), 1, .3)
#' m <- lm(mpg ~ wt + cyl + vs, data = mtcars, weights = weight)
#' find_weights(m)
#' @export
find_weights <- function(x, ...) {
  UseMethod("find_weights")
}


#' @export
find_weights.default <- function(x, ...) {
  tryCatch(
    {
      call_string <- .safe_deparse(x$call)
      if (!is.null(call_string)) {
        w <- .safe_deparse(parse(text = call_string)[[1]]$weights)

        # edge case, users use "eval(parse())" to parse weight variables
        if (grepl("^eval\\(parse\\(", w)) {
          w <- eval(parse(text = .trim(gsub("eval\\(parse\\((.*)=(.*)\\)\\)", "\\2", w))))
        }

        if (.is_empty_object(w) || w == "NULL") w <- NULL
      } else {
        w <- NULL
      }
      w
    },
    error = function(e) {
      NULL
    }
  )
}


#' @export
find_weights.brmsfit <- function(x, ...) {
  f <- find_formula(x)

  if (is_multivariate(f)) {
    resp <- unlist(lapply(f, function(i) .safe_deparse(i$conditional[[2L]])))
  } else {
    resp <- .safe_deparse(f$conditional[[2L]])
  }

  resp <- .compact_character(unname(sapply(resp, function(i) {
    if (grepl("(.*)\\|(\\s+)weights\\((.*)\\)", i)) {
      i
    } else {
      ""
    }
  })))

  w <- .trim(sub("(.*)\\|(\\s+)weights\\((.*)\\)", "\\3", resp))
  if (.is_empty_object(w)) w <- NULL
  w
}


#' @export
find_weights.merMod <- function(x, ...) {
  tryCatch(
    {
      w <- .safe_deparse(parse(text = .safe_deparse(x@call))[[1]]$weights)

      # edge case, users use "eval(parse())" to parse weight variables
      if (grepl("^eval\\(parse\\(", w)) {
        w <- eval(parse(text = .trim(gsub("eval\\(parse\\((.*)=(.*)\\)\\)", "\\2", w))))
      }

      if (.is_empty_object(w) || w == "NULL") w <- NULL
      w
    },
    error = function(e) {
      NULL
    }
  )
}
