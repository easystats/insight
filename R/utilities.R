#' @title Small helper functions
#' @name trim_ws
#'
#' @description Collection of small helper functions. `trim_ws()` is an
#' efficient function to trim leading and trailing whitespaces from character
#' vectors or strings. `n_unique()` returns the number of unique values in a
#' vector. `safe_deparse()` is comparable to `deparse1()`, i.e. it can safely
#' deparse deparse very long expressions into a single string.
#'
#' @param x A (character) vector, or for some functions may also be a data frame.
#' @param na.rm Logical, if missing values should be removed from the input.
#' @param ... Currently not used.
#'
#' @return (tbd)
#'
#' @examples
#' trim_ws("  no space!  ")
#' n_unique(iris$Species)
#' @export
trim_ws <- function(x, ...) {
  gsub("^\\s+|\\s+$", "", x)
}



# n_unique ---------------------------------------

#' @rdname trim_ws
#' @export
n_unique <- function(x, ...) {
  UseMethod("n_unique")
}

#' @rdname trim_ws
#' @export
n_unique.default <- function(x, na.rm = TRUE, ...) {
  if (is.null(x)) {
    return(0)
  }
  if (isTRUE(na.rm)) x <- stats::na.omit(x)
  length(unique(x))
}

#' @export
n_unique.data.frame <- function(x, na.rm = TRUE, ...) {
  sapply(x, n_unique, na.rm = na.rm)
}



# safe_depars ---------------------------------------

#' @rdname trim_ws
safe_deparse <- function(x, ...) {
  if (is.null(x)) {
    return(NULL)
  }
  paste0(sapply(deparse(x, width.cutoff = 500), trim_ws, simplify = TRUE), collapse = " ")
}
