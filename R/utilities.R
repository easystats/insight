#' @title Small helper functions
#' @name trim_ws
#'
#' @description Collection of small helper functions. `trim_ws()` is an
#' efficient function to trim leading and trailing whitespaces from character
#' vectors or strings. `n_unique()` returns the number of unique values in a
#' vector. `safe_deparse()` is comparable to `deparse1()`, i.e. it can safely
#' deparse very long expressions into a single string.
#'
#' @param x A (character) vector, or for some functions may also be a data frame.
#' @param na.rm Logical, if missing values should be removed from the input.
#' @param character_only Logical, if `TRUE` and `x` is a data frame or list,
#' only processes character vectors.
#' @param ... Currently not used.
#'
#' @return For a vector, `n_unique` always returns an integer value, even if the
#' input is `NULL` (the return value will be `0` then). For data frames or lists,
#' `n_unique()` returns a named numeric vector, with the number of unique values
#' for each element.
#'
#' @examples
#' trim_ws("  no space!  ")
#' n_unique(iris$Species)
#' @export
trim_ws <- function(x, ...) {
  UseMethod("trim_ws")
}

#' @export
trim_ws.default <- function(x, ...) {
  gsub("^\\s+|\\s+$", "", x)
}

#' @rdname trim_ws
#' @export
trim_ws.data.frame <- function(x, character_only = TRUE, ...) {
  if (character_only) {
    chars <- which(sapply(x, is.character))
  } else {
    chars <- seq_len(ncol(x))
  }
  if (length(chars)) {
    x[chars] <- lapply(x[chars], trim_ws)
  }
  x
}

#' @export
trim_ws.list <- function(x, character_only = TRUE, ...) {
  if (character_only) {
    chars <- which(sapply(x, is.character))
  } else {
    chars <- seq_len(length(x))
  }
  if (length(chars)) {
    x[chars] <- lapply(x[chars], trim_ws)
  }
  x
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

#' @export
n_unique.list <- function(x, na.rm = TRUE, ...) {
  lapply(x, n_unique, na.rm = na.rm)
}



# safe_deparse ---------------------------------------

#' @rdname trim_ws
#' @export
safe_deparse <- function(x, ...) {
  if (is.null(x)) {
    return(NULL)
  }
  paste0(sapply(deparse(x, width.cutoff = 500), trim_ws, simplify = TRUE), collapse = " ")
}
