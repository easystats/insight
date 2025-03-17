#' @title Small helper functions
#' @name trim_ws
#'
#' @description Collection of small helper functions. `trim_ws()` is an
#' efficient function to trim leading and trailing whitespaces from character
#' vectors or strings. `n_unique()` returns the number of unique values in a
#' vector. `has_single_value()` is equivalent to `n_unique() == 1` but is faster
#' (note the different default for the `remove_na` argument). `safe_deparse()`
#' is comparable to `deparse1()`, i.e. it can safely deparse very long
#' expressions into a single string. `safe_deparse_symbol()` only deparses a
#' substituted expressions when possible, which can be much faster than
#' `deparse(substitute())` for those cases where `substitute()` returns no valid
#' object name.
#'
#' @param x A (character) vector, or for some functions may also be a data frame.
#' @param remove_na Logical, if missing values should be removed from the input.
#' @param character_only Logical, if `TRUE` and `x` is a data frame or list,
#' only processes character vectors.
#' @param ... Currently not used.
#'
#' @return
#' - `n_unique()`: For a vector, `n_unique` always returns an integer value,
#'   even if the input is `NULL` (the return value will be `0` then). For data
#'   frames or lists, `n_unique()` returns a named numeric vector, with the
#'   number of unique values for each element.
#' - `has_single_value()`: `TRUE` if `x` has only one unique value,
#'   `FALSE` otherwise.
#' - `trim_ws()`: A character vector, where trailing and leading white spaces
#'   are removed.
#' - `safe_deparse()`: A character string of the unevaluated expression or symbol.
#' - `safe_deparse_symbol()`: A character string of the unevaluated expression
#'   or symbol, if `x` was a symbol. If `x` is no symbol (i.e. if `is.name(x)`
#'   would return `FALSE`), `NULL` is returned.
#'
#' @examples
#' trim_ws("  no space!  ")
#' n_unique(iris$Species)
#' has_single_value(c(1, 1, 2))
#'
#' # safe_deparse_symbol() compared to deparse(substitute())
#' safe_deparse_symbol(as.name("test"))
#' deparse(substitute(as.name("test")))
#' @export
trim_ws <- function(x, ...) {
  UseMethod("trim_ws")
}

#' @export
trim_ws.default <- function(x, ...) {
  gsub("^\\s+|\\s+$", "", x, useBytes = TRUE)
}

#' @rdname trim_ws
#' @export
trim_ws.data.frame <- function(x, character_only = TRUE, ...) {
  if (character_only) {
    chars <- which(vapply(x, is.character, FUN.VALUE = logical(1L)))
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
    chars <- which(vapply(x, is.character, FUN.VALUE = logical(1L)))
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
n_unique.default <- function(x, remove_na = TRUE, ...) {
  if (is.null(x)) {
    return(0)
  }
  if (isTRUE(remove_na)) x <- x[!is.na(x)]
  length(unique(x))
}

#' @export
n_unique.data.frame <- function(x, remove_na = TRUE, ...) {
  vapply(x, n_unique, remove_na = remove_na, FUN.VALUE = numeric(1L))
}

#' @export
n_unique.list <- function(x, remove_na = TRUE, ...) {
  lapply(x, n_unique, remove_na = remove_na)
}


# safe_deparse ---------------------------------------

#' @rdname trim_ws
#' @export
safe_deparse <- function(x, ...) {
  if (is.null(x)) {
    return(NULL)
  }
  paste(sapply(deparse(x, width.cutoff = 500), trim_ws, simplify = TRUE), collapse = " ")
}


# safe_substitute ---------------------------------------

#' @rdname trim_ws
#' @export
safe_deparse_symbol <- function(x) {
  if (is.name(x)) {
    out <- safe_deparse(x)
  } else {
    out <- NULL
  }
  out
}


# has_single_value ---------------------------------------

#' @rdname trim_ws
#' @export
has_single_value <- function(x, remove_na = FALSE, ...) {
  if (remove_na) x <- x[!is.na(x)]
  !is.null(x) && length(x) > 0L && isTRUE(all(x == x[1]))
}
