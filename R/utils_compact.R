#' Remove empty elements from lists
#'
#' @param x A list or vector.
#' @param remove_na Logical to decide if `NA`s should be removed.
#'
#' @note By default, `compact_list()` does not remove "empty" elements from
#' deeper list levels. Only if an element on the first level of a list is
#' "empty", it is removed.
#'
#' @examples
#' compact_list(list(NULL, 1, c(NA, NA)))
#' compact_list(c(1, NA, NA))
#' compact_list(c(1, NA, NA), remove_na = TRUE)
#'
#' # remove only NULL on top level, don't change deeper lists
#' compact_list(list(
#'   a = 1,
#'   NULL,
#'   b = list(NULL, list(1, 2, 3), list(list(x = 3, y = 4, NULL))),
#'   NULL
#' ))
#' @export
compact_list <- function(x, remove_na = FALSE) {
  is_remove <- vapply(
    x,
    function(i) {
      if (is_model(i)) {
        return(FALSE)
      }
      if (inherits(i, c("Formula", "gFormula"))) {
        return(FALSE)
      }
      if (is.function(i)) {
        return(FALSE)
      }
      if (remove_na) {
        # is.na() returns logical() for empty vectors or NULL, and thus
        # all(is.na()) returns TRUE - which is intended, and we don't need
        # to check for is.null or length() == 0
        all(is.na(i)) || .is_null_string(i)
      } else {
        (length(i) == 0L || is.null(i) || .is_null_string(i))
      }
    },
    logical(1),
    USE.NAMES = FALSE
  )
  x[!is_remove]
}

#' Remove empty strings from character
#'
#' @param x A single character or a vector of characters.
#'
#' @return
#'
#' A character or a character vector with empty strings removed.
#'
#' @examples
#' compact_character(c("x", "y", NA))
#' compact_character(c("x", "NULL", "", "y"))
#'
#' @export
compact_character <- function(x) {
  is_remove <- vapply(
    x,
    function(i) {
      !nzchar(i, keepNA = TRUE) ||
        all(is.na(i)) ||
        any(as.character(i) == "NULL", na.rm = TRUE)
    },
    FUN.VALUE = logical(1),
    USE.NAMES = FALSE
  )
  x[!is_remove]
}


# helper -----------------

.is_null_string <- function(object) {
  if (is.character(object) || is.factor(object)) {
    return(any(object == "NULL", na.rm = TRUE))
  }
  if (is.atomic(object) || is.data.frame(object)) {
    return(FALSE)
  }
  if (is.list(object)) {
    # we check for deeper list objects here, because `as.character()` can be
    # very slow for large lists; we only need to check for first level when
    # compacting lists.
    return(.is_null_list(object))
  }
  is.null(object)
}


# Helper function to check if a list recursively contains only NULL or "NULL" values
.is_null_list <- function(x) {
  # 1. Base guard: If 'x' is not a list, or it is a data frame (which is
  # technically a list under the hood), it cannot be a "null list".
  if (!is.list(x) || is.data.frame(x)) {
    return(FALSE)
  }

  # 2. Base guard: If the list is completely empty (length 0),
  # it is considered a null list.
  if (!length(x)) {
    return(TRUE)
  }

  # 3. Iterate through each element in the list to check its contents
  for (i in x) {
    # If the element is explicitly NULL, it passes the check; move to the next element
    if (is.null(i)) {
      next
    }

    # If the element is a character or a factor, check for the literal string "NULL"
    if (is.character(i) || is.factor(i)) {
      # Check if "NULL" exists anywhere in the character/factor vector
      if (any(i == "NULL", na.rm = TRUE)) {
        next
      }
      # If the string does not contain "NULL", the list contains valid data
      return(FALSE)
    }

    # If the element is a nested list, call this function recursively
    if (is.list(i)) {
      # If the nested list is also entirely null, move to the next element
      if (.is_null_list(i)) {
        next
      }
      # If the nested list contains valid data, return FALSE
      return(FALSE)
    }

    # If the element is of any other type (e.g., numeric, logical, matrix),
    # it is not null, so the list as a whole is not a null list.
    return(FALSE)
  }

  # 4. If the loop completes without ever returning FALSE, all elements
  # were confirmed to be some variation of null.
  TRUE
}
