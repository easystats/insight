#' @title Remove backticks from a string
#' @name text_remove_backticks
#'
#' @description This function removes backticks from a string.
#'
#' @param x A character vector, a data frame or a matrix. If a matrix,
#'   backticks are removed from the column and row names, not from values
#'   of a character vector.
#' @param column If `x` is a data frame, specify the column of character
#'   vectors, where backticks should be removed. If `NULL`, all character
#'   vectors are processed.
#' @param verbose Toggle warnings.
#' @param ... Currently not used.
#'
#' @return `x`, where all backticks are removed.
#'
#' @note If `x` is a character vector or data frame, backticks are removed from
#'   the elements of that character vector (or character vectors from the data
#'   frame.) If `x` is a matrix, the behaviour slightly differs: in this case,
#'   backticks are removed from the column and row names. The reason for this
#'   behaviour is that this function mainly serves formatting coefficient names.
#'   For `vcov()` (a matrix), row and column names equal the coefficient names
#'   and therefore are manipulated then.
#'
#' @examples
#' # example model
#' data(iris)
#' iris$`a m` <- iris$Species
#' iris$`Sepal Width` <- iris$Sepal.Width
#' model <- lm(`Sepal Width` ~ Petal.Length + `a m`, data = iris)
#'
#' # remove backticks from string
#' names(coef(m))
#' text_remove_backticks(names(coef(m)))
#'
#' # remove backticks from character variable in a data frame
#' # column defaults to "Parameter".
#' d <- data.frame(Parameter = names(coef(m)), Estimate = unname(coef(m)))
#' d
#' text_remove_backticks(d)
#' @export
text_remove_backticks <- function(x, ...) {
  UseMethod("text_remove_backticks")
}



#' @export
text_remove_backticks.default <- function(x, verbose = TRUE, ...) {
  if (isTRUE(verbose)) {
    warning(format_message(paste0("Removing backticks currently not supported for objects of class '", class(x)[1], "'.")), call. = FALSE)
  }
  x
}


#' @rdname text_remove_backticks
#' @export
text_remove_backticks.data.frame <- function(x, column = "Parameter", verbose = TRUE, ...) {
  if (is.null(column)) {
    column <- colnames(x)[sapply(x, is.character)]
  }
  not_found <- vector("character")
  for (i in column) {
    if (i %in% colnames(x) && is.character(x[[i]])) {
      x[[i]] <- gsub("`", "", x[[i]], fixed = TRUE)
    } else {
      not_found <- c(not_found, i)
    }
  }
  if (verbose && length(not_found)) {
    warning(format_message("Following columns were not found or were no character vectors:",
                           paste0(not_found, collapse = ", ")), call. = FALSE)
  }
  x
}

#' @export
text_remove_backticks.character <- function(x, ...) {
  gsub("`", "", x, fixed = TRUE)
}

#' @export
text_remove_backticks.factor <- function(x, ...) {
  text_remove_backticks(as.character(x))
}

#' @export
text_remove_backticks.matrix <- function(x, ...) {
  colnames(x) <- gsub("`", "", colnames(x), fixed = TRUE)
  rownames(x) <- gsub("`", "", colnames(x), fixed = TRUE)
  x
}
