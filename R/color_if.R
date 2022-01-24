#' @title Color-formatting for data columns based on condition
#' @name color_if
#'
#' @description
#' Convenient function that formats columns in data frames with color codes,
#' where the color is chosen based on certain conditions. Columns are then
#' printed in color in the console.
#'
#' @param x A data frame
#' @param columns Character vector with column names of `x` that should be
#'   formatted.
#' @param predicate A function that takes `columns` and `value` as input
#'   and which should return `TRUE` or `FALSE`, based on if the condition
#'   (in comparison with `value`) is met.
#' @param value The comparator. May be used in conjunction with `predicate`
#'   to quickly set up a function which compares elements in `colums` to `value`.
#'   May be ignored when `predicate` is a function that internally computes other
#'   comparisons. See 'Examples'.
#' @param color_if,colour_if Character vector, indicating the color code used to
#'   format values in `x` that meet the condition of `predicate` and `value`.
#'   May be one of `"red"`, `"yellow"`, `"green"`, `"blue"`,
#'   `"violet"`, `"cyan"` or `"grey"`. Formatting is also possible
#'   with `"bold"` or `"italic"`.
#' @param color_else,colour_else See `color_if`, but only for conditions
#'   that are *not* met.
#' @param digits Digits for rounded values.
#'
#' @details The predicate-function simply works like this:
#' `which(predicate(x[, columns], value))`
#'
#' @return The .
#'
#' @examples
#' # all values in Sepal.Length larger than 5 in green, all remaining in red
#' x <- color_if(iris[1:10, ], columns = "Sepal.Length", predicate = `>`, value = 5)
#' x
#' cat(x$Sepal.Length)
#'
#' # all levels "setosa" in Species in green, all remaining in red
#' x <- color_if(iris, columns = "Species", predicate = `==`, value = "setosa")
#' cat(x$Species)
#'
#' # own function, argument "value" not needed here
#' p <- function(x, y) {
#'   x >= 4.9 & x <= 5.1
#' }
#' # all values in Sepal.Length between 4.9 and 5.1 in green, all remaining in red
#' x <- color_if(iris[1:10, ], columns = "Sepal.Length", predicate = p)
#' cat(x$Sepal.Length)
#' @export
color_if <- function(x,
                     columns,
                     predicate = `>`,
                     value = 0,
                     color_if = "green",
                     color_else = "red",
                     digits = 2) {
  xnew <- x

  if (columns %in% names(x)) {
    x_if <- which(predicate(x[, columns], value))
    x_else <- which(!predicate(x[, columns], value))

    values <- x[, columns]

    xnew[, columns] <- format(
      if (is.numeric(values)) {
        round(values, digits = digits)
      } else {
        values
      },
      width = nchar(columns),
      nsmall = digits,
      justify = "right"
    )

    # remove NA
    xnew[, columns][trimws(xnew[, columns]) == "NA"] <- ""

    if (!is.null(color_if) && length(x_if)) {
      xnew[, columns][x_if] <- .colour(color_if, xnew[, columns][x_if])
    }
    if (!is.null(color_else) && length(x_else)) {
      xnew[, columns][x_else] <- .colour(color_else, xnew[, columns][x_else])
    }
  }

  xnew
}


#' Detect coloured cells
#' @keywords internal
.colour_detect <- function(x) {
  ansi_regex <- paste0(
    "(?:(?:\\x{001b}\\[)|\\x{009b})",
    "(?:(?:[0-9]{1,3})?(?:(?:;[0-9]{0,3})*)?[A-M|f-m])",
    "|\\x{001b}[A-M]"
  )
  grepl(ansi_regex, x, perl = TRUE)
}


#' @rdname color_if
#' @export
colour_if <- function(x,
                      columns,
                      predicate = `>`,
                      value = 0,
                      colour_if = "green",
                      colour_else = "red",
                      digits = 2) {
  color_if(
    x = x,
    columns = columns,
    predicate = predicate,
    value = value,
    color_if = colour_if,
    color_else = colour_else,
    digits = digits
  )
}
