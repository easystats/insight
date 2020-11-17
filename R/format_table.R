#' Data frame and Tables Pretty Formatting
#'
#' @param x A data frame.
#' @param sep Column separator.
#' @param header Header separator. Can be \code{NULL}.
#' @param format Name of output-format, as string. If \code{NULL}, returned
#'   output is used for basic printing. Currently, only \code{"markdown"} is
#'   supported, or \code{NULL} (the default) for plain text.
#' @param caption Table caption. Only applies to markdown-formatted tables.
#' @param align Column alignment. Only applies to markdown-formatted tables.
#'   By default \code{align = NULL}, numeric columns are right-aligned,
#'   and other columns are left-aligned. May be a string to indicate alignment
#'   rules for the complete table, like \code{"left"}, \code{"right"},
#'   \code{"center"} or \code{"firstleft"} (to left-align first column,
#'   center remaining); or maybe a string with abbreviated alignment characters,
#'   where the length of the string must equal the number of columns, for
#'   instance, \code{align = "lccrl"} would left-align the first column, center
#'   the second and third, right-align column four and left-align the fifth
#'   column.
#' @inheritParams format_value
#'
#' @return A data frame in character format.
#' @examples
#' cat(format_table(iris))
#' cat(format_table(iris, sep = " ", header = "*", digits = 1))
#' @export
format_table <- function(x, sep = " | ", header = "-", digits = 2, protect_integers = TRUE, missing = "", width = NULL, format = NULL, caption = NULL, align = NULL) {
  df <- x

  # round all numerics
  col_names <- names(df)
  df <- as.data.frame(sapply(df, function(i) {
    if (is.numeric(i)) {
      format_value(i, digits = digits, protect_integers = protect_integers, missing = missing, width = width)
    } else {
      i
    }
  }, simplify = FALSE), stringsAsFactors = FALSE)


  # Convert to character
  df <- as.data.frame(sapply(df, as.character, simplify = FALSE), stringsAsFactors = FALSE)
  names(df) <- col_names
  df[is.na(df)] <- as.character(missing)

  # Add colnames as row
  df <- rbind(colnames(df), df)

  # Align
  aligned <- format(df, justify = "right")

  # Center first row
  first_row <- as.character(aligned[1, ])
  for (i in 1:length(first_row)) {
    aligned[1, i] <- format(trimws(first_row[i]), width = nchar(first_row[i]), justify = "right")
  }

  final <- as.matrix(aligned)

  # left-align first column (if a character or a factor)
  if (!is.numeric(x[, 1])) {
    final[, 1] <- format(trimws(final[, 1]), justify = "left")
  }

  if (is.null(format)) {
    .format_basic_table(final, header, sep)
  } else if (format == "markdown") {
    .format_markdown_table(final, x, caption = caption, align = align)
  }
}






.format_basic_table <- function(final, header, sep) {
  # Transform to character
  rows <- c()
  for (row in 1:nrow(final)) {
    final_row <- paste0(final[row, ], collapse = sep)
    rows <- paste0(rows, final_row, sep = "\n")

    # First row separation
    if (row == 1) {
      if (!is.null(header)) {
        rows <- paste0(rows, paste0(rep_len(header, nchar(final_row)), collapse = ""), sep = "\n")
      }
    }
  }
  rows
}




.format_markdown_table <- function(final, x, caption = NULL, align = NULL) {
  column_width <- nchar(final[1, ])
  n_columns <- ncol(final)
  first_row_leftalign <- (!is.null(align) && align == "firstleft")

  header <- "|"
  for (i in 1:n_columns) {
    line <- paste0(rep_len("-", column_width[i]), collapse = "")
    align_char <- substr(align, i, i)
    if (is.null(align)) {
      if (grepl("^\\s", final[2, i])) {
        line <- paste0(line, ":")
        final[, i] <- format(final[, i], width = column_width[i] + 1, justify = "right")
      } else {
        line <- paste0(":", line)
        final[, i] <- format(final[, i], width = column_width[i] + 1, justify = "left")
      }
    } else if (align == "left" || (first_row_leftalign && i == 1) || align_char == "l") {
      line <- paste0(line, ":")
      final[, i] <- format(final[, i], width = column_width[i] + 1, justify = "left")
    } else if (align == "right" || (first_row_leftalign && i == 1) || align_char == "r") {
      line <- paste0(":", line)
      final[, i] <- format(final[, i], width = column_width[i] + 1, justify = "right")
    } else {
      line <- paste0(":", line, ":")
      final[, i] <- format(final[, i], width = column_width[i] + 2, justify = "centre")
    }
    header <- paste0(header, line, "|")
  }

  # Transform to character
  rows <- c()
  for (row in 1:nrow(final)) {
    final_row <- paste0("|", paste0(final[row, ], collapse = "|"), "|", collapse = "")
    rows <- c(rows, final_row)

    # First row separation
    if (row == 1) {
      rows <- c(rows, header)
    }
  }

  if (!is.null(caption)) {
    rows <- c(paste0("Table: ", caption), "", rows)
  }

  attr(rows, "format") <- "pipe"
  class(rows) <- c("knitr_kable", "character")
  rows
}
