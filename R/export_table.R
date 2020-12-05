#' Data frame and Tables Pretty Formatting
#'
#' @param x A data frame.
#' @param sep Column separator.
#' @param header Header separator. Can be \code{NULL}.
#' @param format Name of output-format, as string. If \code{NULL} (or \code{"text"}),
#'   returned output is used for basic printing. Currently, only \code{"markdown"} is
#'   supported, or \code{NULL} (the default) resp. \code{"text"} for plain text.
#' @param caption,subtitle Table caption and subtitle, as string. If \code{NULL},
#'   no caption or subtitle is printed.
#' @param footer Table footer, as string. For markdown-formatted tables, table
#'   footers, due to the limitation in markdown rendering, are actually just a
#'   new text line under the table.
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
#' @note This function is going to be renamed in a future update. Please use its
#' alias \code{export_table()}.
#'
#' @return A data frame in character format.
#' @examples
#' cat(export_table(iris))
#' cat(export_table(iris, sep = " ", header = "*", digits = 1))
#' @export
export_table <- function(x,
                         sep = " | ",
                         header = "-",
                         digits = 2,
                         protect_integers = TRUE,
                         missing = "",
                         width = NULL,
                         format = NULL,
                         caption = NULL,
                         subtitle = NULL,
                         align = NULL,
                         footer = NULL,
                         zap_small = FALSE) {

  # check args
  if (is.null(format)) {
    format <- "text"
  }

  if (format == "md") {
    format <- "markdown"
  }

  # single data frame
  if (is.data.frame(x)) {
    if (is.null(caption)) {
      caption <- attributes(x)$table_caption
    }
    if (is.null(subtitle)) {
      subtitle <- attributes(x)$table_subtitle
    }
    if (is.null(footer)) {
      footer <- attributes(x)$table_footer
    }
    out <- .export_table(
      x = x,
      sep = sep,
      header = header,
      digits = digits,
      protect_integers = protect_integers,
      missing = missing,
      width = width,
      format = format,
      caption = caption,
      subtitle = subtitle,
      align = align,
      footer = footer,
      zap_small = zap_small
    )
  } else if (is.list(x)) {
    # list of data frames
    tmp <- lapply(.compact_list(x), function(i) {
      .export_table(
        x = i,
        sep = sep,
        header = header,
        digits = digits,
        protect_integers = protect_integers,
        missing = missing,
        width = width,
        format = format,
        caption = attributes(i)$table_caption,
        subtitle = attributes(i)$table_subtitle,
        align = align,
        footer = attributes(i)$table_footer,
        zap_small = zap_small
      )
    })
    out <- c()
    if (format == "text") {
      for (i in 1:length(tmp)) {
        out <- paste0(out, tmp[[i]], "\n")
      }
      out <- substr(out, 1, nchar(out) - 1)
    } else if (format == "markdown") {
      for (i in 1:length(tmp)) {
        out <- c(out, tmp[[i]], "")
      }
      out <- out[1:(length(out) - 1)]
    }
  } else {
    return(NULL)
  }

  if (format == "markdown") {
    attr(out, "format") <- "pipe"
    class(out) <- c("knitr_kable", "character")
  }
  out
}


#' @rdname export_table
#' @export
format_table <- export_table








# create matrix of raw table layout --------------------


.export_table <- function(x, sep = " | ", header = "-", digits = 2, protect_integers = TRUE, missing = "", width = NULL, format = NULL, caption = NULL, subtitle = NULL, align = NULL, footer = NULL, zap_small = FALSE) {
  df <- x

  # round all numerics
  col_names <- names(df)
  df <- as.data.frame(sapply(df, function(i) {
    if (is.numeric(i)) {
      format_value(i, digits = digits, protect_integers = protect_integers, missing = missing, width = width, zap_small = zap_small)
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

  if (format == "text") {
    out <- .format_basic_table(final, header, sep, caption = caption, subtitle = subtitle, footer = footer)
  } else if (format == "markdown") {
    out <- .format_markdown_table(final, x, caption = caption, subtitle = subtitle, footer = footer, align = align)
  }

  out
}






# plain text formatting ------------------------


.format_basic_table <- function(final, header, sep, caption = NULL, subtitle = NULL, footer = NULL) {
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

  if (!is.null(caption)) {
    if (length(caption) == 2 && .is_valid_colour(caption[2])) {
      caption <- .colour(caption[2], caption[1])
    }
    if (!is.null(subtitle)) {
      if (length(subtitle) == 2 && .is_valid_colour(subtitle[2])) {
        subtitle <- .colour(subtitle[2], subtitle[1])
      }
    } else {
      subtitle <- ""
    }
    title_line <- .trim(paste0(caption[1], " ", subtitle[1]))
    rows <- paste0(title_line, "\n\n", rows)
  }

  if (!is.null(footer)) {
    if (length(footer) == 2 && .is_valid_colour(footer[2])) {
      footer <- .colour(footer[2], footer[1])
    }
    rows <- paste0(rows, footer[1])
  }

  rows
}






# markdown formatting -------------------


.format_markdown_table <- function(final, x, caption = NULL, subtitle = NULL, footer = NULL, align = NULL) {
  column_width <- nchar(final[1, ])
  n_columns <- ncol(final)
  first_row_leftalign <- (!is.null(align) && align == "firstleft")

  ## create header line for markdown table -----
  header <- "|"

  # go through all columns of the data frame
  for (i in 1:n_columns) {

    # create separater line for current column
    line <- paste0(rep_len("-", column_width[i]), collapse = "")

    # check if user-defined alignment is requested, and if so, extract
    # alignment direction and save to "align_char"
    align_char <- ""
    if (!is.null(align)) {
      if (align %in% c("left", "right", "center", "firstleft")) {
        align_char <- ""
      } else {
        align_char <- substr(align, i, i)
      }
    }

    # auto-alignment?
    if (is.null(align)) {

      # if so, check if string in column starts with
      # whitespace (indicating right-alignment) or not.
      if (grepl("^\\s", final[2, i])) {
        line <- paste0(line, ":")
        final[, i] <- format(final[, i], width = column_width[i] + 1, justify = "right")
      } else {
        line <- paste0(":", line)
        final[, i] <- format(final[, i], width = column_width[i] + 1, justify = "left")
      }

      # left alignment, or at least first line only left?
    } else if (align == "left" || (first_row_leftalign && i == 1) || align_char == "l") {
      line <- paste0(":", line)
      final[, i] <- format(final[, i], width = column_width[i] + 1, justify = "left")

      # right-alignment
    } else if (align == "right" || align_char == "r") {
      line <- paste0(line, ":")
      final[, i] <- format(final[, i], width = column_width[i] + 1, justify = "right")

      # else, center
    } else {
      line <- paste0(":", line, ":")
      final[, i] <- format(final[, i], width = column_width[i] + 2, justify = "centre")
    }

    # finally, we have our header-line that indicates column alignments
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
    if (!is.null(subtitle)) {
      caption[1] <- paste0(caption[1], " ", subtitle[1])
    }
    rows <- c(paste0("Table: ", .trim(caption[1])), "", rows)
  }

  if (!is.null(footer)) {
    rows <- c(rows, footer[1])
  }

  rows
}
