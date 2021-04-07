#' Data frame and Tables Pretty Formatting
#'
#' @param x A data frame.
#' @param sep Column separator.
#' @param header Header separator. Can be \code{NULL}.
#' @param empty_line Separator used for empty lines. If \code{NULL}, line remains
#'   empty (i.e. filled with whitespaces).
#' @param format Name of output-format, as string. If \code{NULL} (or \code{"text"}),
#'   returned output is used for basic printing. Can be one of \code{NULL} (the
#'   default) resp. \code{"text"} for plain text, \code{"markdown"} (or
#'   \code{"md"}) for markdown and \code{"html"} for HTML output.
#' @param title,caption,subtitle Table title (same as caption) and subtitle, as strings. If \code{NULL},
#'   no title or subtitle is printed, unless it is stored as attributes (\code{table_title},
#'   or its alias \code{table_caption}, and \code{table_subtitle}).
#' @param footer Table footer, as string. For markdown-formatted tables, table
#'   footers, due to the limitation in markdown rendering, are actually just a
#'   new text line under the table.
#' @param align Column alignment. For markdown-formatted tables, the default
#'   \code{align = NULL} will right-align numeric columns, while all other
#'   columns will be left-aligned. If \code{format = "html"}, the default is
#'   left-align first column and center all remaining. May be a string to
#'   indicate alignment rules for the complete table, like \code{"left"},
#'   \code{"right"}, \code{"center"} or \code{"firstleft"} (to left-align first
#'   column, center remaining); or maybe a string with abbreviated alignment
#'   characters, where the length of the string must equal the number of columns,
#'   for instance, \code{align = "lccrl"} would left-align the first column, center
#'   the second and third, right-align column four and left-align the fifth
#'   column. For HTML-tables, may be one of \code{"center"}, \code{"left"} or
#'   \code{"right"}.
#' @param group_by Name of column in \code{x} that indicates grouping for tables.
#'   Only applies when \code{format = "html"}. \code{group_by} is passed down
#'   to \code{gt::gt(groupname_col = group_by)}.
#' @inheritParams format_value
#' @inheritParams get_data
#'
#' @note The values for \code{caption}, \code{subtitle} and \code{footer}
#'   can also be provided as attributes of \code{x}, e.g. if \code{caption = NULL}
#'   and \code{x} has attribute \code{table_caption}, the value for this
#'   attribute will be used as table caption. \code{table_subtitle} is the
#'   attribute for \code{subtitle}, and \code{table_footer} for \code{footer}.
#'
#' @return A data frame in character format.
#' @examples
#' cat(export_table(iris))
#' cat(export_table(iris, sep = " ", header = "*", digits = 1))
#' \dontrun{
#' # colored footers
#' data(iris)
#' x <- as.data.frame(iris[1:5, ])
#' attr(x, "table_footer") <- c("This is a yellow footer line.", "yellow")
#' cat(export_table(x))
#'
#' attr(x, "table_footer") <- list(
#'   c("\nA yellow line", "yellow"),
#'   c("\nAnd a red line", "red"),
#'   c("\nAnd a blue line", "blue")
#' )
#' cat(export_table(x))
#'
#' attr(x, "table_footer") <- list(
#'   c("Without the ", "yellow"),
#'   c("new-line character ", "red"),
#'   c("we can have multiple colors per line.", "blue")
#' )
#' cat(export_table(x))
#' }
#' @export
export_table <- function(x,
                         sep = " | ",
                         header = "-",
                         empty_line = NULL,
                         digits = 2,
                         protect_integers = TRUE,
                         missing = "",
                         width = NULL,
                         format = NULL,
                         title = NULL,
                         caption = title,
                         subtitle = NULL,
                         footer = NULL,
                         align = NULL,
                         group_by = NULL,
                         zap_small = FALSE,
                         verbose = TRUE) {

  # check args
  if (is.null(format)) {
    format <- "text"
  }

  if (format == "md") {
    format <- "markdown"
  }

  # sanity check
  if (is.null(x) || (is.data.frame(x) && nrow(x) == 0) || .is_empty_object(x)) {
    if (isTRUE(verbose)) {
      message(paste0("Can't export table to ", format, ", data frame is empty."))
    }
    return(NULL)
  }

  # if we have a list of data frame and HTML format, create a single
  # data frame now...
  if (identical(format, "html") && !is.data.frame(x) && is.list(x)) {
    x <- do.call(rbind, lapply(x, function(i) {
      attr_name <- .check_caption_attr_name(i)
      i$Component <- attr(i, attr_name)[1]
      i
    }))
  }


  # single data frame
  if (is.data.frame(x)) {
    if (!is.null(title)) {
      caption <- title
    }
    if (is.null(caption)) {
      attr_name <- .check_caption_attr_name(x)
      caption <- attributes(x)[[attr_name]]
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
      footer = footer,
      align = align,
      group_by = group_by,
      zap_small = zap_small,
      empty_line = empty_line
    )
  } else if (is.list(x)) {
    # list of data frames
    tmp <- lapply(.compact_list(x), function(i) {
      attr_name <- .check_caption_attr_name(i)
      .export_table(
        x = i,
        sep = sep,
        header = header,
        digits = digits,
        protect_integers = protect_integers,
        missing = missing,
        width = width,
        format = format,
        caption = attributes(i)[[attr_name]],
        subtitle = attributes(i)$table_subtitle,
        footer = attributes(i)$table_footer,
        align = align,
        group_by = group_by,
        zap_small = zap_small,
        empty_line = empty_line
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



# check whether "table_caption" or its alias "table_title" is used as attribute
.check_caption_attr_name <- function(x) {
  attr_name <- "table_caption"
  if (is.null(attr(x, attr_name, exact = TRUE)) && !is.null(attr(x, "table_title", exact = TRUE))) {
    attr_name <- "table_title"
  }
  attr_name
}



# create matrix of raw table layout --------------------


.export_table <- function(x, sep = " | ", header = "-", digits = 2, protect_integers = TRUE, missing = "", width = NULL, format = NULL, caption = NULL, subtitle = NULL, footer = NULL, align = NULL, group_by = NULL, zap_small = FALSE, empty_line = NULL) {
  df <- as.data.frame(x)

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

  if (identical(format, "html")) {
    out <- .format_html_table(df, caption = caption, subtitle = subtitle, footer = footer, align = align, group_by = group_by)
  } else {
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
      out <- .format_basic_table(final, header, sep, caption = caption, subtitle = subtitle, footer = footer, align = align, empty_line = empty_line)
    } else if (format == "markdown") {
      out <- .format_markdown_table(final, x, caption = caption, subtitle = subtitle, footer = footer, align = align)
    }
  }

  out
}






# plain text formatting ------------------------


.format_basic_table <- function(final, header, sep, caption = NULL, subtitle = NULL, footer = NULL, align = NULL, empty_line = NULL) {

  # align table, if requested
  if (!is.null(align) && length(align) == 1) {
    for (i in 1:ncol(final)) {
      align_char <- ""
      if (align %in% c("left", "right", "center", "firstleft")) {
        align_char <- ""
      } else {
        align_char <- substr(align, i, i)
      }

      # left alignment, or at least first line only left?
      if (align == "left" || (align == "firstleft" && i == 1) || align_char == "l") {
        final[, i] <- format(trimws(final[, i]), justify = "left")

        # right-alignment
      } else if (align == "right" || align_char == "r") {
        final[, i] <- format(trimws(final[, i]), justify = "right")

        # else, center
      } else {
        final[, i] <- format(trimws(final[, i]), justify = "centre")
      }
    }
  }

  # Transform to character
  rows <- c()
  for (row in 1:nrow(final)) {
    final_row <- paste0(final[row, ], collapse = sep)
    # check if we have an empty row
    if (!is.null(empty_line) && all(nchar(trimws(final[row, ])) == 0)) {
      rows <- paste0(rows, paste0(rep_len(empty_line, nchar(final_row)), collapse = ""), sep = "\n")
    } else {
      rows <- paste0(rows, final_row, sep = "\n")
    }

    # First row separation
    if (row == 1) {
      if (!is.null(header)) {
        rows <- paste0(rows, paste0(rep_len(header, nchar(final_row)), collapse = ""), sep = "\n")
      }
    }
  }

  if (!is.null(caption) && caption != "") {
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
    # paste everything together and remove unnecessary double spaces
    title_line <- .trim(paste0(caption[1], " ", subtitle[1]))
    title_line <- gsub("  ", " ", title_line, fixed = TRUE)
    rows <- paste0(title_line, "\n\n", rows)
  }

  if (!is.null(footer)) {
    if (is.list(footer)) {
      for (i in footer) {
        rows <- .paste_footers(i, rows)
      }
    } else {
      rows <- .paste_footers(footer, rows)
    }
  }

  rows
}


# helper ----------------

.paste_footers <- function(footer, rows) {
  if (.is_empty_string(footer)) {
    return(rows)
  }
  if (length(footer) == 2 && .is_valid_colour(footer[2])) {
    footer <- .colour(footer[2], footer[1])
  }
  paste0(rows, footer[1])
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
    if (is.list(footer)) {
      for (i in footer) {
        if (!.is_empty_string(i)) {
          rows <- c(rows, i[1])
        }
      }
    } else if (!.is_empty_string(footer)) {
      rows <- c(rows, footer[1])
    }
  }

  rows
}





# html formatting ---------------------------

.format_html_table <- function(final, caption = NULL, subtitle = NULL, footer = NULL, align = "center", group_by = NULL) {
  if (!requireNamespace("gt", quietly = TRUE)) {
    stop("Package 'gt' required to create HTML tables. Please install it.", call. = FALSE)
  }

  if (is.null(align)) {
    align <- "firstleft"
  }

  group_by_columns <- c(intersect(c("Group", "Response", "Effects", "Component"), names(final)), group_by)
  if (!length(group_by_columns)) {
    group_by_columns <- NULL
  } else {
    # remove columns with only 1 unique value - this *should* be safe to
    # remove, but we may check if all printed sub titles look like intended
    for (i in group_by_columns) {
      if (.n_unique(final[[i]]) <= 1) {
        final[[i]] <- NULL
      }
    }
  }

  tab <- gt::gt(final, groupname_col = group_by_columns)
  header <- gt::tab_header(tab, title = caption, subtitle = subtitle)
  footer <- gt::tab_source_note(header, source_note = footer)
  out <- gt::cols_align(footer, align = "center")

  # align columns
  if (!is.null(out[["_boxhead"]]) && !is.null(out[["_boxhead"]]$column_align)) {
    if (align == "firstleft") {
      out[["_boxhead"]]$column_align[1] <- "left"
    } else {
      col_align <- c()
      for (i in 1:nchar(align)) {
        col_align <- c(col_align, switch(substr(align, i, i),
          "l" = "left",
          "r" = "right",
          "center"
        ))
      }
      out[["_boxhead"]]$column_align <- col_align
    }
  }

  out
}
