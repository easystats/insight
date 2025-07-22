#' @title Data frame and Tables Pretty Formatting
#' @name export_table
#'
#' @description Function to export data frames into tables, which can be printed
#' to the console, or displayed in markdown or HTML format (and thereby, exported
#' to other formats like Word or PDF). The table width is automatically adjusted
#' to fit into the width of the display device (e.g., width of console). Use
#' the `table_width` argument to control this behaviour.
#'
#' @param x A data frame. May also be a list of data frames, to export multiple
#'   data frames into multiple tables.
#' @param sep Column separator.
#' @param header Header separator. Can be `NULL`.
#' @param cross Character that is used where separator and header lines cross.
#' @param empty_line Separator used for empty lines. If `NULL`, line remains
#'   empty (i.e. filled with whitespaces).
#' @param format Name of output-format, as string. If `NULL` (or `"text"`),
#'   returned output is used for basic printing. Can be one of `NULL` (the
#'   default) resp. `"text"` for plain text, `"markdown"` (or `"md"`) for
#'   markdown and `"html"` for HTML output. A special option is `"tt"`, which
#'   creates a [`tinytable::tt()`] object, where the output format is dependent
#'   on the context where the table is used, i.e. it can be markdown format when
#'   `export_table()` is used in markdown files, or LaTeX format when creating
#'   PDFs etc.
#' @param title,caption,subtitle Table title (same as caption) and subtitle, as
#'   strings. If `NULL`, no title or subtitle is printed, unless it is stored as
#'   attributes (`table_title`, or its alias `table_caption`, and
#'   `table_subtitle`). If `x` is a list of data frames, `caption` may be a list
#'   of table captions, one for each table.
#' @param footer Table footer, as string. For markdown-formatted tables, table
#'   footers, due to the limitation in markdown rendering, are actually just a
#'   new text line under the table. If `x` is a list of data frames, `footer`
#'   may be a list of table captions, one for each table.
#' @param align Column alignment. For markdown-formatted tables, the default
#'   `align = NULL` will right-align numeric columns, while all other columns
#'   will be left-aligned. If `format = "html"`, the default is left-align first
#'   column and center all remaining. May be a string to indicate alignment
#'   rules for the complete table, like `"left"`, `"right"`, `"center"` or
#'   `"firstleft"` (to left-align first column, center remaining); or a string
#'   with abbreviated alignment characters, where the length of the string must
#'   equal the number of columns. For instance, `align = "lccrl"` would
#'   left-align the first column, center the second and third, right-align
#'   column four and left-align the fifth column.
#' @param by Name of column(s) in `x` that indicates grouping for tables.
#'   When `format = "html"`, `by` is passed down to `gt::gt(groupname_col = by)`.
#'   Likewise, for `format = "tt"`, `by` indicates the name of the variable in
#'   the data frame, which is then used to create row headers in the table.
#'   For markdown and text format, `x` is internally split into a list of data
#'   frames. See also `row_groups` to group rows in the printed output.
#' @param width Refers to the width of columns (with numeric values). Can be
#'   either `NULL`, a number or a named numeric vector. If `NULL`, the width for
#'   each column is adjusted to the minimum required width. If a number, columns
#'   with numeric values will have the minimum width specified in `width`. If
#'   a named numeric vector, value names are matched against column names, and
#'   for each match, the specified width is used (see 'Examples'). Only applies
#'   to text-format (see `format`).
#' @param table_width Numeric,`"auto"`, `NULL` or `Inf`, indicating the width of
#'   the complete table.
#'   - If `table_width = "auto"` (default) and the table is wider than the
#'     current width (i.e. line length) of the console (or any other source for
#'     textual output, like markdown files), the table is split into multiple
#'     parts.
#'   - Else, if `table_width` is numeric and table rows are larger than
#'     `table_width`, the table is split into multiple parts. For each new table,
#'     the first column is repeated for better orientation.
#'   - Use `NULL` or `Inf` to turn off automatic splitting of the table.
#'   - `options(easystats_table_width = <value>)` can be used to set a default
#'     width for tables.
#' @param remove_duplicates Logical, if `TRUE` and table is split into multiple
#'   parts, duplicated ("empty") rows will be removed. If `FALSE`, empty rows
#'   will be preserved. Only applies when `table_width` is *not* `NULL` (or
#'   `Inf`) *and* table is split into multiple parts.
#' @param column_names Character vector of names that will be used as column
#'   names in the table. Must either be of same length as columns in the table,
#'   or a named vector, where names (LHS) indicate old column names, and values
#'   (RHS) are used as new column names.
#' @param row_groups Named list, can be used as alternative to `by` to group
#'   rows in the printed output, but in a more flexible way. List elements may
#'   either be character vectors that match the names of values in the first
#'   column of the data frame that belong to one group, or list elements can be
#'   row numbers of those value rows that should belong to one group. The names
#'   of the list elements will be used as group names, which will be inserted as
#'   "header row". Rows will be re-ordered according to the order used in
#'   `row_groups`, while all rows with non-matching values will be added to the
#'   end.
#' @param column_groups Named list, can be used to group columns in the printed
#'   output. List elements must indicate column indices for columns that should
#'   belong to one group. The names of the list elements will be used as group
#'   names, which will be inserted as "column header row". Currently only
#'   works for `format = "tt"` or `format = "html"`.
#' @param ... Arguments passed to [`tinytable::tt()`] and [`tinytable::style_tt()`]
#'   when `format = "tt"`.
#' @inheritParams format_value
#' @inheritParams get_data
#'
#' @note The values for `caption`, `subtitle` and `footer` can also be provided
#' as attributes of `x`, e.g. if `caption = NULL` and `x` has attribute
#' `table_caption`, the value for this attribute will be used as table caption.
#' `table_subtitle` is the attribute for `subtitle`, and `table_footer` for
#' `footer`.
#'
#' @inherit format_table seealso
#'
#' @return If `format = "text"` (or `NULL`), a formatted character string is
#' returned. `format = "markdown"` (or `"md"`) returns a character string of
#' class `knitr_kable`, which renders nicely in markdown files. `format = "html"`
#' returns an `gt` object (created by the **gt** package), which - by default -
#' is displayed in the IDE's viewer pane or default browser. This object can be
#' further modified with the various gt-functions. `format = "tt"` returns a
#' [`tinytable::tt()`] object, which is a lightweight table format that can be
#' used in markdown, LaTeX, HTML and other formats, depending on the context
#' where the table is used.
#'
#' @examples
#' export_table(head(iris))
#' export_table(head(iris), cross = "+")
#' export_table(head(iris), sep = " ", header = "*", digits = 1)
#'
#' # split longer tables
#' export_table(head(iris), table_width = 30)
#'
#' # group (split) tables by variables
#' export_table(head(mtcars, 8), by = "cyl")
#'
#' \donttest{
#' # colored footers
#' data(iris)
#' x <- as.data.frame(iris[1:5, ])
#' attr(x, "table_footer") <- c("This is a yellow footer line.", "yellow")
#' export_table(x)
#'
#' attr(x, "table_footer") <- list(
#'   c("\nA yellow line", "yellow"),
#'   c("\nAnd a red line", "red"),
#'   c("\nAnd a blue line", "blue")
#' )
#' export_table(x)
#'
#' attr(x, "table_footer") <- list(
#'   c("Without the ", "yellow"),
#'   c("new-line character ", "red"),
#'   c("we can have multiple colors per line.", "blue")
#' )
#' export_table(x)
#'
#' # rename column names
#' export_table(x, column_names = letters[1:5])
#' export_table(x, column_names = c(Species = "a"))
#' }
#'
#' # column-width
#' d <- data.frame(
#'   x = c(1, 2, 3),
#'   y = c(100, 200, 300),
#'   z = c(10000, 20000, 30000)
#' )
#' export_table(d)
#' export_table(d, width = 8)
#' export_table(d, width = c(x = 5, z = 10))
#' export_table(d, width = c(x = 5, y = 5, z = 10), align = "lcr")
#'
#' # group rows in the table
#' \dontrun{
#' data(mtcars)
#'
#' # fit model
#' mtcars$cyl <- as.factor(mtcars$cyl)
#' mtcars$gear <- as.factor(mtcars$gear)
#' model <- lm(mpg ~ hp + gear * vs + cyl + drat, data = mtcars)
#'
#' # model summary, don't select "Intercept" parameter
#' mp <- as.data.frame(format(
#'   parameters::model_parameters(model, drop = "^\\(Intercept")
#' ))
#'
#' # define groups for the table
#' groups <- list(
#'   Engine = c("cyl [6]", "cyl [8]", "vs", "hp"),
#'   Interactions = c(8, 9),
#'   Controls = c(2, 3, 7)
#' )
#'
#' # export table with groups, using tinytable format
#' export_table(mp, format = "tt", row_groups = groups)
#' }
#' @export
export_table <- function(x,
                         sep = " | ",
                         header = "-",
                         cross = NULL,
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
                         column_names = NULL,
                         align = NULL,
                         by = NULL,
                         zap_small = FALSE,
                         table_width = "auto",
                         remove_duplicates = FALSE,
                         row_groups = NULL,
                         column_groups = NULL,
                         verbose = TRUE,
                         ...) {
  # check args
  if (is.null(format)) {
    format <- "text"
  }

  # check args
  format <- validate_argument(format, c("text", "markdown", "md", "html", "tt"))

  # handle alias
  if (format == "md") {
    format <- "markdown"
  }

  # validation check
  if (is.null(x) || (is.data.frame(x) && nrow(x) == 0) || is_empty_object(x)) {
    if (isTRUE(verbose)) {
      format_alert("Can't export table to ", format, ", data frame is empty.")
    }
    return(NULL)
  }


  # if we have a list of data frames and HTML format, create a single
  # data frame now. HTML format needs a single data frame. Sub tables
  # are split by their group-column later, see code below
  # "gt(final, groupname_col = group_by_columns)".
  x <- .bind_html_tables(x, format)

  # check for indention
  indent_groups <- attributes(x)$indent_groups

  # check dots for alias name of "indent_rows"
  if (is.null(row_groups)) {
    row_groups <- attributes(x)$indent_rows
  }

  # split data frames?
  x <- .split_tables(x, by, format)

  # setup common arguments for table formatting -----------------------------

  export_args <- list(
    sep = sep,
    header = header,
    cross = cross,
    digits = digits,
    protect_integers = protect_integers,
    missing = missing,
    width = width,
    format = format,
    column_names = column_names,
    align = align,
    group_by = by,
    zap_small = zap_small,
    empty_line = empty_line,
    indent_groups = indent_groups,
    row_groups = row_groups,
    column_groups = column_groups,
    table_width = table_width,
    remove_duplicated_lines = remove_duplicates,
    verbose = verbose
  )

  # table from single data frame --------------------------------------------

  if (is.data.frame(x)) {
    # check default attributes for caption, sub-title and footer
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

    # add remaining arguments to export_args
    export_args$caption <- caption
    export_args$subtitle <- subtitle
    export_args$footer <- footer

    # convert data frame into specified output format
    out <- do.call(.export_table, c(list(x = x), export_args, list(...)))

  } else if (is.list(x)) {
    # table from list of data frames -----------------------------------------

    # remove empty elements
    l <- compact_list(x)

    # list of data frames
    tmp <- lapply(seq_along(l), function(element) {
      i <- l[[element]]

      # use individual footer for each list element...
      t_footer <- attributes(i)$table_footer

      # ...unless we have a footer-argument.
      # Then use this as last (final) footer
      if (element == length(l) && is.null(attributes(i)$table_footer) && !is.null(footer) && !is.list(footer)) {
        t_footer <- footer
      }

      # if we still have no footer, check if user provided a list of titles
      if (is.null(t_footer) && !is.null(footer) && is.list(footer) && length(footer) == length(l)) {
        t_footer <- footer[[element]]
      }


      # for lists of data frame, each list element may have
      # an own attribute for the title, to have "subheadings"
      # for each table

      attr_name <- .check_caption_attr_name(i)

      # if only alias "title" is provided, copy it to caption-variable
      if (!is.null(title) && is.null(caption)) {
        caption <- title
      }

      # use individual title for each list element...
      t_title <- attributes(i)[[attr_name]]

      # ...unless we have a title-argument.
      # Then use this as first (main) header
      if (element == 1 && is.null(attributes(i)[[attr_name]]) && !is.null(caption) && !is.list(caption)) {
        t_title <- caption
      }

      # if we still have no title, check if user provided a list of titles
      if (is.null(t_title) && !is.null(caption) && is.list(caption) && length(caption) == length(l)) {
        t_title <- caption[[element]]
      }

      # add remaining arguments to export_args
      export_args$caption <- t_title
      export_args$subtitle <- attributes(i)$table_subtitle
      export_args$footer <- t_footer

      # convert data frame into specified output format
      do.call(.export_table, c(list(x = i), export_args, list(...)))
    })

    # insert new lines between tables, but this is only needed
    # for text or markdown tables

    out <- NULL
    if (format == "text") {
      for (i in seq_along(tmp)) {
        out <- paste0(out, tmp[[i]], "\n")
      }
      out <- substr(out, 1, nchar(out) - 1)
    } else if (format == "markdown") {
      for (i in seq_along(tmp)) {
        out <- c(out, tmp[[i]], "")
      }
      out <- out[1:(length(out) - 1)]
    }
  } else {
    # for all invalid inputs, return NULL
    return(NULL)
  }

  # add specific knitr-attribute for proper printing inside rmarkdown
  if (format == "markdown") {
    attr(out, "format") <- "pipe"
    class(out) <- c("knitr_kable", "character")
  } else if (format == "text") {
    class(out) <- c("insight_table", class(out))
  }

  out
}


# methods -----------------------------------

#' @export
print.insight_table <- function(x, ...) {
  cat(x)
  invisible(x)
}


# small helper ----------------------

.bind_html_tables <- function(x, format = "html") {
  if (!is.data.frame(x) && is.list(x)) {
    if (identical(format, "html")) {
      x <- do.call(rbind, lapply(x, function(i) {
        attr_name <- .check_caption_attr_name(i)
        i$Component <- attr(i, attr_name)[1]
        i
      }))
    } else if (identical(format, "tt")) {
      # add table caption as group variable, and bind tables
      # we then extract row headers based on values in the group indices
      x <- do.call(rbind, lapply(x, function(i) {
        i$group <- attr(i, "table_caption")
        i
      }))
    }
  }

  x
}


# split data frame for text format - unlike HTML, where we need to bind
# lists of data frames to a single data frame and have a "group_by" variable,
# we need a list of data frames for text or markdown output (instead of a
# single data frame)
.split_tables <- function(x, by, format) {
  if (!is.null(by) && is.data.frame(x) && !format %in% c("html", "tt")) {
    # convert formula into string
    if (inherits(by, "formula")) {
      by <- all.vars(by)
    }
    # numeric indices are possible - just extract column names at that positions
    if (is.numeric(by)) {
      if (any(by < 1 || by > ncol(x))) {
        format_error("Indices in `by` cannot be lower than 1 or higher than the number of columns in the data frame.")
      }
      by <- colnames(x)[by]
    }
    # check if all by columns are in the data
    if (!all(by %in% colnames(x))) {
      suggestion <- .misspelled_string(colnames(x), by)
      msg <- "Not all variables in `by` were found in the data frame."
      if (is.null(suggestion$msg) || !length(suggestion$msg) || !nzchar(suggestion$msg)) {
        msg <- paste(msg, "Please use one of the following names:", .to_string(colnames(x)))
      } else {
        msg <- paste(msg, suggestion$msg)
      }
      format_error(msg)
    }
    # convert `by` columns into factor. we do this so split works with correct
    # order of values in "by" columns. If `by` columns are character vector,
    # `split()` sorts alpahbetically, which we don't want
    x[by] <- lapply(x[by], function(i) {
      factor(i, levels = unique(i))
    })

    # create titles based on group names and levels
    groups <- expand.grid(lapply(x[by], unique))
    groups[] <- lapply(groups, as.character)

    group_titles <- lapply(seq_len(nrow(groups)), function(i) {
      paste0(colnames(groups), "=", as.character(groups[i, ]), collapse = ", ")
    })

    # split data frames
    x <- split(x, x[by])

    # remove by-columns and set title
    x <- lapply(seq_along(x), function(i) {
      x[[i]][by] <- NULL
      attr(x[[i]], "table_title") <- c(paste("Group:", group_titles[[i]]), "blue")
      x[[i]]
    })
  }

  x
}


# check whether "table_caption" or its alias "table_title" is used as attribute
.check_caption_attr_name <- function(x) {
  attr_name <- "table_caption"
  if (is.null(attr(x, attr_name, exact = TRUE)) && !is.null(attr(x, "table_title", exact = TRUE))) {
    attr_name <- "table_title"
  }
  attr_name
}


# work horse: create matrix of raw table layout --------------------

.export_table <- function(x,
                          sep = " | ",
                          header = "-",
                          cross = NULL,
                          digits = 2,
                          protect_integers = TRUE,
                          missing = "",
                          width = NULL,
                          format = NULL,
                          caption = NULL,
                          subtitle = NULL,
                          footer = NULL,
                          column_names = NULL,
                          align = NULL,
                          group_by = NULL,
                          zap_small = FALSE,
                          empty_line = NULL,
                          indent_groups = NULL,
                          row_groups = NULL,
                          column_groups = NULL,
                          table_width = NULL,
                          remove_duplicated_lines = FALSE,
                          verbose = TRUE,
                          ...) {
  table_data <- as.data.frame(x)

  # rename columns?
  table_data <- .new_column_names(table_data, column_names)

  # check width argument, for format value. cannot have
  # named vector of length > 1 here
  if (is.null(width) || length(width) == 1) {
    col_width <- width
  } else {
    col_width <- NULL
  }

  # round all numerics, and convert to character
  col_names <- names(table_data)
  table_data[] <- lapply(table_data, function(i) {
    if (is.numeric(i)) {
      out <- format_value(i,
        digits = digits, protect_integers = protect_integers,
        missing = missing, width = col_width, zap_small = zap_small
      )
    } else {
      out <- i
    }
    as.character(out)
  })

  # add back column names
  names(table_data) <- col_names
  table_data[is.na(table_data)] <- as.character(missing)

  if (format %in% c("html", "tt")) {
    # html / tinytabl formatting starts here, needs
    # less preparation of table matrix
    fun_args <- list(
      table_data,
      caption = caption,
      subtitle = subtitle,
      footer = footer,
      align = align,
      group_by = group_by,
      column_groups = column_groups,
      row_groups = row_groups
    )
    if (format == "html") {
      out <- do.call(.format_html_table, fun_args)
    } else {
      out <- do.call(.format_tiny_table, c(fun_args, list(...)))
    }
    return(out)

    # text and markdown go here...
  } else {
    # Add colnames as first row to the data frame
    table_data <- rbind(colnames(table_data), table_data)

    # Initial alignment for complete data frame is right-alignment
    aligned <- format(table_data, justify = "right")

    # default alignment
    col_align <- rep("right", ncol(table_data))

    # first row definitely right alignment, fixed width
    first_row <- as.character(aligned[1, ])
    for (i in seq_along(first_row)) {
      aligned[1, i] <- format(
        trim_ws(first_row[i]),
        width = nchar(first_row[i], type = "width"),
        justify = "right"
      )
    }

    final <- as.matrix(aligned)

    # left-align first column (if a character or a factor)
    if (!is.numeric(x[, 1])) {
      final[, 1] <- format(trim_ws(final[, 1]), justify = "left")
      col_align[1] <- "left"
    }

    if (format == "text") {
      # go for simple text output
      out <- .format_basic_table(
        final,
        header,
        sep,
        cross,
        caption = caption,
        subtitle = subtitle,
        footer = footer,
        align = align,
        empty_line = empty_line,
        indent_groups = indent_groups,
        row_groups = row_groups,
        col_names = col_names,
        col_width = width,
        col_align = col_align,
        table_width = table_width,
        remove_duplicated_lines = remove_duplicated_lines,
        verbose = verbose
      )
    } else if (format == "markdown") {
      # markdown is a bit different...
      out <- .format_markdown_table(
        final,
        caption = caption,
        subtitle = subtitle,
        footer = footer,
        align = align,
        indent_groups = indent_groups,
        row_groups = row_groups
      )
    }
  }

  out
}


# plain text formatting ----------------------------------
# --------------------------------------------------------

.format_basic_table <- function(final,
                                header,
                                sep,
                                cross = NULL,
                                caption = NULL,
                                subtitle = NULL,
                                footer = NULL,
                                align = NULL,
                                empty_line = NULL,
                                indent_groups = NULL,
                                row_groups = NULL,
                                col_names = NULL,
                                col_width = NULL,
                                col_align = NULL,
                                table_width = NULL,
                                remove_duplicated_lines = FALSE,
                                verbose = TRUE) {
  # indent groups? export_table() allows to indent specific rows,
  # which might be useful when we have tables of regression coefficients,
  # and some coefficients are grouped together, which is visually emphasized
  # by indention...

  if (!is.null(indent_groups) && any(grepl(indent_groups, final[, 1], fixed = TRUE))) {
    final <- .indent_groups(final, indent_groups)
    skip_first_align <- TRUE
  } else if (!is.null(row_groups)) {
    final <- .row_groups(final, row_groups)$final
    skip_first_align <- TRUE
  } else {
    skip_first_align <- FALSE
  }

  # align table, if requested. unlike the generic aligments that have been done
  # previously, we now look for column-specific alignments. we furthermore save
  # the alignments in "col_align", which we may need later when we set a fixed
  # column width for specific columns across multiple tables, and have to
  # re-align the columns again.

  if (!is.null(align) && length(align) == 1) {
    for (i in seq_len(ncol(final))) {
      # if we have indented groups, we skip the first alignment, else all
      # indentions are lost
      if (skip_first_align && i == 1) {
        next
      }
      align_char <- ""
      if (!align %in% c("left", "right", "center", "firstleft")) {
        align_char <- substr(align, i, i)
      }

      # left alignment, or at least first line only left?
      if (align == "left" || (align == "firstleft" && i == 1) || align_char == "l") {
        final[, i] <- format(trim_ws(final[, i]), justify = "left")
        col_align[i] <- "left"

        # right-alignment
      } else if (align == "right" || align_char == "r") {
        final[, i] <- format(trim_ws(final[, i]), justify = "right")
        col_align[i] <- "right"

        # else, center
      } else {
        final[, i] <- format(trim_ws(final[, i]), justify = "centre")
        col_align[i] <- "centre"
      }
    }
  }


  # check for fixed column widths. usually, column width is aligned to the
  # widest element in that column. for multiple tables, this may result in
  # columns which do not have the the same width across tables, despite
  # having the same "meaning" (e.g., zero-inflated models that have a table
  # for the count and the zero-inflated model components: both have columns
  # for SE or CI, but one column may be 10 chars wide, the same column in
  # the other table could be 9 or 11 chars) - with this option, we can set a
  # fixed width for specific columns across all tables.

  if (!is.null(col_width) && length(col_width) > 1 && !is.null(names(col_width))) {
    matching_columns <- stats::na.omit(match(names(col_width), col_names))
    if (length(matching_columns)) {
      for (i in matching_columns) {
        w <- as.vector(col_width[col_names[i]])
        final[, i] <- format(trim_ws(final[, i]), width = w, justify = col_align[i])
      }
    }
  }


  # we can split very wide tables
  final_extra <- NULL
  overlength_warning <- FALSE

  # check if user requested automatic width-adjustment of tables,
  # or if a given width is required
  table_width_adjustment <- identical(table_width, "auto") || (!is.null(table_width) && !is.infinite(table_width) && is.numeric(table_width)) # nolint

  if (table_width_adjustment) {
    # define the length of a table line. if specific table width is defined
    # (i.e. table_width is numeric), use that to define length of table lines.
    # else, if "auto", check the current width of the user console and use that
    # to flexibly adjust table width to the width of the console window.

    if (is.numeric(table_width)) {
      line_width <- table_width
    } else {
      line_width <- getOption("easystats_table_width", getOption("width", 80))
    }

    # width of first table row of complete table. Currently, "final" is still
    # a matrix, so we need to paste the columns of the first row into a string
    row_width <- nchar(paste(final[1, ], collapse = sep), type = "width")

    # possibly first split - all table columns longer than "line_width"
    # (i.e. first table row) go into a second string
    if (row_width > line_width) {
      final_extra <- list(final)
      e <- 1
      while (nchar(paste(utils::tail(final_extra, 1)[[1]][1, ], collapse = sep), type = "width") > line_width && e <= length(final_extra)) { # nolint
        .final_temp <- final_extra[[e]]

        i <- 1
        # determine how many columns fit into the first line
        while (nchar(paste(.final_temp[1, 1:i], collapse = sep), type = "width") < line_width) {
          i <- i + 1
        }
        # copy first column, and all columns that did not fit into the first line
        # into the second table matrix
        if (i < ncol(.final_temp)) {
          if (i > 1) {
            final_extra[[e]] <- .final_temp[, 1:(i - 1), drop = FALSE]
          } else {
            # if first column is very long, i = 1, and then indexing 1:(i-1)
            # doesn't work. Just copy first column, and give a warning about
            # overlengthy columns
            final_extra[[e]] <- .final_temp[, 1, drop = FALSE]
            overlength_warning <- TRUE
          }
          # check if only the first column was copied, and no other columns -
          # if so, we will repeatedly only copy column one and move all other
          # columns in the next table part (where this happens again)
          # in this case, *don't* copy first column again
          if (i > 2) {
            # repeat first column in each table, for better orientation
            copy_range <- c(1, i:ncol(.final_temp))
          } else if (i == 2) {
            # here we have a very wide first column, so don't repeat it -
            # else, subsequent columns won't fit into table
            copy_range <- i:ncol(.final_temp)
          } else {
            # here we have an overlengthy first column - make sure we don't
            # process this column multiple times, so skip it here
            copy_range <- 2:ncol(.final_temp)
          }
          final_extra[[e + 1]] <- .final_temp[, copy_range, drop = FALSE]
        }
        e <- e + 1
      }

      final <- final_extra[[1]]
      if (length(final_extra) > 1) {
        final_extra <- final_extra[-1]
      } else {
        final_extra <- NULL
      }
    }
  }

  if (overlength_warning && verbose) {
    format_warning("The table contains very wide columns that don't fit into the available display-width of the console. Splitting tables into multiple parts did not have the desired effect.") # nolint
  }

  # Transform table matrix into a string value that can be printed
  rows <- .table_parts(
    rows = NULL,
    final = final,
    header = header,
    sep = sep,
    cross = cross,
    empty_line = empty_line,
    table_width_adjustment = table_width_adjustment,
    remove_duplicated_lines = remove_duplicated_lines
  )

  # if we have over-lengthy tables that are split into parts,
  # print extra table here
  if (!is.null(final_extra)) {
    for (fex in final_extra) {
      rows <- .table_parts(
        rows = paste0(rows, "\n"),
        final = fex,
        header = header,
        sep = sep,
        cross = cross,
        empty_line = empty_line,
        table_width_adjustment = table_width_adjustment,
        remove_duplicated_lines = remove_duplicated_lines
      )
    }
  }

  # if caption is available, add a row with a headline
  if (!is.null(caption) && caption[1] != "") {
    # if we have a colour value, make coloured ansi-string
    if (length(caption) == 2 && .is_valid_colour(caption[2])) {
      caption <- .colour(caption[2], caption[1])
    }
    if (is.null(subtitle)) {
      subtitle <- ""
    } else if (length(subtitle) == 2 && .is_valid_colour(subtitle[2])) {
      # if we have a colour value, make coloured ansi-string
      subtitle <- .colour(subtitle[2], subtitle[1])
    }

    # paste everything together and remove unnecessary double spaces
    title_line <- trim_ws(paste0(caption[1], " ", subtitle[1]))
    title_line <- gsub("  ", " ", title_line, fixed = TRUE)
    rows <- paste0(title_line, "\n\n", rows)
  }

  # if footer is available, add a row with a footer. footers may
  # also be provided as list of character vectors, so each footer
  # line can get its own color

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


# helper to prepare table body for text output ---------------------
# ------------------------------------------------------------------

.table_parts <- function(rows,
                         final,
                         header,
                         sep,
                         cross,
                         empty_line,
                         table_width_adjustment,
                         remove_duplicated_lines = FALSE) {
  # "final" is a matrix here. we now paste each row into a character string,
  # add separator chars etc.
  for (row in seq_len(nrow(final))) {
    # create a string for each row, where cells from original matrix are
    # separated by the separator char
    final_row <- paste(final[row, ], collapse = sep)
    # check if we have an empty row, and if so, fill with an
    # "empty line separator", if requested by user
    if (!is.null(empty_line) && !any(nzchar(trim_ws(final[row, ])))) {
      # check whether user wants to have a "cross" char where vertical and
      # horizontal lines (from empty line separator) cross. But don't add
      # a "cross" when the empty line is the last row in the table...
      empty_line_with_cross <- .insert_cross(
        # the empty line, which is just empty cells with separator char,
        # will now be replaced by the "empty line char", so we have a
        # clean separator line
        paste(rep_len(empty_line, nchar(final_row, type = "width")), collapse = ""),
        cross, sep, final_row,
        is_last_row = row == nrow(final)
      )
      rows <- paste0(rows, empty_line_with_cross, "\n")
    } else {
      rows <- paste0(rows, final_row, "\n")
    }

    # After first row, we might have a separator row
    if (row == 1 && !is.null(header)) { # check if we have any separator for the header row
      # check whether user wants to have a "cross" char where vertical and
      # horizontal lines (from header line) cross.
      header_line <- .insert_cross(
        paste(rep_len(header, nchar(final_row, type = "width")), collapse = ""),
        cross, sep, final_row,
        is_last_row = row == nrow(final)
      )
      # add separator row after header line
      rows <- paste0(rows, header_line, "\n")
    }
  }

  # we table is split into multiple parts, we remove duplicated separator lines
  if (table_width_adjustment) {
    # split single character back into rows again
    out <- unlist(strsplit(rows, "\\n"), use.names = FALSE)
    # find out where we have consecutive duplicated rows
    consecutive_dups <- which(out[-1] == out[-length(out)])
    if (length(consecutive_dups)) {
      # if second to last line is a separator line, *and* we usually also have
      # a final line, remove that one
      if (.is_separator_line(out[length(out) - 1], empty_line, cross, sep)) {
        consecutive_dups <- unique(c(consecutive_dups, length(out) - 1))
      }
      # copy consecutive duplicated rows into temporary object
      tmp <- out[consecutive_dups]
      # remove separator and cross signs
      if (!is.null(empty_line) && nzchar(empty_line)) {
        tmp <- gsub(empty_line, "", tmp, fixed = TRUE)
      }
      if (!is.null(cross) && nzchar(cross)) {
        tmp <- gsub(cross, "", tmp, fixed = TRUE)
      }
      if (!is.null(sep) && nzchar(sep)) {
        tmp <- gsub(sep, "", tmp, fixed = TRUE)
      }
      # now check which rows are empty (lines) AND consecutive duplicated
      # remove those from "out"
      remove_dups <- consecutive_dups[-nzchar(tmp)]
      # if all consecutive duplicates were empty
      if (!length(remove_dups)) {
        remove_dups <- consecutive_dups
      }
      # remove duplicated lines, or make them "empty"?
      if (remove_duplicated_lines) {
        out <- out[-remove_dups]
      } else if (!is.null(empty_line) && nzchar(empty_line)) {
        # when consecutive duplicated rows are removed, the different table
        # parts may have different height (number of rows). To avoid confusion,
        # it is possible to preserve the height of each table part by setting
        # remove_duplicates = FALSE
        out[remove_dups] <- gsub(empty_line, " ", out[remove_dups], fixed = TRUE)
        if (!is.null(sep) && nzchar(sep) && !is.null(cross) && nzchar(cross)) {
          out[remove_dups] <- gsub(trim_ws(cross), trim_ws(sep), out[remove_dups], fixed = TRUE)
        }
      }
      # collapse back into single string
      rows <- paste0(paste(out, collapse = "\n"), "\n")
    }
  }

  rows
}


.is_separator_line <- function(x, empty_line, cross, sep) {
  if (!is.null(empty_line) && nzchar(empty_line)) {
    x <- gsub(empty_line, "", x, fixed = TRUE)
  }
  if (!is.null(cross) && nzchar(cross)) {
    x <- gsub(cross, "", x, fixed = TRUE)
  }
  if (!is.null(sep) && nzchar(sep)) {
    x <- gsub(sep, "", x, fixed = TRUE)
  }
  !nzchar(x)
}


.insert_cross <- function(row, cross, sep, pattern, is_last_row) {
  # check if at places where row and column "lines" cross, we need a
  # special char. E.g., if columns are separated with "|" and header line
  # with "-", we might have a "+" to have properly "crossed lines"
  if (!is.null(cross) && !is_last_row) {
    cross_position <- unlist(gregexpr(trim_ws(sep), pattern, fixed = TRUE), use.names = FALSE)
    for (pp in cross_position) {
      substr(row, pp, pp) <- cross
    }
  }
  row
}


# helper -------------------------------------------------
# --------------------------------------------------------

.new_column_names <- function(table_data, column_names) {
  # new column names for the table?
  if (!is.null(column_names)) {
    # if we have no named vector, we assume that we have new names for each
    # column in the table
    if (is.null(names(column_names))) {
      # error if length does not match number of columns
      if (length(column_names) != ncol(table_data)) {
        format_error("Number of names in `column_names` does not match number of columns in data frame.")
      }
      colnames(table_data) <- column_names
    } else {
      # if we have a named vector, all elements must be named
      if (!all(nzchar(names(column_names)))) {
        format_error("If `column_names` is a named vector, all elements must be named.")
      }
      # if we have a named vector, all names must be present in column names
      if (!all(names(column_names) %in% colnames(table_data))) {
        suggestion <- .misspelled_string(colnames(table_data), names(column_names))
        msg <- "Not all names in `column_names` were found in column names of data frame."
        if (is.null(suggestion$msg) || !length(suggestion$msg) || !nzchar(suggestion$msg)) {
          msg <- paste(msg, "Please use one of the following names:", .to_string(colnames(table_data)))
        } else {
          msg <- paste(msg, suggestion$msg)
        }
        format_error(msg)
      }
      for (i in names(column_names)) {
        colnames(table_data)[colnames(table_data) == i] <- column_names[i]
      }
    }
  }
  table_data
}


.paste_footers <- function(footer, rows) {
  if (.is_empty_string(footer)) {
    return(rows)
  }
  # if we have a colour value, make coloured ansi-string
  if (length(footer) == 2 && .is_valid_colour(footer[2])) {
    footer <- .colour(footer[2], footer[1])
  }
  paste0(rows, footer[1])
}


# row grouping -------------------------------------------
# --------------------------------------------------------

.indent_groups <- function(final, indent_groups) {
  # check length of indent string
  whitespace <- sprintf("%*s", nchar(indent_groups, type = "width"), " ")

  # find start index of groups
  grps <- grep(indent_groups, final[, 1], fixed = TRUE)

  # create index for those rows that should be indented
  grp_rows <- seq(grps[1], nrow(final))
  grp_rows <- grp_rows[!grp_rows %in% grps]

  # indent
  final[grp_rows, 1] <- paste0(whitespace, final[grp_rows, 1])

  # remove indent token
  final[, 1] <- gsub(indent_groups, "", final[, 1], fixed = TRUE)

  # trim whitespace at end
  final[, 1] <- trimws(final[, 1], which = "right")

  # move group name (indent header) to left
  final[, 1] <- format(final[, 1], justify = "left", width = max(nchar(final[, 1], type = "width")))
  final
}


.row_groups <- function(final, row_groups, whitespace = "  ", remove_first = TRUE) {
  if (is.list(row_groups)) {
    # convert matrix to data frame, required for .row_groups_tt()
    new_final <- as.data.frame(final)
    if (remove_first) {
      # remove first row, which contains column names
      new_final <- new_final[-1, , drop = FALSE]
    }
    # if we have a list of row indices, we need to convert this into a vector
    # of row indices
    out <- .row_groups_tt(
      new_final,
      row_groups = row_groups,
      first_index_only = FALSE,
      reorder = TRUE
    )
    # updated objects
    final <- as.matrix(out$final)
    row_index <- out$row_groups
    # insert header rows in final-matrix
    grps <- vapply(out$row_groups, function(i) i[1], numeric(1))
    for (j in length(grps):1) {
      if (grps[j] == 1) {
        final <- rbind(
          c(names(grps)[j], rep_len("", ncol(final) - 1)),
          final[1:nrow(final), , drop = FALSE]
        )
      } else {
        final <- rbind(
          final[1:(grps[j] - 1), , drop = FALSE],
          c(names(grps)[j], rep_len("", ncol(final) - 1)),
          final[grps[j]:nrow(final), , drop = FALSE]
        )
      }
      row_index[[j]] <- row_index[[j]] + j
    }
    row_index <- unlist(row_index, use.names = FALSE)
    # we have to add back column names again, and also format them
    if (remove_first) {
      final <- rbind(colnames(out$final), final)
    }
  } else {
    row_index <- row_groups
    grps <- NULL
  }

  # create index for those rows that should be indented
  # for text format, first row is column names, so we need a +1 here
  grp_rows <- row_index
  if (remove_first) {
    grp_rows <- grp_rows + 1
  }

  # indent
  final[grp_rows, 1] <- paste0(whitespace, final[grp_rows, 1])

  # find rows that should not be indented
  non_grp_rows <- setdiff(seq_len(nrow(final)), grp_rows)

  # justify columns
  for (i in 2:ncol(final)) {
    final[, i] <- format(final[, i], justify = "right")
  }
  # header row should be right-aligned, so we need to do this separately
  final[, 1] <- format(final[, 1], justify = ifelse(remove_first, "left", "right"))

  list(final = final, row_headers = non_grp_rows)
}


.row_groups_tt <- function(x,
                           row_groups = NULL,
                           group_by = NULL,
                           first_index_only = TRUE,
                           reorder = TRUE,
                           ...) {
  # check grouping - if we have a grouping variable in "group_by", we use this
  # for grouping rows. an alternative is to provide the "row_groups" argument,
  # which is a list of row indices, or parameter names, that should be grouped
  # together.
  if (!is.null(group_by)) {
    groups <- data.frame(
      group_by = factor(x[[group_by]], levels = unique(x[[group_by]])),
      row = seq_len(nrow(x)),
      stringsAsFactors = FALSE
    )
    row_groups <- split(groups$row, groups$group_by)
    # remove no longer needed group variable
    x[[group_by]] <- NULL
  }
  # group rows? If we have row_groups already, these will be overwritten here
  if (!is.null(row_groups)) {
    # extract first column, which contains the "value" names
    values <- trim_ws(x[[1]])
    # now find matching row based on value name or row index
    row_index <- lapply(row_groups, function(g) {
      if (is.character(g)) {
        # groups were provided as values (string)
        match(g, values)
      } else {
        # else, we assume that the group is a row position
        g
      }
    })
    # sanity check - do all rows match a parameter?
    group_indices <- unlist(row_index, use.names = FALSE)
    if (anyNA(group_indices) || any(group_indices < 1) || any(group_indices > nrow(x))) {
      insight::format_error("Some group indices do not match any parameter.")
    }
    # if row indices are not sorted, we need to resort the parameters data frame
    if (reorder && is.unsorted(unlist(row_index))) {
      new_rows <- c(unlist(row_index), setdiff(seq_len(nrow(x)), unlist(row_index)))
      x <- x[new_rows, ]
      # we need to update indices in groups as well. Therefore, we need to convert
      # list of row indices into a vector with row indices, then subtract the
      # differences of old and new row positions, and then split that vector into
      # a list again
      row_index <- utils::relist(
        match(unlist(row_index, use.names = FALSE), new_rows),
        skeleton = row_index
      )
    }
    # we now just need the first index of each group, which is the first
    # row position of each group
    if (first_index_only) {
      row_groups <- lapply(seq_along(row_index), function(i) {
        row_index[[i]][1]
      })
    } else {
      row_groups <- row_index
    }

    # set element names
    names(row_groups) <- names(row_index)
  }
  list(final = x, row_groups = row_groups)
}


# tinytable formatting ----------------------------------
# --------------------------------------------------------

.format_tiny_table <- function(final,
                               caption = NULL,
                               subtitle = NULL,
                               footer = NULL,
                               align = NULL,
                               group_by = NULL,
                               column_groups = NULL,
                               row_groups = NULL,
                               ...) {
  # need data frame, not matrix
  final <- as.data.frame(final)
  if (!is.null(align) && length(align) == 1) {
    align <- switch(align,
      left = "l",
      center = "c",
      right = "r",
      firstleft = paste0("l", rep_len("r", ncol(final) - 1)),
      align
    )
  }

  check_if_installed("tinytable")

  # we need to indent rows first, because we re-order the rows here
  out <- .row_groups_tt(final, row_groups = row_groups, group_by = group_by, ...)
  # now create the tinytable object
  final <- tinytable::tt(out$final, caption = caption[1], notes = footer, ...)
  # insert sub header rows and column spans, if we have any
  if (!is.null(out$row_groups) || !is.null(column_groups)) {
    final <- tinytable::group_tt(final, i = out$row_groups, j = column_groups)
  }
  tinytable::style_tt(final, align = align, ...)
}


# markdown formatting ------------------------------
# --------------------------------------------------

.format_markdown_table <- function(final,
                                   caption = NULL,
                                   subtitle = NULL,
                                   footer = NULL,
                                   align = NULL,
                                   indent_groups = NULL,
                                   row_groups = NULL) {
  column_width <- nchar(final[1, ])
  n_columns <- ncol(final)
  first_row_leftalign <- (!is.null(align) && align == "firstleft")

  ## create header line for markdown table -----
  header <- "|"

  # indent groups?
  if (!is.null(indent_groups) && any(grepl(indent_groups, final[, 1], fixed = TRUE))) {
    final <- .indent_groups(final, indent_groups)
  } else if (!is.null(row_groups)) {
    final <- .row_groups(final, row_groups, whitespace = "")$final
  }

  # go through all columns of the data frame
  for (i in 1:n_columns) {
    # create separator line for current column
    tablecol <- paste0(rep_len("-", column_width[i]), collapse = "")

    # check if user-defined alignment is requested, and if so, extract
    # alignment direction and save to "align_char"
    align_char <- ""
    if (!is.null(align) && !align %in% c("left", "right", "center", "firstleft")) {
      align_char <- substr(align, i, i)
    }

    # auto-alignment?
    if (is.null(align)) {
      # if so, check if string in column starts with
      # whitespace (indicating right-alignment) or not.
      if (grepl("^\\s", final[2, i])) {
        tablecol <- paste0(tablecol, ":")
        final[, i] <- format(final[, i], width = column_width[i] + 1, justify = "right")
      } else {
        tablecol <- paste0(":", tablecol)
        final[, i] <- format(final[, i], width = column_width[i] + 1, justify = "left")
      }

      # left alignment, or at least first tablecol only left?
    } else if (align == "left" || (first_row_leftalign && i == 1) || align_char == "l") {
      tablecol <- paste0(":", tablecol)
      final[, i] <- format(final[, i], width = column_width[i] + 1, justify = "left")

      # right-alignment
    } else if (align == "right" || align_char == "r") {
      tablecol <- paste0(tablecol, ":")
      final[, i] <- format(final[, i], width = column_width[i] + 1, justify = "right")

      # else, center
    } else {
      tablecol <- paste0(":", tablecol, ":")
      final[, i] <- format(final[, i], width = column_width[i] + 2, justify = "centre")
    }

    # finally, we have our header-line that indicates column alignments
    header <- paste0(header, tablecol, "|")
  }

  # Transform to character
  rows <- NULL
  for (row in seq_len(nrow(final))) {
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
    rows <- c(paste0("Table: ", trim_ws(caption[1])), "", rows)
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


# html formatting ----------------------------------------
# --------------------------------------------------------

.format_html_table <- function(
  final,
  caption = NULL,
  subtitle = NULL,
  footer = NULL,
  align = "center",
  group_by = NULL,
  row_groups = NULL,
  column_groups = NULL,
  ...
) {
  check_if_installed("gt")

  if (is.null(align)) {
    align <- "firstleft"
  }

  group_by_columns <- c(
    intersect(c("Group", "Response", "Effects", "Component"), names(final)),
    group_by
  )
  if (length(group_by_columns)) {
    # remove columns with only 1 unique value - this *should* be safe to
    # remove, but we may check if all printed sub titles look like intended
    for (i in group_by_columns) {
      if (n_unique(final[[i]]) <= 1) {
        final[[i]] <- NULL
      }
    }
  } else {
    group_by_columns <- NULL
  }

  # indent groups?
  if (!is.null(row_groups)) {
    out <- .row_groups(final, row_groups, "\U00A0\U00A0", remove_first = FALSE)
    final <- out$final
    highlight_rows <- out$row_headers
  } else {
    highlight_rows <- NULL
  }

  # do we need to render <br>?
  any_br <- any(vapply(
    final,
    function(i) any(grepl("<br>", i, fixed = TRUE)),
    logical(1)
  ))
  any_nbsp <- any(vapply(
    final,
    function(i) any(grepl("&nbsp;", i, fixed = TRUE)),
    logical(1)
  ))

  # validation check - clean caption, subtitle and footer from ansi-colour codes,
  # which only work for text format... But if user occidentally provides colours
  # for HTML format as well, remove those colour codes, so they don't appear as
  # text in the table header and footer. Furthermore, in footers, we need to
  # remove newline-characters

  if (!is.null(caption)) {
    if (is.list(caption)) {
      caption <- lapply(caption, function(i) {
        i[1]
      })
    } else {
      caption <- caption[1]
    }
  }

  if (!is.null(subtitle)) {
    if (is.list(subtitle)) {
      subtitle <- lapply(subtitle, function(i) {
        i[1]
      })
    } else {
      subtitle <- subtitle[1]
    }
  }

  if (!is.null(footer)) {
    if (is.list(footer)) {
      footer <- lapply(footer, function(i) {
        gsub("\n", "", i[1], fixed = TRUE)
      })
    } else {
      footer <- footer[1]
    }
  }

  tab <- gt::gt(final, groupname_col = group_by_columns)
  header <- gt::tab_header(tab, title = caption, subtitle = subtitle)
  footer <- gt::tab_source_note(header, source_note = gt::html(footer))
  out <- gt::cols_align(footer, align = "center")

  # emphasize header of row groups?
  if (!is.null(highlight_rows) && length(highlight_rows)) {
    out <- gt::tab_style(
      out,
      style = gt::cell_text(style = "oblique"),
      locations = gt::cells_body(columns = 1, rows = highlight_rows)
    )
  }

  # emphasize row groups labels? when we have group_by_columns
  if (!is.null(group_by_columns)) {
    out <- gt::tab_style(
      out,
      style = gt::cell_text(style = "oblique"),
      locations = gt::cells_row_groups()
    )
  }

  # custom alignment of columns
  if (align == "firstleft") {
    out <- gt::cols_align(out, "left", 1)
  } else {
    for (i in 1:nchar(align)) {
      col_align <- switch(substr(align, i, i), l = "left", r = "right", "center")
      out <- gt::cols_align(out, col_align, i)
    }
  }

  # group columns?
  if (!is.null(column_groups)) {
    for (i in names(column_groups)) {
      out <- gt::tab_spanner(out, label = i, columns = column_groups[[i]])
    }
  }

  # correctly render line breaks like <br>
  if (any_br || any_nbsp) {
    out <- gt::fmt_markdown(out)
  }

  out
}
