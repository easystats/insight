#' @title Generic export of data frames into formatted tables
#' @name display
#'
#' @description `display()` is a generic function to export data frames
#' into various table formats (like plain text, markdown, ...). `print_md()`
#' usually is a convenient wrapper for `display(format = "markdown")`.
#' Similar, `print_html()` is a shortcut for `display(format = "html")`.
#' See the documentation for the specific objects' classes.
#'
#' @param object,x A data frame.
#' @param format String, indicating the output format. Can be `"markdown"` or
#' `"html"`. A special option is `"tt"`, which creates a [`tinytable::tt()`]
#' object, where the output format is dependent on the context where the table
#' is used, i.e. it can be markdown format when `export_table()` is used in
#' markdown files, or LaTex format when creating PDFs etc.
#' @param ... Arguments passed to other methods.
#'
#' @return Depending on `format`, either an object of class `gt_tbl`,
#' `tinytable`, or a character vector of class `knitr_kable`.
#'
#' @examplesIf all(check_if_installed(c("gt", "tinytable"), quietly = TRUE))
#' display(iris[1:5, ], format = "html")
#'
#' display(iris[1:5, ], format = "tt")
#'
#' display(iris[1:5, ], format = "markdown")
#' @export
display <- function(object, ...) {
  UseMethod("display")
}


#' @rdname display
#' @export
print_md <- function(x, ...) {
  UseMethod("print_md")
}


#' @rdname display
#' @export
print_html <- function(x, ...) {
  UseMethod("print_html")
}


# data.frame --------------------------------------------------------------

#' @rdname display
#' @export
display.data.frame <- function(object, format = "markdown", ...) {
  format <- validate_argument(format, c("md", "markdown", "html", "tt"))

  if (identical(format, "html")) {
    print_html(x = object, ...)
  } else if (identical(format, "tt")) {
    print_html(x = object, backend = "tt", ...)
  } else {
    print_md(x = object, ...)
  }
}


#' @rdname display
#' @export
print_md.data.frame <- function(x, ...) {
  export_table(x, format = "markdown", ...)
}


#' @rdname display
#' @export
print_html.data.frame <- function(x, ...) {
  dots <- list(...)
  if (is.null(dots$backend) || !identical(dots$backend, "tt")) {
    export_table(x, format = "html", ...)
  } else {
    export_table(x, format = "tt", ...)
  }
}


# matrix --------------------------------------------------------------

#' @export
display.matrix <- display.data.frame

#' @export
print_md.matrix <- function(x, ...) {
  # prepare matrix for printing
  x <- .prepare_matrix_print(x, col_names = "")
  # export table now
  export_table(x, format = "markdown", ...)
}

#' @export
print_html.matrix <- function(x, ...) {
  # prepare matrix for printing
  x <- .prepare_matrix_print(x, col_names = "Value")
  # export table now
  print_html.data.frame(x, ...)
}


# array --------------------------------------------------------------

#' @export
display.array <- display.data.frame

#' @export
print_md.array <- print_md.matrix

#' @export
print_html.array <- print_html.matrix


# table --------------------------------------------------------------

#' @export
display.table <- display.data.frame

#' @export
print_md.table <- function(x, ...) {
  # prepare matrix for printing
  x <- .prepare_matrix_print(x, col_names = "")
  # export table now
  export_table(x, format = "markdown", ...)
}

#' @export
print_html.table <- function(x, ...) {
  # prepare matrix for printing
  x <- .prepare_matrix_print(x, col_names = "Value")
  # export table now
  print_html.data.frame(x, ...)
}


# utils --------------------------------------------------------------

.prepare_matrix_print <- function(x, col_names = "Value") {
  # to data frame
  if (inherits(x, "table")) {
    x <- as.data.frame.matrix(x)
  } else {
    x <- as.data.frame(x)
  }
  # add row names
  x <- cbind(Row = rownames(x), x)
  # some cleanup
  rownames(x) <- NULL
  colnames(x)[1] <- col_names
  x
}
