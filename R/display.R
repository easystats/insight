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
#' @param format String, indicating the output format. Can be `"markdown"` or `"html"`.
#' @param ... Arguments passed to other methods.
#'
#' @return Depending on `format`, either an object of class `gt_tbl`
#'   or a character vector of class `knitr_kable`.
#'
#' @examples
#' display(iris[1:5, ])
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
  if (identical(format, "html")) {
    print_html(x = object, ...)
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
  export_table(x, format = "html", ...)
}
