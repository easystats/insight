#' @title Generic export of data frames into formatted tables
#' @name display
#'
#' @description \code{display()} is a generic function to export data frames
#' into various table formats (like plain text, markdown, ...). \code{print_md()}
#' usually is a convenient wrapper for \code{display(format = "markdown")}.
#' Similar, \code{print_html()} is a shortcut for \code{display(format = "html")}.
#' See the documentation for the specific objects' classes.
#'
#' @param object,x A data frame.
#' @param ... Arguments passed to other methods.
#'
#' @examples
#' display(iris)
#'
#'
#' @return A data frame.
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
display.data.frame <- function(object,
                               ...) {
  if (identical(format, "html")) {
    print_html(x = object, ...)
  } else {
    print_md(x = object, ...)
  }
}



#' @rdname display
#' @export
print_md.data.frame <- function(x,
                                ...) {

  export_table(x, format = "markdown", ...)
}




#' @rdname display
#' @export
print_md.data.frame <- function(x,
                                ...) {

  export_table(x, format = "html", ...)
}