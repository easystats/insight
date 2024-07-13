#' @export
print.easystats_check <- function(x, ...) {
  # check attributes
  my_title <- attr(x, "title")
  my_text <- attr(x, "text")
  color <- attr(x, "color")

  # no attributes found? check list elements then...
  if (is.null(my_title) && is.null(my_text) && is.null(color)) {
    if ("title" %in% names(x)) my_title <- x$title
    if ("text" %in% names(x)) my_text <- x$text
    if ("color" %in% names(x)) color <- x$color
  }

  if (!is.null(my_title)) {
    print_color(paste0("# ", my_title, "\n\n"), "blue")
  }

  print_color(my_text, color)
  invisible(x)
}
