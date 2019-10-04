#' @export
print.easystats_check <- function(x, ...) {
  # check attributes
  title <- attr(x, "title")
  text <- attr(x, "text")
  color <- attr(x, "color")

  # no attributes found? check list elements then...
  if (is.null(title) && is.null(text) && is.null(color)) {
    if ("title" %in% names(x)) title <- x$title
    if ("text" %in% names(x)) text <- x$text
    if ("color" %in% names(x)) color <- x$color
  }

  if (!is.null(title)) {
    print_color(paste0("# ", title, "\n\n"), "blue")
  }

  print_color(text, color)
}
