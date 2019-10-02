#' @export
print.easystats_check <- function(x, ...) {
  if (!is.null(x$title)) {
    insight::print_color(x$title, "blue")
  }
  insight::print_color(x$text, x$color)
}
