#' @title Coloured console output
#' @name print_color
#'
#' @description Convenient function that allows coloured output in the console.
#'  Mainly implemented to reduce package dependencies.
#'
#' @param text The text to print.
#' @param color,colour Character vector, indicating the colour for printing.
#'   May be one of \code{"red"}, \code{"yellow"}, \code{"green"}, \code{"blue"},
#'   \code{"violet"}, \code{"cyan"} or \code{"grey"}. Formatting is also possible
#'   with \code{"bold"} or \code{"italic"}.
#'
#' @details This function prints \code{text} directly to the console using
#'   \code{cat()}, so no string is returned.
#'
#' @return Nothing.
#'
#' @examples
#' print_color("I'm blue dabedi dabedei", "blue")
#' @export
print_color <- function(text, color) {
  cat(.colour(colour = color, x = text))
}

#' @rdname print_color
#' @export
print_colour <- function(text, colour) {
  print_color(color = colour, text = text)
}
