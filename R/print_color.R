#' @title Coloured console output
#' @name print_color
#'
#' @description Convenient function that allows coloured output in the console.
#'  Mainly implemented to reduce package dependencies.
#'
#' @param text The text to print.
#' @param color,colour Character vector, indicating the colour for printing.
#'   May be one of `"red"`, `"yellow"`, `"green"`, `"blue"`,
#'   `"violet"`, `"cyan"` or `"grey"`. Formatting is also possible
#'   with `"bold"` or `"italic"`.
#'
#' @details This function prints `text` directly to the console using
#'   `cat()`, so no string is returned.
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
