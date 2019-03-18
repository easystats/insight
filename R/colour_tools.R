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
#'
#' @export
print_color <- function(text, color) {
  cat(.colour(colour = color, x = text))
}

#' @rdname print_color
#' @export
print_colour <- function(text, colour) {
  print_color(color = colour, text = text)
}


#' @keywords internal
.rstudio_with_ansi_support <- function() {
  if (Sys.getenv("RSTUDIO", "") == "") {
    return(FALSE)
  }
  if ((cols <- Sys.getenv("RSTUDIO_CONSOLE_COLOR", "")) !=
      "" && !is.na(as.numeric(cols))) {
    return(TRUE)
  }
  requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable() &&
    rstudioapi::hasFun("getConsoleHasColor")
}


#' @keywords internal
.supports_color <- function() {
  enabled <- getOption("crayon.enabled")
  if (!is.null(enabled)) {
    return(isTRUE(enabled))
  }
  if (.rstudio_with_ansi_support() && sink.number() == 0) {
    return(TRUE)
  }
  if (!isatty(stdout())) {
    return(FALSE)
  }
  if (Sys.info()["sysname"] == "windows") {
    if (Sys.getenv("ConEmuANSI") == "ON") {
      return(TRUE)
    }
    if (Sys.getenv("CMDER_ROOT") != "") {
      return(TRUE)
    }
    return(FALSE)
  }
  if ("COLORTERM" %in% names(Sys.getenv())) {
    return(TRUE)
  }
  if (Sys.getenv("TERM") == "dumb") {
    return(FALSE)
  }
  grepl("^screen|^xterm|^vt100|color|ansi|cygwin|linux", Sys.getenv("TERM"),
        ignore.case = TRUE, perl = TRUE
  )
}



#' @keywords internal
.blue <- function(x) {
  if (.supports_color()) {
    x[!is.na(x)] <- paste0("\033[34m", x[!is.na(x)], "\033[39m")
  }
  x
}

#' @keywords internal
.bold <- function(x) {
  if (.supports_color()) {
    x[!is.na(x)] <- paste0("\033[1m", x[!is.na(x)], "\033[22m")
  }
  x
}

#' @keywords internal
.italic <- function(x) {
  if (.supports_color()) {
    x[!is.na(x)] <- paste0("\033[3m", x[!is.na(x)], "\033[23m")
  }
  x
}

#' @keywords internal
.red <- function(x) {
  if (.supports_color()) {
    x[!is.na(x)] <- paste0("\033[31m", x[!is.na(x)], "\033[39m")
  }
  x
}

#' @keywords internal
.green <- function(x) {
  if (.supports_color()) {
    x[!is.na(x)] <- paste0("\033[32m", x[!is.na(x)], "\033[39m")
  }
  x
}

#' @keywords internal
.yellow <- function(x) {
  if (.supports_color()) {
    x[!is.na(x)] <- paste0("\033[33m", x[!is.na(x)], "\033[39m")
  }
  x
}

#' @keywords internal
.violet <- function(x) {
  if (.supports_color()) {
    x[!is.na(x)] <- paste0("\033[35m", x[!is.na(x)], "\033[39m")
  }
  x
}

#' @keywords internal
.cyan <- function(x) {
  if (.supports_color()) {
    x[!is.na(x)] <- paste0("\033[36m", x[!is.na(x)], "\033[39m")
  }
  x
}


#' @keywords internal
.grey <- function(x) {
  if (.supports_color()) {
    x[!is.na(x)] <- paste0("\033[90m", x[!is.na(x)], "\033[39m")
  }
  x
}


#' @keywords internal
.colour <- function(colour = "red", x) {
  switch(
    colour,
    red = .red(x),
    yellow = .yellow(x),
    green = .green(x),
    blue = .blue(x),
    violet = .violet(x),
    cyan = .cyan(x),
    grey = .grey(x),
    bold = .bold(x),
    italic = .italic(x),
    warning(paste0("`color` ", colour, " not yet supported."))
  )
}
