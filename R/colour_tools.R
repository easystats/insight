.COLOR_CODES <- list(
  # regular colors
  black = c(30, 39),
  red = c(31, 39),
  green = c(32, 39),
  yellow = c(33, 39),
  blue = c(34, 39),
  violet = c(35, 39),
  cyan = c(36, 39),
  white = c(37, 39),
  grey = c(90, 39),
  # bright colors
  bred = c(91, 39),
  bgreen = c(92, 39),
  byellow = c(93, 39),
  bblue = c(94, 39),
  bviolet = c(95, 39),
  bcyan = c(96, 39),
  bwhite = c(97, 39),
  # styles
  bold = c(1, 22),
  italic = c(3, 23),
  # background colors
  bg_black = c(40, 49),
  bg_red = c(41, 49),
  bg_green = c(42, 49),
  bg_yellow = c(43, 49),
  bg_blue = c(44, 49),
  bg_violet = c(45, 49),
  bg_cyan = c(46, 49),
  bg_white = c(47, 49)
)


.apply_color <- function(x, color_code) {
  x[!is.na(x)] <- paste0(
    "\033[", color_code[1], "m", x[!is.na(x)], "\033[", color_code[2], "m"
  )
}


.colour <- function(colour = "red", x) {
  # do nothing if ANSI-colors are not supported
  if (!.supports_color()) {
    return(x)
  }

  # replace "bright" suffixes to a generic color code
  if (grepl("^(bright_|br_)", colour)) {
    colour <- gsub("^(bright_|br_)", "b", colour)
  }

  if (colour %in% names(.COLOR_CODES)) {
    .apply_color(x, .COLOR_CODES[[colour]])
  } else {
    format_warning(paste0("`color` ", colour, " not yet supported."))
  }
}


.is_valid_colour <- function(colour) {
  # replace "bright" suffixes to a generic color code
  if (grepl("^(bright_|br_)", colour)) {
    colour <- gsub("^(bright_|br_)", "b", colour)
  }

  colour %in% c(
    "red", "yellow", "green", "blue", "violet", "cyan", "grey", "bold",
    "italic", "bred", "bgreen", "byellow", "bblue", "bviolet", "bcyan",
    "bwhite", "bg_red", "bg_green", "bg_yellow", "bg_blue", "bg_violet",
    "bg_cyan", "bg_white", "bg_black"
  )
}


.supports_color <- function() {
  enabled <- getOption("crayon.enabled")
  if (!is.null(enabled)) {
    return(isTRUE(enabled))
  }
  if (.rstudio_with_ansi_support() && sink.number() == 0) {
    return(TRUE)
  }
  if (Sys.getenv("POSITRON", "") == "1" && sink.number() == 0) {
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


.rstudio_with_ansi_support <- function() {
  if (Sys.getenv("RSTUDIO", "") == "") {
    return(FALSE)
  }

  cols <- Sys.getenv("RSTUDIO_CONSOLE_COLOR", "")
  if (cols != "" && !is.na(as.numeric(cols))) {
    return(TRUE)
  }

  requireNamespace("rstudioapi", quietly = TRUE) &&
    rstudioapi::isAvailable() &&
    rstudioapi::hasFun("getConsoleHasColor")
}
