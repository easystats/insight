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


# regular colors -------------------------------------

.black <- function(x, bg = FALSE) {
  if (.supports_color()) {
    style <- ifelse(isTRUE(bg), "4", "3")
    x[!is.na(x)] <- paste0("\033[", style, "0m", x[!is.na(x)], "\033[", style, "9m")
  }
  x
}

.blue <- function(x, bg = FALSE) {
  if (.supports_color()) {
    style <- ifelse(isTRUE(bg), "4", "3")
    x[!is.na(x)] <- paste0("\033[", style, "4m", x[!is.na(x)], "\033[", style, "9m")
  }
  x
}

.cyan <- function(x, bg = FALSE) {
  if (.supports_color()) {
    style <- ifelse(isTRUE(bg), "4", "3")
    x[!is.na(x)] <- paste0("\033[", style, "6m", x[!is.na(x)], "\033[", style, "9m")
  }
  x
}

.green <- function(x, bg = FALSE) {
  if (.supports_color()) {
    style <- ifelse(isTRUE(bg), "4", "3")
    x[!is.na(x)] <- paste0("\033[", style, "2m", x[!is.na(x)], "\033[", style, "9m")
  }
  x
}

.grey <- function(x) {
  if (.supports_color()) {
    x[!is.na(x)] <- paste0("\033[90m", x[!is.na(x)], "\033[39m")
  }
  x
}

.red <- function(x, bg = FALSE) {
  if (.supports_color()) {
    style <- ifelse(isTRUE(bg), "4", "3")
    x[!is.na(x)] <- paste0("\033[", style, "1m", x[!is.na(x)], "\033[", style, "9m")
  }
  x
}

.violet <- function(x, bg = FALSE) {
  if (.supports_color()) {
    style <- ifelse(isTRUE(bg), "4", "3")
    x[!is.na(x)] <- paste0("\033[", style, "5m", x[!is.na(x)], "\033[", style, "9m")
  }
  x
}

.white <- function(x, bg = FALSE) {
  if (.supports_color()) {
    style <- ifelse(isTRUE(bg), "4", "3")
    x[!is.na(x)] <- paste0("\033[", style, "7m", x[!is.na(x)], "\033[", style, "9m")
  }
  x
}

.yellow <- function(x, bg = FALSE) {
  if (.supports_color()) {
    style <- ifelse(isTRUE(bg), "4", "3")
    x[!is.na(x)] <- paste0("\033[", style, "3m", x[!is.na(x)], "\033[", style, "9m")
  }
  x
}

# font styles ---------------------------

.bold <- function(x) {
  if (.supports_color()) {
    x[!is.na(x)] <- paste0("\033[1m", x[!is.na(x)], "\033[22m")
  }
  x
}

.italic <- function(x) {
  if (.supports_color()) {
    x[!is.na(x)] <- paste0("\033[3m", x[!is.na(x)], "\033[23m")
  }
  x
}

# bright colors ---------------------------------

.bright_blue <- function(x) {
  if (.supports_color()) {
    x[!is.na(x)] <- paste0("\033[94m", x[!is.na(x)], "\033[39m")
  }
  x
}

.bright_cyan <- function(x) {
  if (.supports_color()) {
    x[!is.na(x)] <- paste0("\033[96m", x[!is.na(x)], "\033[39m")
  }
  x
}

.bright_green <- function(x) {
  if (.supports_color()) {
    x[!is.na(x)] <- paste0("\033[92m", x[!is.na(x)], "\033[39m")
  }
  x
}

.bright_red <- function(x) {
  if (.supports_color()) {
    x[!is.na(x)] <- paste0("\033[91m", x[!is.na(x)], "\033[39m")
  }
  x
}

.bright_violet <- function(x) {
  if (.supports_color()) {
    x[!is.na(x)] <- paste0("\033[95m", x[!is.na(x)], "\033[39m")
  }
  x
}

.bright_white <- function(x) {
  if (.supports_color()) {
    x[!is.na(x)] <- paste0("\033[97m", x[!is.na(x)], "\033[39m")
  }
  x
}

.bright_yellow <- function(x) {
  if (.supports_color()) {
    x[!is.na(x)] <- paste0("\033[93m", x[!is.na(x)], "\033[39m")
  }
  x
}


# tools -----------------------------------

.colour <- function(colour = "red", x) {
  # replace "bright" suffixes to a generic color code
  if (grepl("^(bright_|br_)", colour)) {
    colour <- gsub("^(bright_|br_)", "b", colour)
  }

  switch(colour,
    black = .black(x),
    red = .red(x),
    green = .green(x),
    yellow = .yellow(x),
    blue = .blue(x),
    violet = .violet(x),
    cyan = .cyan(x),
    white = .white(x),
    grey = .grey(x),
    bred = .bright_red(x),
    bgreen = .bright_green(x),
    byellow = .bright_yellow(x),
    bblue = .bright_blue(x),
    bviolet = .bright_violet(x),
    bcyan = .bright_cyan(x),
    bwhite = .bright_white(x),
    bg_red = .red(x, bg = TRUE),
    bg_green = .green(x, bg = TRUE),
    bg_yellow = .yellow(x, bg = TRUE),
    bg_blue = .blue(x, bg = TRUE),
    bg_violet = .violet(x, bg = TRUE),
    bg_cyan = .cyan(x, bg = TRUE),
    bg_white = .white(x, bg = TRUE),
    bg_black = .black(x, bg = TRUE),
    bold = .bold(x),
    italic = .italic(x),
    format_warning(paste0("`color` ", colour, " not yet supported."))
  )
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
