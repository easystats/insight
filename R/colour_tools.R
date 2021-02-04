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



.blue <- function(x) {
  if (.supports_color()) {
    x[!is.na(x)] <- paste0("\033[34m", x[!is.na(x)], "\033[39m")
  }
  x
}

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

.red <- function(x) {
  if (.supports_color()) {
    x[!is.na(x)] <- paste0("\033[31m", x[!is.na(x)], "\033[39m")
  }
  x
}

.green <- function(x) {
  if (.supports_color()) {
    x[!is.na(x)] <- paste0("\033[32m", x[!is.na(x)], "\033[39m")
  }
  x
}

.yellow <- function(x) {
  if (.supports_color()) {
    x[!is.na(x)] <- paste0("\033[33m", x[!is.na(x)], "\033[39m")
  }
  x
}

.violet <- function(x) {
  if (.supports_color()) {
    x[!is.na(x)] <- paste0("\033[35m", x[!is.na(x)], "\033[39m")
  }
  x
}

.cyan <- function(x) {
  if (.supports_color()) {
    x[!is.na(x)] <- paste0("\033[36m", x[!is.na(x)], "\033[39m")
  }
  x
}

.grey <- function(x) {
  if (.supports_color()) {
    x[!is.na(x)] <- paste0("\033[90m", x[!is.na(x)], "\033[39m")
  }
  x
}


.colour <- function(colour = "red", x) {
  switch(colour,
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


.is_valid_colour <- function(colour) {
  colour %in% c("red", "yellow", "green", "blue", "violet", "cyan", "grey", "bold", "italic")
}
