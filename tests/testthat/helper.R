skip_if_not_or_load_if_installed <- function(package) {
  testthat::skip_if_not_installed(package)
  suppressMessages(suppressWarnings(suppressPackageStartupMessages(
    require(package, warn.conflicts = FALSE, character.only = TRUE, quietly = TRUE)
  )))
}
