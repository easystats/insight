#' @title Global options from the insight package
#' @name insight-options
#'
#' @section Global options to set defaults for function arguments:
#'
#' - `options(easystats_display_format = <value>)` will set the default format
#'   for the `display()` methods. Can be one of `"markdown"`, `"html"`, or
#'   `"tt"`. See [`display()`] for details.
#'
#' - `options(easystats_table_width = <value>)` sets the default width
#'   for tables printed to the console. Tables exceeding this width are
#'   wrapped into multiple parts to ensure clean formatting and avoid
#'   excessively long lines.
#'
#' - `options(easystats_ci_separator = <value>)` defines the separator string
#'   used between the lower and upper CI limits. Defaults to `", "`, resulting
#'   in an output like `[0.3, 0.8]`.
#'
#' - `options(insight_use_symbols = <value>)` accepts `TRUE` or `FALSE`. If
#'   `TRUE`, statistical symbols (e.g., eta-squared, omega) are rendered using
#'   their corresponding Unicode characters.
#'
#' - `options(easystats_errors = <value>)` accepts `TRUE` or `FALSE`. If
#'   `TRUE`, functions that typically suppress errors (via `tryCatch()`)
#'   will instead halt execution and throw an error when an issue occurs.
NULL
