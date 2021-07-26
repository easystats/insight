#' Reshape CI between wide/long formats
#'
#' Reshape CI between wide/long formats.
#'
#' @param x A data frame containing columns named `CI_low` and `CI_high`.
#'
#' @examples
#' x <- data.frame(
#'   Parameter = c("Term 1", "Term 2", "Term 1", "Term 2"),
#'   CI = c(.8, .8, .9, .9),
#'   CI_low = c(.2, .3, .1, .15),
#'   CI_high = c(.5, .6, .8, .85),
#'   stringsAsFactors = FALSE
#' )
#' reshape_ci(x)
#' reshape_ci(reshape_ci(x))
#' @export
reshape_ci <- function(x) {


  # Long to wide ----------------
  if ("CI_low" %in% names(x) & "CI_high" %in% names(x) & "CI" %in% names(x)) {
    ci_position <- which(names(x) == "CI")

    # Reshape
    if (length(unique(x$CI)) > 1) {
      if (!"Parameter" %in% names(x)) {
        x$Parameter <- NA
        remove_parameter <- TRUE
      } else {
        remove_parameter <- FALSE
      }

      x <- stats::reshape(
        x,
        idvar = "Parameter",
        timevar = "CI",
        direction = "wide",
        v.names = c("CI_low", "CI_high"),
        sep = "_"
      )
      row.names(x) <- NULL
      if (remove_parameter) x$Parameter <- NULL
    }

    # Replace at the right place
    ci_colname <- names(x)[c(grepl("CI_low_*", names(x)) | grepl("CI_high_*", names(x)))]
    colnames_1 <- names(x)[0:(ci_position - 1)][!names(x)[0:(ci_position - 1)] %in% ci_colname]
    colnames_2 <- names(x)[!names(x) %in% c(ci_colname, colnames_1)]
    x <- x[c(colnames_1, ci_colname, colnames_2)]


    # Wide to long --------------
  } else {
    if (!"Parameter" %in% names(x)) {
      x$Parameter <- 1:nrow(x)
      remove_parameter <- TRUE
    } else {
      remove_parameter <- FALSE
    }

    lows <- grepl("CI_low_*", names(x))
    highs <- grepl("CI_high_*", names(x))
    ci <- as.numeric(gsub("CI_low_", "", names(x)[lows]))
    if (paste0(ci, collapse = "-") != paste0(gsub("CI_high_", "", names(x)[highs]), collapse = "-")) {
      stop("Something went wrong in the CIs reshaping.")
      return(x)
    }
    if (sum(lows) > 1 & sum(highs) > 1) {
      low <- stats::reshape(
        x[!highs],
        direction = "long",
        varying = list(names(x)[lows]),
        sep = "_",
        timevar = "CI",
        v.names = "CI_low",
        times = ci
      )
      high <- stats::reshape(
        x[!lows],
        direction = "long",
        varying = list(names(x)[highs]),
        sep = "_",
        timevar = "CI",
        v.names = "CI_high",
        times = ci
      )
      x <- merge(low, high)
      x$id <- NULL
      x <- x[order(x$Parameter), ]
      row.names(x) <- NULL
      if (remove_parameter) x$Parameter <- NULL
    }

    # Replace at the right place
    ci_position <- which(lows)[1]
    ci_colname <- c("CI", "CI_low", "CI_high")
    colnames_1 <- names(x)[0:(ci_position - 1)][!names(x)[0:(ci_position - 1)] %in% ci_colname]
    colnames_2 <- names(x)[!names(x) %in% c(ci_colname, colnames_1)]
    x <- x[c(colnames_1, ci_colname, colnames_2)]
  }

  class(x) <- intersect(c("data.frame", "numeric"), class(x))
  x
}
