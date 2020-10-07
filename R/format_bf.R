#' Bayes Factor formatting
#'
#' @param bf Bayes Factor.
#' @param protect_ratio Should values smaller than 1 be represented as ratios?
#' @param na_refrence How to format missing values (\code{NA}). By default, \code{NA} values are formatted as \code{"BF = 1"}.
#' @inheritParams format_p
#'
#' @return A formatted string.
#'
#' @examples
#' format_bf(1.20)
#' format_bf(c(1.20, 1557, 3.5, 12), stars = TRUE)
#' format_bf(c(1.20, 1557, 3.5, 12), name = NULL)
#' format_bf(c(NA, 3.45, 345))
#' format_bf(c(NA, 3.45, 345), na_reference = 1)
#' @export
format_bf <- function(bf, stars = FALSE, stars_only = FALSE, name = "BF", protect_ratio = FALSE, na_reference = NA) {
  if (!is.na(na_reference)) {
    bf[is.na(bf)] <- na_reference
  }

  bf_orig <- bf

  if (protect_ratio) {
    is_small <- bf < 1
    bf[is_small] <- 1 / bf[is_small]
  } else {
    is_small <- logical(length(bf))
  }

  digits <- ifelse(is.na(bf), 0, ifelse(bf < 1, 3, 2))

  ## Very big/small values
  text <-
    ifelse(bf_orig > 1000, "> 1000",
      ifelse(bf_orig < 1 / 1000, ifelse(is_small, "< 1/1000", "< 0.001"),
        paste0(
          "= ",
          ifelse(is_small, "1/", ""),
          format_value(bf, digits = digits)
        )
      )
    )

  ## Add stars
  text <- ifelse(bf_orig > 30, paste0(text, "***"),
    ifelse(bf_orig > 10, paste0(text, "**"),
      ifelse(bf_orig > 3, paste0(text, "*"), text)
    )
  )

  .add_prefix_and_remove_stars(text, stars, stars_only, name)
}
