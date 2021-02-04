#' Bayes Factor formatting
#'
#' @param bf Bayes Factor.
#' @param protect_ratio Should values smaller than 1 be represented as ratios?
#' @param na_reference How to format missing values (`NA`).
#' @param exact Should very large or very small values be reported with a
#'   scientific format (e.g., 4.24e5), or as truncated values (as "> 1000" and
#'   "< 1/1000").
#' @inheritParams format_p
#'
#' @return A formatted string.
#'
#' @examples
#' format_bf(bfs <- c(0.000045, 0.033, NA, 1557, 3.54))
#' format_bf(bfs, exact = TRUE, name = NULL)
#' format_bf(bfs, stars = TRUE)
#' format_bf(bfs, protect_ratio = TRUE)
#' format_bf(bfs, protect_ratio = TRUE, exact = TRUE)
#' format_bf(bfs, na_reference = 1)
#' @export
format_bf <- function(bf, stars = FALSE, stars_only = FALSE, name = "BF",
                      protect_ratio = FALSE, na_reference = NA, exact = FALSE) {
  if (!is.na(na_reference)) {
    bf[is.na(bf)] <- na_reference
  } else {
    bf[bad_bf <- is.na(bf)] <- 1
  }

  bf_orig <- bf

  if (protect_ratio) {
    is_small <- bf < 1
    bf[is_small] <- 1 / bf[is_small]
  } else {
    is_small <- logical(length(bf))
  }

  digits <- ifelse(is.na(bf), 0, ifelse(bf < 1, 3, 2))

  text <- paste0(
    "= ",
    ifelse(is_small, "1/", ""),
    format_value(bf, digits = digits)
  )

  ## Very big/small values
  is_extreme <- bf_orig > 1000 | bf_orig < 1 / 1000
  if (any(is_extreme)) {
    if (exact) {
      text[is_extreme] <- ifelse(bf_orig[is_extreme] > 1000,
        sprintf("= %.2e", bf_orig[is_extreme]),
        text[is_extreme]
      )
      text[is_extreme] <- ifelse(bf_orig[is_extreme] < 1 / 1000,
        ifelse(is_small[is_extreme],
          sprintf("= 1/%.2e", bf[is_extreme]),
          sprintf("= %.2e", bf_orig[is_extreme])
        ),
        text[is_extreme]
      )
    } else {
      text[is_extreme] <- ifelse(bf_orig[is_extreme] > 1000,
        "> 1000",
        text[is_extreme]
      )
      text[is_extreme] <- ifelse(bf_orig[is_extreme] < 1 / 1000,
        ifelse(is_small[is_extreme], "< 1/1000", "< 0.001"),
        text[is_extreme]
      )
    }
  }

  ## Add stars
  text <- ifelse(bf_orig > 30, paste0(text, "***"),
    ifelse(bf_orig > 10, paste0(text, "**"),
      ifelse(bf_orig > 3, paste0(text, "*"), text)
    )
  )

  out <- .add_prefix_and_remove_stars(text, stars, stars_only, name)
  if (is.na(na_reference)) out[bad_bf] <- ""
  out
}
