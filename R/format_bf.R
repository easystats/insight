#' Bayes Factor formatting
#'
#' @param bf Bayes Factor.
#' @param protect_ratio Should values smaller than 1 be represented as ratios?
#' @param na_reference How to format missing values (`NA`).
#' @param exact Should very large or very small values be reported with a
#'   scientific format (e.g., 4.24e5), or as truncated values (as "> 1000" and
#'   "< 1/1000").
#' @param inferiority_star String, indicating the symbol that is used to
#'   indicate inferiority, i.e. when the Bayes Factor is smaller than one third
#'   (the thresholds are smaller than one third, 1/10 and 1/30).
#' @inheritParams format_p
#'
#' @return A formatted string.
#'
#' @examples
#' bfs <- c(0.000045, 0.033, NA, 1557, 3.54)
#' format_bf(bfs)
#' format_bf(bfs, exact = TRUE, name = NULL)
#' format_bf(bfs, stars = TRUE)
#' format_bf(bfs, protect_ratio = TRUE)
#' format_bf(bfs, protect_ratio = TRUE, exact = TRUE)
#' format_bf(bfs, na_reference = 1)
#' @export
format_bf <- function(bf,
                      stars = FALSE,
                      stars_only = FALSE,
                      inferiority_star = "\u00B0",
                      name = "BF",
                      protect_ratio = FALSE,
                      na_reference = NA,
                      exact = FALSE) {
  if (is.na(na_reference)) {
    bf[bad_bf <- is.na(bf)] <- 1
  } else {
    bf[is.na(bf)] <- na_reference
  }

  bf_orig <- bf

  if (protect_ratio) {
    is_small <- bf < 1
    bf[is_small] <- 1 / bf[is_small]
  } else {
    is_small <- logical(length(bf))
  }

  digits <- ifelse(is.na(bf), 0, ifelse(bf < 1, 3, 2)) # nolint

  bf_text <- paste0(
    "= ",
    ifelse(is_small, "1/", ""),
    format_value(bf, digits = digits)
  )

  ## Very big/small values
  is_extreme <- bf_orig > 1000 | bf_orig < 1 / 1000
  if (any(is_extreme)) {
    if (exact) {
      bf_text[is_extreme] <- ifelse(bf_orig[is_extreme] > 1000,
        sprintf("= %.2e", bf_orig[is_extreme]),
        bf_text[is_extreme]
      )
      bf_text[is_extreme] <- ifelse(bf_orig[is_extreme] < 1 / 1000,
        ifelse(is_small[is_extreme], # nolint
          sprintf("= 1/%.2e", bf[is_extreme]),
          sprintf("= %.2e", bf_orig[is_extreme])
        ),
        bf_text[is_extreme]
      )
    } else {
      bf_text[is_extreme] <- ifelse(bf_orig[is_extreme] > 1000,
        "> 1000",
        bf_text[is_extreme]
      )
      bf_text[is_extreme] <- ifelse(bf_orig[is_extreme] < 1 / 1000,
        ifelse(is_small[is_extreme], "< 1/1000", "< 0.001"), # nolint
        bf_text[is_extreme]
      )
    }
  }

  ## Add stars
  bf_text <- ifelse(bf_orig > 30, paste0(bf_text, "***"),
    ifelse(bf_orig > 10, paste0(bf_text, "**"), # nolint
      ifelse(bf_orig > 3, paste0(bf_text, "*"), bf_text) # nolint
    )
  )

  ## Add inferiority stars
  if (!is.null(inferiority_star)) {
    bf_text <- ifelse(bf_orig < (1 / 30), paste0(bf_text, paste(rep_len(inferiority_star, 3), collapse = "")), # nolint
      ifelse(bf_orig < 0.1, paste0(bf_text, paste(rep_len(inferiority_star, 2), collapse = "")), # nolint
        ifelse(bf_orig < (1 / 3), paste0(bf_text, inferiority_star), bf_text) # nolint
      )
    )
  }

  out <- .add_prefix_and_remove_stars(
    p_text = bf_text,
    stars = stars,
    stars_only = stars_only,
    name = name,
    inferiority_star = inferiority_star
  )
  if (is.na(na_reference)) out[bad_bf] <- ""
  out
}
