#' Percentage in ROPE formatting
#'
#' @param rope_percentage Value or vector of percentages in ROPE.
#' @inheritParams format_p
#' @inheritParams format_ci
#'
#' @return A formatted string.
#'
#' @examples
#' format_rope(c(0.02, 0.12, 0.357, 0))
#' format_rope(c(0.02, 0.12, 0.357, 0), name = NULL)
#' @export
format_rope <- function(rope_percentage, name = "in ROPE", digits = 2) {
  text <- ifelse(rope_percentage == 0, "0%",
    ifelse(rope_percentage == 1, "100%",
      format_value(rope_percentage, digits = digits, as_percent = TRUE)
    )
  )

  if (!is.null(name)) {
    text <- paste(text, name)
  }

  text
}
