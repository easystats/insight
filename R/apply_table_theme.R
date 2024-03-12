#' @param out A `tinytable` object.
#' @param x The underlying data frame, used to create `out`.
#' @param theme The theme to apply to the table. One of `"default"`, `"grid"`,
#' `"striped"`, `"bootstrap"`, `"void"`, `"tabular"`, or `"darklines"`.
#' @param sub_header_positions A vector of row positions to apply a border to.
#' Currently particular for internal use of other _easystats_ packages.
#' @rdname export_table
#' @export
apply_table_theme <- function(out, x, theme = "default", sub_header_positions = NULL) {
  # if no theme, do nothing
  if (is.null(theme)) {
    return(out)
  }

  # packages available?
  check_if_installed("tinytable", minimum_version = "0.1.0")

  switch(theme,
    grid = ,
    striped = ,
    tabular = ,
    void = ,
    bootstrap = {
      out <- tinytable::theme_tt(out, theme = theme)
    },
    darklines = {
      # borders for sub headings
      if (!is.null(sub_header_positions) && length(sub_header_positions) > 1) {
        out <- tinytable::style_tt(
          out,
          i = sub_header_positions[2:length(sub_header_positions)],
          line = "b",
          line_color = "#cccccc",
          line_width = 0.05
        )
      }
      # top table border
      out <- tinytable::style_tt(
        out,
        i = -1,
        line = "t",
        line_width = 0.2,
        line_color = "#444444"
      )
      # table border between headers for model names and column headers
      out <- tinytable::style_tt(
        out,
        i = -1,
        j = 2:ncol(x),
        line = "b",
        line_color = "#999999"
      )
      # bottom table border
      out <- tinytable::style_tt(
        out,
        i = nrow(x) + length(sub_header_positions),
        line_width = 0.15,
        line = "b",
        line_color = "#444444"
      )
    },
    # default theme
    {
      # borders for sub headings
      if (!is.null(sub_header_positions) && length(sub_header_positions) > 1) {
        out <- tinytable::style_tt(
          out,
          i = sub_header_positions[2:length(sub_header_positions)],
          line = "b",
          line_color = "#d4d4d4",
          line_width = 0.05
        )
      }
      # top table border
      out <- tinytable::style_tt(
        out,
        i = -1,
        line = "t",
        line_color = "#d4d4d4"
      )
      # table border between headers for model names and column headers
      out <- tinytable::style_tt(
        out,
        i = -1,
        j = 2:ncol(x),
        line = "b",
        line_color = "#d4d4d4"
      )
      # bottom table border
      out <- tinytable::style_tt(
        out,
        i = nrow(x) + length(sub_header_positions),
        line = "b",
        line_color = "#d4d4d4"
      )
    }
  )
  out
}
