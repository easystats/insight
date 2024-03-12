skip_if_not_installed("tinytable", minimum_version = "0.1.0")

test_that("apply_table_theme", {
  x <- mtcars[1:4, 1:3]
  out <- tinytable::tt(x)
  out@output <- "html"
  expect_s4_class(apply_table_theme(out, x, theme = "bootstrap"), "tinytable")
  expect_s4_class(apply_table_theme(out, x, theme = "darklines"), "tinytable")
  expect_s4_class(apply_table_theme(out, x, theme = NULL), "tinytable")
})
