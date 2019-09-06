if (require("testthat") && require("insight")) {
  context("insight, format")

  test_that("format_value", {
    testthat::expect_equal(nchar(format_value(1.2012313)), 4)
    testthat::expect_true(is.int(2))
    testthat::expect_equal(nchar(format_value(4.2, protect_integers = TRUE)), 4)
    testthat::expect_equal(nchar(format_value(4.0, protect_integers = TRUE)), 1)
  })
}
