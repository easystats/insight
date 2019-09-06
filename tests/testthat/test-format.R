if (require("testthat") && require("insight")) {
  context("insight, format")

  test_that("format_value", {
    expect_equal(nchar(format_value(1.2012313)), 4)
    expect_equal(format_value(4.2, protect_integers = TRUE), "4.20")
    expect_equal(format_value(4.0, protect_integers = TRUE), "4")
  })
}
