if (requiet("testthat") && requiet("insight")) {
  x <- c(0.0000453, 0.12, 1.2, 0.0001234)
  test_that("format_value", {
    f <- format_value(x, zap_small = FALSE)
    expect_equal(f, c("4.53e-05", "0.12", "1.20", "1.23e-04"))
    f <- format_value(x, zap_small = TRUE)
    expect_equal(f, c("0.00", "0.12", "1.20", "0.00"))
  })
}
