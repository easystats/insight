if (require("testthat") && require("insight")) {
  test_that("format_value", {
    expect_equal(nchar(format_value(1.2012313)), 4)
    expect_equal(format_value(4.2, protect_integers = TRUE), "4.20")
    expect_equal(format_value(4.0, protect_integers = TRUE), "4")
    expect_equal(format_value(0, protect_integers = TRUE), "0")
    expect_equal(format_value(0), "0.00")
    expect_equal(format_value(1234565789101112), "1.23e+15")
    expect_equal(format_value(1234565789101112, protect_integers = TRUE), "1234565789101112")
    expect_equal(format_value(0.0000000123), "1.23e-08")
    expect_equal(format_value(0.0000000123, zap_small = TRUE), "0.00")
    expect_equal(format_value(0.0000000123, digits = 8), "0.00000001")
    expect_equal(format_value(0.95, as_percent = TRUE), "95.00%")
    expect_equal(format_value(0.000001, as_percent = TRUE), "1.00e-04%")
    expect_equal(format_value(0.000001, as_percent = TRUE, zap_small = TRUE), "0.00%")
  })

  test_that("format_value", {
    expect_equal(format_value(0.0045, zap_small = TRUE), "0.00")
    expect_equal(format_value(0.0045), "4.50e-03")
    expect_equal(format_value(0.00045), "4.50e-04")
    expect_equal(format_value(0.00045, digits = 3), "4.500e-04")
    expect_equal(format_value(0.00045, digits = 4), "0.0004")
  })

  test_that("format_ci", {
    expect_equal(
      format_ci(c(123, 123, 123, 123), c(123, 12345, 123456, 123456789012), width = "auto"),
      c("95% CI [123.00,   123.00]", "95% CI [123.00, 12345.00]", "95% CI [123.00, 1.23e+05]", "95% CI [123.00, 1.23e+11]")
    )
    expect_equal(
      format_ci(c(123, 123, 123, 123), c(123, 12345, 123456, 123456789012), width = "auto", digits = 5),
      c(
        "95% CI [123.00000,   123.00000]", "95% CI [123.00000, 12345.00000]",
        "95% CI [123.00000, 1.23456e+05]", "95% CI [123.00000, 1.23457e+11]"
      )
    )
    expect_equal(
      format_ci(c(123, 123, 123, 123), c(123, 12345, 123456, 123456789012), width = "auto", digits = 0),
      c("95% CI [123,    123]", "95% CI [123,  12345]", "95% CI [123,  1e+05]", "95% CI [123,  1e+11]")
    )
  })

  test_that("format others", {
    testthat::expect_true(is.character(insight::format_pd(0.02)))
    testthat::expect_equal(nchar(format_bf(4)), 9)
    testthat::expect_true(is.character(format_rope(0.02)))
  })

  test_that("format_number", {
    testthat::expect_equal(format_number(2), "two")
    testthat::expect_equal(format_number(45), "forty five")
    testthat::expect_equal(format_number(2), "two")
  })

  test_that("format_p", {
    testthat::expect_equal(nchar(format_p(0.02)), 9)
    testthat::expect_equal(nchar(format_p(0.02, stars = TRUE)), 10)
    testthat::expect_equal(nchar(format_p(0.02, stars_only = TRUE)), 1)
  })
}
