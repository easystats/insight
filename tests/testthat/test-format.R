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
    expect_equal(format_value(0.0000000123, digits = 8), "0.00000001")
    expect_equal(format_value(0.95, as_percent = TRUE), "95.00%")
    expect_equal(format_value(0.000001, as_percent = TRUE), "1.00e-04%")
    expect_equal(format_value(0.00001, as_percent = TRUE), "0.00%")
  })

  test_that("format_value", {
    expect_equal(format_value(0.0045), "0.00")
    expect_equal(format_value(0.00045), "4.50e-04")
    expect_equal(format_value(0.00045, digits = 3), "4.500e-04")
    expect_equal(format_value(0.00045, digits = 4), "0.0004")
  })
}
