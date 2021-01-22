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
    expect_true(is.character(insight::format_pd(0.02)))
    expect_equal(nchar(format_bf(4)), 9)
    expect_true(is.character(format_rope(0.02)))
  })

  test_that("format_number", {
    expect_equal(format_number(2), "two")
    expect_equal(format_number(45), "forty five")
    expect_equal(format_number(2), "two")
  })

  test_that("format_p", {
    expect_equal(nchar(format_p(0.02)), 9)
    expect_equal(nchar(format_p(0.02, stars = TRUE)), 10)
    expect_equal(nchar(format_p(0.02, stars_only = TRUE)), 1)
  })

  test_that("format_table, other CI columns", {
    x <- data.frame(test_CI = .9, test_CI_low = .1, test_CI_high = 1.3)
    test <- utils::capture.output(format_table(x))
    expect_equal(test, c("   test 90% CI", "1 [0.10, 1.30]"))

    x <- data.frame(CI = .8, CI_low = 2.43, CI_high = 5.453,
                    test_CI = .9, test_CI_low = .1, test_CI_high = 1.3)
    test <- utils::capture.output(format_table(x))
    expect_equal(test, c("        80% CI  test 90% CI", "1 [2.43, 5.45] [0.10, 1.30]"))

    x <- data.frame(CI_low = 2.43, CI_high = 5.453, test_CI_low = .1, test_CI_high = 1.3)
    attr(x, "ci") <- .8
    attr(x, "ci_test") <- .9
    test <- utils::capture.output(format_table(x))
    expect_equal(test, c("        80% CI  test 90% CI", "1 [2.43, 5.45] [0.10, 1.30]"))
  })
}
