test_that("format_value", {
  expect_identical(nchar(format_value(1.2012313)), 4L)
  expect_identical(format_value(4.2, protect_integers = TRUE), "4.20")
  expect_identical(format_value(4.0, protect_integers = TRUE), "4")
  expect_identical(format_value(0, protect_integers = TRUE), "0")
  expect_identical(format_value(0), "0.00")
  expect_identical(format_value(0, decimal_point = ","), "0,00")
  expect_identical(format_value(1234565789101112), "1.23e+15")
  expect_identical(format_value(1234565789101112, protect_integers = TRUE), "1234565789101112")
  expect_identical(format_value(0.0000000123), "1.23e-08")
  expect_identical(format_value(0.0000000123, zap_small = TRUE), "0.00")
  expect_identical(format_value(0.0000000123, digits = 8), "0.00000001")
  expect_identical(format_value(c(0.012, 0.45, -0.03), lead_zero = FALSE), c(".01", ".45", "-.03"))
  expect_identical(format_value(c(1.012, 0.45, -0.03), lead_zero = FALSE), c("1.01", ".45", "-.03"))
  expect_identical(format_value(c(1.012, 0.45, -0.03), lead_zero = FALSE, decimal_point = ","), c("1,01", ",45", "-,03"))
  expect_identical(format_value(c(0.45, -0.03), style_positive = "plus"), c("+0.45", "-0.03"))
  expect_identical(format_value(c(0.45, -0.03), style_positive = "plus", lead_zero = FALSE), c("+.45", "-.03"))
  expect_equal(
    format_value(as.factor(c("A", "B", "A"))),
    structure(c(1L, 2L, 1L), levels = c("A", "B"), class = "factor"),
    ignore_attr = TRUE
  )
  expect_identical(
    format_value(c(0.45, -0.03), style_positive = "plus", style_negative = "parens", lead_zero = FALSE),
    c("+.45", "(.03)")
  )
  expect_identical(
    format_value(c(0.45, -0.03), style_positive = "plus", style_negative = "parens"),
    c("+0.45", "(0.03)")
  )
  expect_identical(format_value(0.95, as_percent = TRUE), "95.00%")
  expect_identical(format_value(0.000001, as_percent = TRUE), "1.00e-04%")
  expect_identical(format_value(0.000001, as_percent = TRUE, zap_small = TRUE), "0.00%")
})

test_that("format_value", {
  expect_identical(format_value(0.0045, zap_small = TRUE), "0.00")
  expect_identical(format_value(0.0045), "4.50e-03")
  expect_identical(format_value(0.00045), "4.50e-04")
  expect_identical(format_value(0.00045, digits = 3), "4.500e-04")
  expect_identical(format_value(0.00045, digits = 4), "0.0004")
})

test_that("format_ci", {
  expect_identical(
    format_ci(c(123, 123, 123, 123), c(123, 12345, 123456, 123456789012), width = "auto"),
    c("95% CI [123.00,   123.00]", "95% CI [123.00, 12345.00]", "95% CI [123.00, 1.23e+05]", "95% CI [123.00, 1.23e+11]")
  )
  expect_identical(
    format_ci(c(123, 123, 123, 123), c(123, 12345, 123456, 123456789012), width = "auto", digits = 5),
    c(
      "95% CI [123.00000,   123.00000]", "95% CI [123.00000, 12345.00000]",
      "95% CI [123.00000, 1.23456e+05]", "95% CI [123.00000, 1.23457e+11]"
    )
  )
  expect_identical(
    format_ci(c(123, 123, 123, 123), c(123, 12345, 123456, 123456789012), width = "auto", digits = 0),
    c("95% CI [123,    123]", "95% CI [123,  12345]", "95% CI [123,  1e+05]", "95% CI [123,  1e+11]")
  )
  expect_identical(format_ci(1.24, 0.0000054), "95% CI [1.24, 5.40e-06]")
  expect_identical(format_ci(1.24, 0.0000054, digits = 0), "95% CI [1, 5e-06]")
  expect_identical(format_ci(1.24, 0.0000054, zap_small = TRUE), "95% CI [1.24, 0.00]")
  expect_identical(format_ci(1.24, 0.0000054, zap_small = TRUE, digits = 0), "95% CI [1, 0]")
})

test_that("format_ci, bayestestR", {
  skip_if_not_installed("bayestestR")
  set.seed(123)
  x <- rnorm(1000)
  expect_identical(format_ci(bayestestR::ci(x)), "95% ETI [-1.94, 2.04]")
  expect_identical(format_ci(bayestestR::ci(x, method = "HDI")), "95% HDI [-1.97, 1.98]")
  expect_identical(format_ci(bayestestR::ci(x), ci_string = "CI"), "95% CI [-1.94, 2.04]")
})

test_that("format_ci, parameters", {
  skip_if_not_installed("parameters")
  mm <- lm(Sepal.Length ~ Petal.Width + Species, data = iris)
  expect_identical(
    format_ci(parameters::ci(mm)),
    c(
      "95% CI [4.62, 4.94]", "95% CI [0.53, 1.30]", "95% CI [-0.52, 0.40]",
      "95% CI [-0.76, 0.66]"
    )
  )
})

test_that("format_ci, factor", {
  data(iris)
  expect_identical(
    format_ci(iris$Species[1], iris$Species[100]),
    "95% CI [setosa, versicolor]"
  )
})

test_that("format_ci, character", {
  expect_identical(
    format_ci("a", "c"),
    "95% CI [a, c]"
  )
})

test_that("format others", {
  expect_type(insight::format_pd(0.02), "character")
  expect_identical(nchar(format_bf(4)), 9L)
  expect_identical(
    format_bf(c(0.000045, 0.033, NA, 1557, 3.54)),
    c("BF < 0.001", "BF = 0.033", "", "BF > 1000", "BF = 3.54")
  )
  expect_identical(
    format_bf(c(0.000045, 0.033, NA, 1557, 3.54), name = NULL),
    c("< 0.001", "0.033", "", "> 1000", "3.54")
  )
  expect_identical(
    format_bf(c(0.000045, 0.233, NA, 1557, 3.54), stars = TRUE),
    c("BF < 0.001°°°", "BF = 0.233°", "", "BF > 1000***", "BF = 3.54*")
  )
  expect_identical(
    format_bf(c(0.000045, 0.233, NA, 1557, 3.54), stars = TRUE, inferiority_star = NULL),
    c("BF < 0.001", "BF = 0.233", "", "BF > 1000***", "BF = 3.54*")
  )
  expect_identical(
    format_bf(c(0.000045, 0.233, NA, 1557, 3.54), stars = TRUE, stars_only = TRUE),
    c("°°°", "°", "", "***", "*")
  )
  expect_identical(
    format_bf(c(0.000045, 0.033, NA, 1557, 3.54), protect_ratio = TRUE),
    c("BF < 1/1000", "BF = 1/30.30", "", "BF > 1000", "BF = 3.54")
  )
  expect_identical(
    format_bf(c(0.000045, 0.033, NA, 1557, 3.54), protect_ratio = TRUE, exact = TRUE),
    c("BF = 1/2.22e+04", "BF = 1/30.30", "", "BF = 1.56e+03", "BF = 3.54")
  )
  expect_type(format_rope(0.02), "character")
})

test_that("format_number", {
  expect_identical(format_number(2), "two")
  expect_identical(format_number(45), "forty five")
  expect_identical(format_number(2), "two")
})

test_that("format_p", {
  expect_identical(nchar(format_p(0.02)), 9L)
  expect_identical(nchar(format_p(0.02, stars = TRUE)), 10L)
  expect_identical(nchar(format_p(0.02, stars_only = TRUE)), 1L)
})

test_that("format_table, other CI columns", {
  x <- data.frame(test_CI = 0.9, test_CI_low = 0.1, test_CI_high = 1.3)
  test <- utils::capture.output(format_table(x))
  expect_identical(test, c("   test 90% CI", "1 [0.10, 1.30]"))

  x <- data.frame(
    CI = 0.8,
    CI_low = 2.43,
    CI_high = 5.453,
    test_CI = 0.9,
    test_CI_low = 0.1,
    test_CI_high = 1.3
  )
  test <- utils::capture.output(format_table(x))
  expect_identical(test, c("        80% CI  test 90% CI", "1 [2.43, 5.45] [0.10, 1.30]"))

  x <- data.frame(CI_low = 2.43, CI_high = 5.453, test_CI_low = 0.1, test_CI_high = 1.3)
  attr(x, "ci") <- 0.8
  attr(x, "ci_test") <- 0.9
  test <- utils::capture.output(format_table(x))
  expect_identical(test, c("        80% CI  test 90% CI", "1 [2.43, 5.45] [0.10, 1.30]"))

  x <- data.frame(
    CI_low = 2.43,
    CI_high = 5.453,
    test_CI_low = 0.1,
    test_CI_high = 1.3,
    other_CI_low = 0.12,
    other_CI_high = 1.4
  )
  attr(x, "ci") <- 0.8
  attr(x, "ci_test") <- 0.9
  test <- utils::capture.output(format_table(x))
  expect_identical(test, c("        80% CI  test 80% CI other 80% CI", "1 [2.43, 5.45] [0.10, 1.30] [0.12, 1.40]"))
})


test_that("format_table, multiple CI columns", {
  d <- data.frame(
    Parameter = c("(Intercept)", "wt", "cyl"),
    Coefficient = c(39.69, -3.19, -1.51),
    SE = c(1.71, 0.76, 0.41),
    CI_low_0.8 = c(37.44, -4.18, -2.05),
    CI_high_0.8 = c(41.94, -2.2, -0.96),
    CI_low_0.9 = c(36.77, -4.48, -2.21),
    CI_high_0.9 = c(42.6, -1.9, -0.8),
    t = c(23.14, -4.22, -3.64),
    df_error = c(29, 29, 29),
    stringsAsFactors = FALSE
  )
  attr(d, "ci") <- c(0.8, 0.9)
  expect_equal(
    format_table(d),
    data.frame(
      Parameter = c("(Intercept)", "wt", "cyl"),
      Coefficient = c("39.69", "-3.19", "-1.51"),
      SE = c("1.71", "0.76", "0.41"),
      `80% CI` = c("[37.44, 41.94]", "[-4.18, -2.20]", "[-2.05, -0.96]"),
      `90% CI` = c("[36.77, 42.60]", "[-4.48, -1.90]", "[-2.21, -0.80]"),
      `t(29)` = c("23.14", "-4.22", "-3.64"),
      stringsAsFactors = FALSE
    ),
    ignore_attr = TRUE
  )


  # d <- data.frame(
  #   Parameter = c("(Intercept)", "wt", "cyl"),
  #   Coefficient = c(39.69, -3.19, -1.51),
  #   SE = c(1.71, 0.76, 0.41),
  #   CI_low_0.8 = c(37.44, -4.18, -2.05),
  #   CI_high_0.8 = c(41.94, -2.2, -0.96),
  #   CI_low_0.9 = c(36.77, -4.48, -2.21),
  #   CI_high_0.9 = c(42.6, -1.9, -0.8),
  #   t = c(23.14, -4.22, -3.64),
  #   df_error = c(29, 29, 29),
  #   stringsAsFactors = FALSE
  # )
  # expect_equal(
  #   format_table(d),
  #   data.frame(
  #     Parameter = c("(Intercept)", "wt", "cyl"),
  #     Coefficient = c("39.69", "-3.19", "-1.51"),
  #     SE = c("1.71", "0.76", "0.41"),
  #     `80% CI` = c("[37.44, 41.94]", "[-4.18, -2.20]", "[-2.05, -0.96]"),
  #     `90% CI` = c("[36.77, 42.60]", "[-4.48, -1.90]", "[-2.21, -0.80]"),
  #     `t(29)` = c("23.14", "-4.22", "-3.64"),
  #     stringsAsFactors = FALSE
  #   ),
  #   ignore_attr = TRUE
  # )
})


test_that("format_table, preserve attributes", {
  d <- mtcars[1:3, 1:3]
  attr(d, "table_footer") <- "This is a footer"
  attr(d, "table_caption") <- "And the caption"
  d2 <- insight::format_table(d, digits = 3, preserve_attributes = TRUE)
  expect_named(attributes(d2), c("names", "row.names", "class", "table_footer", "table_caption"))
  expect_identical(attributes(d2)$table_caption, "And the caption")
})
