if (require("testthat") && require("insight")) {
  data(iris)

  m1 <- lm(Sepal.Length ~ Petal.Width + Species, data = iris)
  m2 <- lm(Sepal.Length ~ Species, data = iris)
  m3 <- lm(Sepal.Length ~ Species, data = iris)
  m4 <- lm(Sepal.Length ~ 1, data = iris)

  test_that("ellipses_info", {
    expect_message(ellipsis_info(m1, m2, m3, m4))
  })

  info <- ellipsis_info(m1, m2, m4)
  test_that("ellipses_info", {
    expect_equal(attributes(info)$is_nested, TRUE)
    expect_equal(attributes(info)$is_nested_decreasing, TRUE)
    expect_equal(attributes(info)$is_nested_increasing, FALSE)
  })

  info <- ellipsis_info(m4, m3, m1)
  test_that("ellipses_info", {
    expect_equal(attributes(info)$is_nested, TRUE)
    expect_equal(attributes(info)$is_nested_decreasing, FALSE)
    expect_equal(attributes(info)$is_nested_increasing, TRUE)
  })
}
