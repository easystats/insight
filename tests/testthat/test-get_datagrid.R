if (requiet("testthat") && requiet("insight")) {
  m1 <- lm(hp ~ ordered(cyl), data = mtcars)
  m2 <- lm(hp ~ as.ordered(cyl), data = mtcars)
  m3 <- lm(hp ~ as.factor(cyl), data = mtcars)
  m4 <- lm(hp ~ factor(cyl), data = mtcars)

  test_that("get_data", {
    expect_true(attributes(get_data(m1)$cyl)$factor)
    expect_equal(get_datagrid(m1)$cyl, c(4, 6, 8))
    expect_true(attributes(get_data(m2)$cyl)$factor)
    expect_equal(get_datagrid(m2)$cyl, c(4, 6, 8))
    expect_true(attributes(get_data(m3)$cyl)$factor)
    expect_equal(get_datagrid(m3)$cyl, c(4, 6, 8))
    expect_true(attributes(get_data(m4)$cyl)$factor)
    expect_equal(get_datagrid(m4)$cyl, c(4, 6, 8))
  })
}
