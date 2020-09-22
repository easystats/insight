if (require("testthat") && require("insight") && require("lme4")) {
  data(mtcars)
  m1 <- lmer(mpg ~ am + (1 | cyl), data = mtcars)
  m2 <- lm(mpg ~ am, data = mtcars)

  test_that("get_data", {
    expect_null(get_weights(m1))
    expect_null(get_weights(m2))
  })
}
