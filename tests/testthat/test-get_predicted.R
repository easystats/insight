if (require("testthat") && require("insight") && require("lme4")) {
  data(mtcars)

  test_that("get_predicted - lm", {
    expect_equal(length(get_predicted(lm(mpg ~ am, data = mtcars))), 32)
  })

  # m <- lmer(mpg ~ am + (1 | cyl), data = mtcars)
}
