if (require("testthat") && require("insight") && require("lme4")) {
  data(mtcars)


  test_that("get_predicted - lm", {
    expect_equal(length(get_predicted(lm(mpg ~ am, data = mtcars))), 32)
  })


  test_that("get_predicted - data", {
    set.seed(333)
    m <- lm(mpg ~ am, data = mtcars)
    newdata <- data.frame(am = sample(c(0,1), replace=TRUE, size=20))

    expect_equal(length(get_predicted(m, newdata)), 20)
    expect_equal(length(get_predicted(newdata, m)), 20)
  })

  # m <- lmer(mpg ~ am + (1 | cyl), data = mtcars)
}
