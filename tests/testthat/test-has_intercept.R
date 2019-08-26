if (require("testthat") && require("insight")) {
  context("insight, has_intercept")

  library(lme4)
  data(mtcars)
  data(sleepstudy)

  m1 <- lm(mpg ~ 0 + gear, data = mtcars)
  m2 <- lm(mpg ~ gear, data = mtcars)
  m3 <-
    suppressWarnings(lmer(Reaction ~ 0 + Days + (Days |
      Subject), data = sleepstudy))
  m4 <- lmer(Reaction ~ Days + (Days | Subject), data = sleepstudy)
  m5 <-
    suppressWarnings(lmer(Reaction ~ 0 + (Days |
      Subject), data = sleepstudy))

  test_that("has_intercept", {
    expect_true(has_intercept(m2))
    expect_false(has_intercept(m1))
    expect_true(has_intercept(m4))
    expect_false(has_intercept(m3))
    expect_false(has_intercept(m5))
  })
}
