if (require("testthat") && require("insight")) {
  library(lme4)
  data(mtcars)
  data(sleepstudy)

  m1 <- lm(mpg ~ 1, data = mtcars)
  m2 <- lm(mpg ~ gear, data = mtcars)
  m3 <- lmer(Reaction ~ 1 + (Days | Subject), data = sleepstudy)
  m4 <- lmer(Reaction ~ Days + (Days | Subject), data = sleepstudy)
  m5 <-
    suppressWarnings(lmer(Reaction ~ 0 + (Days | Subject), data = sleepstudy))

  test_that("is_nullmodel", {
    expect_true(is_nullmodel(m1))
    expect_false(is_nullmodel(m2))
    expect_true(is_nullmodel(m3))
    expect_false(is_nullmodel(m4))
    expect_true(is_nullmodel(m5))
  })
}
