if (require("testthat") && require("insight") && require("lme4")) {
  data(mtcars)
  data(sleepstudy)
  data(iris)

  m1 <- lm(mpg ~ 0 + gear, data = mtcars)
  m2 <- lm(mpg ~ gear, data = mtcars)
  m3 <- suppressWarnings(lmer(Reaction ~ 0 + Days + (Days | Subject), data = sleepstudy))
  m4 <- lmer(Reaction ~ Days + (Days | Subject), data = sleepstudy)
  m5 <- suppressWarnings(lmer(Reaction ~ 0 + (Days | Subject), data = sleepstudy))

  m6 <- lm(Sepal.Length ~ 0 + Petal.Width + Species, data = iris)
  m7 <- lm(Sepal.Length ~ -1 + Petal.Width + Species, data = iris)
  m8 <- lm(Sepal.Length ~ Petal.Width + Species, data = iris)
  m9 <- lm(Sepal.Length ~ Petal.Width + Species + 1, data = iris)

  test_that("has_intercept", {
    expect_true(has_intercept(m2))
    expect_false(has_intercept(m1))
    expect_true(has_intercept(m4))
    expect_false(has_intercept(m3))
    expect_false(has_intercept(m5))
    expect_false(has_intercept(m6))
    expect_false(has_intercept(m7))
    expect_true(has_intercept(m8))
    expect_true(has_intercept(m9))
  })
}
