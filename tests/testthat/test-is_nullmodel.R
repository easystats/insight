skip_if_not_installed("lme4")

data(sleepstudy, package = "lme4")

m1 <- lm(mpg ~ 1, data = mtcars)
m2 <- lm(mpg ~ gear, data = mtcars)
m3 <- lme4::lmer(Reaction ~ 1 + (Days | Subject), data = sleepstudy)
m4 <- lme4::lmer(Reaction ~ Days + (Days | Subject), data = sleepstudy)
m5 <- suppressWarnings(lme4::lmer(Reaction ~ 0 + (Days | Subject), data = sleepstudy))

test_that("is_nullmodel", {
  expect_true(is_nullmodel(m1))
  expect_false(is_nullmodel(m2))
  expect_true(is_nullmodel(m3))
  expect_false(is_nullmodel(m4))
  expect_true(is_nullmodel(m5))
})

test_that("is_nullmodel, don't be verbose", {
  expect_silent(is_nullmodel(lm(mtcars$mpg ~ 1)))
})
