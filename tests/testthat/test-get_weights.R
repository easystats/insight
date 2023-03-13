if (skip_if_not_or_load_if_installed("lme4")) {
  data(mtcars)
  m1 <- lmer(mpg ~ am + (1 | cyl), data = mtcars)
  m2 <- lm(mpg ~ am, data = mtcars)

  test_that("get_weights", {
    expect_null(get_weights(m1))
    expect_null(get_weights(m2))
  })

  set.seed(123)
  mtcars$w <- abs(rnorm(nrow(mtcars), sd = 0.5))

  m1 <- lmer(mpg ~ am + (1 | cyl), data = mtcars, weights = w)
  m2 <- lm(mpg ~ am, data = mtcars, weights = w)

  test_that("get_weights", {
    expect_equal(
      get_weights(m1),
      mtcars$w,
      tolerance = 1e-2
    )
    expect_equal(
      get_weights(m2),
      mtcars$w,
      tolerance = 1e-2
    )
  })
}

if (skip_if_not_or_load_if_installed("nlme")) {
  data("Orthodont", package = "nlme")
  m <- lme( # a model of variance only
    distance ~ 1,
    data = Orthodont, # grand mean
    weights = varConstPower(form = ~ age | Sex)
  )

  out <- get_weights(m)
  test_that("get_weights nlme", {
    expect_identical(colnames(out), c("age", "Sex"))
  })
}
