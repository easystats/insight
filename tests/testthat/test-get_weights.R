if (require("testthat") && require("insight") && require("lme4")) {
  data(mtcars)
  m1 <- lmer(mpg ~ am + (1 | cyl), data = mtcars)
  m2 <- lm(mpg ~ am, data = mtcars)

  test_that("get_weights", {
    expect_null(get_weights(m1))
    expect_null(get_weights(m2))
  })

  set.seed(123)
  mtcars$w <- abs(rnorm(nrow(mtcars), sd = .5))

  m1 <- lmer(mpg ~ am + (1 | cyl), data = mtcars, weights = w)
  m2 <- lm(mpg ~ am, data = mtcars, weights = w)

  test_that("get_weights", {
    expect_equal(
      get_weights(m1),
      c(0.28, 0.115, 0.779, 0.035, 0.065, 0.858, 0.23, 0.633, 0.343,
        0.223, 0.612, 0.18, 0.2, 0.055, 0.278, 0.893, 0.249, 0.983, 0.351,
        0.236, 0.534, 0.109, 0.513, 0.364, 0.313, 0.843, 0.419, 0.077,
        0.569, 0.627, 0.213, 0.148),
      tolerance = 1e-2
    )
    expect_equal(
      get_weights(m2),
      c(0.28, 0.115, 0.779, 0.035, 0.065, 0.858, 0.23, 0.633, 0.343,
        0.223, 0.612, 0.18, 0.2, 0.055, 0.278, 0.893, 0.249, 0.983, 0.351,
        0.236, 0.534, 0.109, 0.513, 0.364, 0.313, 0.843, 0.419, 0.077,
        0.569, 0.627, 0.213, 0.148),
      tolerance = 1e-2
    )
  })
}
