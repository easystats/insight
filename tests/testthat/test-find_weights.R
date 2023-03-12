if (skip_if_not_or_load_if_installed("lme4")) {
  test_that("find_weights", {
    data(mtcars)
    mtcars$weight <- rnorm(nrow(mtcars), 1, 0.3)
    m <- lm(mpg ~ wt + cyl + vs, data = mtcars, weights = weight)
    expect_identical(find_weights(m), "weight")
  })
  test_that("find_weights", {
    data(iris)
    iris$wgt <- rnorm(nrow(iris), 1, 0.3)
    m <- lmer(Sepal.Width ~ Sepal.Length + (1 | Species), data = iris, weights = wgt)
    expect_identical(find_weights(m), "wgt")
  })
}


if (skip_if_not_or_load_if_installed("nlme")) {
  data(Orthodont)
  Orthodont$w <- abs(rnorm(nrow(Orthodont)))

  m1 <- lme(
    distance ~ age,
    data = Orthodont,
    random = ~ 1 | Subject,
    weights = varIdent(form = ~ 1 | Sex)
  )

  m2 <- lme(
    distance ~ age,
    data = Orthodont,
    random = ~ 1 | Subject
  )

  m3 <- lme(
    distance ~ age,
    data = Orthodont,
    random = ~ 1 | Subject,
    weights = ~w
  )

  test_that("find_weights", {
    expect_identical(find_weights(m1), "Sex")
    expect_null(find_weights(m2))
    expect_identical(find_weights(m3), "w")
  })
}
