if (requiet("testthat") && requiet("insight") && requiet("lme4")) {
  test_that("find_weights", {
    data(mtcars)
    mtcars$weight <- rnorm(nrow(mtcars), 1, .3)
    m <- lm(mpg ~ wt + cyl + vs, data = mtcars, weights = weight)
    expect_equal(find_weights(m), "weight")
  })
  test_that("find_weights", {
    data(iris)
    iris$wgt <- rnorm(nrow(iris), 1, .3)
    m <- lmer(Sepal.Width ~ Sepal.Length + (1 | Species), data = iris, weights = wgt)
    expect_equal(find_weights(m), "wgt")
  })
}
