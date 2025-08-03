test_that("get_transformation - detect powers", {
  data(iris)
  m <- lm(Sepal.Length^2 ~ Species, data = iris)
  fun <- get_transformation(m)
  expect_identical(fun$transformation(2), 4)
  expect_identical(fun$inverse(4), 2)
  m <- lm(I(Sepal.Length^3) ~ Species, data = iris)
  fun <- get_transformation(m)
  expect_identical(fun$transformation(2), 8)
  expect_identical(fun$inverse(8), 2)
  m <- lm(Sepal.Length^4.7 ~ Species, data = iris)
  fun <- get_transformation(m)
  expect_equal(fun$transformation(2), 25.99208, tolerance = 1e-3)
  expect_equal(fun$inverse(25.99208), 2, tolerance = 1e-3)

  # fail for power to 0
  data(mtcars)
  mod <- lm(mpg^0 ~ hp, data = mtcars)
  expect_message(
    {
      out <- get_transformation(mod)
    },
    regex = "The transformation"
  )
  expect_null(out)
})


test_that("get_transformation - detect scale", {
  data(mtcars)
  m <- lm(mpg / 0.7 ~ hp, data = mtcars)
  fun <- get_transformation(m)
  expect_equal(fun$transformation(2), 2 / 0.7, tolerance = 1e-3)
  expect_equal(fun$inverse(2 / 0.7), 2, tolerance = 1e-3)
  m <- lm(I(mpg / 0.7) ~ hp, data = mtcars)
  fun <- get_transformation(m)
  expect_equal(fun$transformation(2), 2 / 0.7, tolerance = 1e-3)
  expect_equal(fun$inverse(2 / 0.7), 2, tolerance = 1e-3)
})


test_that("get_transformation - box-cox", {
  data(mtcars)
  m <- lm((mpg^0.7 - 1) / 0.7 ~ hp, data = mtcars)
  fun <- get_transformation(m)
  expect_equal(fun$transformation(2), (2^0.7 - 1) / 0.7, tolerance = 1e-3)
  expect_equal(fun$inverse((2^0.7 - 1) / 0.7), 2, tolerance = 1e-3)
  m <- lm(I((mpg^0.7 - 1) / 0.7) ~ hp, data = mtcars)
  fun <- get_transformation(m)
  expect_equal(fun$transformation(2), (2^0.7 - 1) / 0.7, tolerance = 1e-3)
  expect_equal(fun$inverse((2^0.7 - 1) / 0.7), 2, tolerance = 1e-3)
})


test_that("get_transformation - include_all", {
  model <- lm(mpg ~ log(wt) + I(gear^2) + exp(am), data = mtcars)
  out <- get_transformation(model, include_all = TRUE)
  expect_named(out, c("response", "conditional"))
  expect_named(out$conditional, c("wt", "gear", "am"))
  expect_equal(out$conditional$gear$transformation(2), 4, tolerance = 1e-3)
  expect_equal(out$conditional$gear$inverse(2), sqrt(2), tolerance = 1e-3)
})


test_that("get_transformation - base-log", {
  mdl <- lm(log(dist, base = 8) ~ speed, data = cars)
  f <- get_transformation(mdl)
  expect_equal(f$transformation(10), 1.10730936496245, tolerance = 1e-5)
  expect_equal(f$inverse(1.10730936496245), 10, tolerance = 1e-4)
  mdl <- lm(log(dist, base = 10) ~ speed, data = cars)
  f <- get_transformation(mdl)
  expect_equal(f$transformation(10), 1, tolerance = 1e-4)
  expect_equal(f$inverse(1), 10, tolerance = 1e-4)
})
