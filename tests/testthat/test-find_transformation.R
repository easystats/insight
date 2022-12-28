test_that("find_transformation - identity", {
  model <- lm(Sepal.Length ~ Species, data = iris)
  expect_equal(
    find_transformation(model),
    "identity"
  )
})

test_that("find_transformation - log", {
  model <- lm(log(Sepal.Length) ~ Species, data = iris)
  expect_equal(
    find_transformation(model),
    "log"
  )
})

test_that("find_transformation - log+x", {
  model <- lm(log(Sepal.Length + 3) ~ Species, data = iris)
  expect_equal(
    find_transformation(model),
    "log(x+3)"
  )
})

test_that("find_transformation - log+x 2", {
  model <- lm(log(2 + Sepal.Length) ~ Species, data = iris)
  expect_equal(
    find_transformation(model),
    "log(x+2)"
  )
})

test_that("find_transformation - log-log", {
  model <- lm(log(log(Sepal.Length)) ~ Species, data = iris)
  expect_equal(
    find_transformation(model),
    "log-log"
  )
})

test_that("find_transformation - sqrt", {
  model <- lm(sqrt(Sepal.Length) ~ Species, data = iris)
  expect_equal(
    find_transformation(model),
    "sqrt"
  )
})

test_that("find_transformation - power-1", {
  model <- lm(I(Sepal.Length^2) ~ Species, data = iris)
  expect_equal(
    find_transformation(model),
    "power"
  )
})

test_that("find_transformation - power-2", {
  model <- lm(I(Sepal.Length^2) ~ Species, data = iris)
  expect_equal(
    find_transformation(model),
    "power"
  )
})

test_that("find_transformation - unknown", {
  model <- lm(I(2 * mpg + 3) ~ hp, data = mtcars)
  expect_null(find_transformation(model))
})
