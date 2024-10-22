test_that("find_transformation - identity", {
  model <- lm(Sepal.Length ~ Species, data = iris)
  expect_identical(find_transformation(model), "identity")
})

test_that("find_transformation - log", {
  model <- lm(log(Sepal.Length) ~ Species, data = iris)
  expect_identical(find_transformation(model), "log")
  expect_identical(find_transformation("log(Sepal.Length)"), "log")
})

test_that("find_transformation - log for function names", {
  # "time" is a function name - make sure this works correctly
  iris$time <- iris$Sepal.Length
  model <- lm(log(time) ~ Species, data = iris)
  expect_identical(find_transformation(model), "log")
  expect_identical(find_transformation("log(time)"), "log")
})

test_that("find_transformation - log+x", {
  model <- lm(log(Sepal.Length + 3) ~ Species, data = iris)
  expect_identical(find_transformation(model), "log(x+3)")
})

test_that("find_transformation - log+x 2", {
  model <- lm(log(2 + Sepal.Length) ~ Species, data = iris)
  expect_identical(find_transformation(model), "log(x+2)")
  expect_identical(find_transformation("log(2 + Sepal.Length)"), "log(x+2)")
})

test_that("find_transformation - log-log", {
  model <- lm(log(log(Sepal.Length)) ~ Species, data = iris)
  expect_identical(find_transformation(model), "log-log")
})

test_that("find_transformation - sqrt", {
  model <- lm(sqrt(Sepal.Length) ~ Species, data = iris)
  expect_identical(find_transformation(model), "sqrt")
})

test_that("find_transformation - power-1", {
  model <- lm(I(Sepal.Length^2) ~ Species, data = iris)
  expect_identical(find_transformation(model), "power")
})

test_that("find_transformation - power-2", {
  model <- lm(I(Sepal.Length^2.5) ~ Species, data = iris)
  expect_identical(find_transformation(model), "power")
})

test_that("find_transformation - scale", {
  model <- lm(mpg / 0.7 ~ hp, data = mtcars)
  expect_identical(find_transformation(model), "scale")
  model <- lm(I(mpg / 0.7) ~ hp, data = mtcars)
  expect_identical(find_transformation(model), "scale")
})

test_that("find_transformation - box-cox", {
  m <- lm((mpg^0.7 - 1) / 0.7 ~ hp, data = mtcars)
  expect_identical(find_transformation(m), "box-cox")
  m <- lm(I((mpg^0.7 - 1) / 0.7) ~ hp, data = mtcars)
  expect_identical(find_transformation(m), "box-cox")
})

test_that("find_transformation - unknown", {
  model <- lm(I(2 * mpg + 3) ~ hp, data = mtcars)
  expect_null(find_transformation(model))
})

test_that("find_transformation - strange bayestestR example", {
  mod <- lm(log(mpg) ~ gear + hp, data = mtcars)
  expect_identical(find_transformation(mod), "log")
})

test_that("find_transformation - inverse", {
  data(iris)
  model <- lm(1 / Sepal.Length ~ Species, data = iris)
  expect_identical(find_transformation(model), "inverse")
  expect_identical(find_transformation("1 / Sepal.Length"), "inverse")
})

test_that("find_transformation - detect powers", {
  # styler: off
  data(iris)
  m1 <- lm(Sepal.Length^(1 / 2) ~ Species, data = iris)
  m2 <- lm(Sepal.Length^2 ~ Species, data = iris)
  m3 <- lm(I(Sepal.Length^(1 / 2)) ~ Species, data = iris)
  m4 <- lm(I(Sepal.Length^3) ~ Species, data = iris)
  m5 <- lm(I(Sepal.Length^2) ~ Species, data = iris)
  m6 <- lm(Sepal.Length ^ 2.3 ~ Species, data = iris)
  m7 <- lm(Sepal.Length^-0.5 ~ Species, data = iris)

  expect_identical(insight::find_transformation(m1), "power")
  expect_identical(insight::find_transformation(m2), "power")
  expect_identical(insight::find_transformation(m3), "power")
  expect_identical(insight::find_transformation(m4), "power")
  expect_identical(insight::find_transformation(m5), "power")
  expect_identical(insight::find_transformation(m6), "power")
  expect_identical(insight::find_transformation(m7), "power")
  expect_identical(insight::find_transformation("Sepal.Length^-0.5"), "power")

  # power **

  m1 <- lm(Sepal.Length**(1 / 2) ~ Species, data = iris)
  m2 <- lm(Sepal.Length**2 ~ Species, data = iris)
  m3 <- lm(I(Sepal.Length**(1 / 2)) ~ Species, data = iris)
  m4 <- lm(I(Sepal.Length**3) ~ Species, data = iris)
  m5 <- lm(I(Sepal.Length**2) ~ Species, data = iris)
  m6 <- lm(I(Sepal.Length ** 1.8) ~ Species, data = iris)

  expect_identical(insight::find_transformation(m1), "power")
  expect_identical(insight::find_transformation(m2), "power")
  expect_identical(insight::find_transformation(m3), "power")
  expect_identical(insight::find_transformation(m4), "power")
  expect_identical(insight::find_transformation(m5), "power")
  expect_identical(insight::find_transformation(m6), "power")
  # styler: on
})
