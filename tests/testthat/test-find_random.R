skip_if_not_installed("mgcv")
skip_if_not_installed("gamm4")
skip_if_not_installed("rstanarm")

data <- iris
data$g <- data$Species
data$Xr <- data$Species


test_that("find_random - mgcv::gamm", {
  model <- mgcv::gamm(Petal.Length ~ Petal.Width + s(Sepal.Length), random = list(Species = ~1), data = iris)
  expect_equal(insight::find_random(model, flatten = TRUE), "Species")

  model <- mgcv::gamm(Petal.Length ~ Petal.Width + s(Sepal.Length), random = list(g = ~1), data = data)
  expect_equal(insight::find_random(model, flatten = TRUE), "g")
})

test_that("find_random - gamm4::gamm4", {
  model <- gamm4::gamm4(Petal.Length ~ Petal.Width + s(Sepal.Length), random = ~ (1 | Species), data = iris)
  expect_equal(insight::find_random(model, flatten = TRUE), "Species")

  model <- gamm4::gamm4(Petal.Length ~ Petal.Width + s(Sepal.Length), random = ~ (1 | Xr), data = data)
  expect_equal(insight::find_random(model, flatten = TRUE), "Xr")
})

test_that("find_random - rstanarm::gamm4", {
  skip_on_cran()
  model <-
    suppressWarnings(rstanarm::stan_gamm4(
      Petal.Length ~ Petal.Width + s(Sepal.Length),
      random = ~ (1 | Species),
      data = iris,
      iter = 100,
      chains = 1,
      refresh = 0
    ))
  expect_equal(insight::find_random(model, flatten = TRUE), "Species")
})
