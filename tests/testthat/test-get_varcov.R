skip_if_not_installed("sandwich")
skip_if_not_installed("clubSandwich")

test_that("informative error in get_varcov.default", {
  skip_if_not_installed("lme4")
  mod <- lme4::lmer(mpg ~ hp + (1 | cyl), data = mtcars)
  # sandwich: not supported
  expect_error(get_varcov(mod, vcov = "HC2"))
  # clubSandwich: supported
  expect_equal(get_varcov(mod, vcov = "CR0"),
    clubSandwich::vcovCR(mod, type = "CR0"),
    tolerance = 1e-4,
    ignore_attr = TRUE
  )
})

test_that("lm: sandwich", {
  mod <- lm(mpg ~ hp * wt, data = mtcars)
  expect_equal(
    get_varcov(mod, vcov = "HC1"),
    sandwich::vcovHC(mod, type = "HC1"),
    ignore_attr = TRUE
  )
  expect_equal(
    get_varcov(mod, vcov = "HC4"),
    sandwich::vcovHC(mod, type = "HC4"),
    ignore_attr = TRUE
  )
  expect_equal(
    get_varcov(mod, vcov = "HC", vcov_args = list(type = "HC4")),
    sandwich::vcovHC(mod, type = "HC4"),
    ignore_attr = TRUE
  )
  expect_equal(get_varcov(mod, vcov = sandwich::vcovOPG),
    sandwich::vcovOPG(mod),
    tolerance = 1e-5
  )

  # examples from ?vcovBS
  skip_on_cran()
  data("PetersenCL", package = "sandwich")
  m <- lm(y ~ x, data = PetersenCL)

  set.seed(1234)
  expect_equal(
    get_varcov(m, vcov = "jackknife", vcov_args = list(cluster = PetersenCL$firm)),
    sandwich::vcovBS(m, cluster = ~firm, type = "jackknife"),
    tolerance = 1e-5
  )
})

test_that("lm: clubSandwich", {
  mod <- lm(mpg ~ hp * wt, data = mtcars)
  expect_equal(
    get_varcov(mod,
      vcov = "CR",
      vcov_args = list(cluster = mtcars$cyl, type = "CR0")
    ),
    clubSandwich::vcovCR(mod, cluster = mtcars$cyl, type = "CR0"),
    tolerance = 1e-5,
    ignore_attr = TRUE
  )
})


test_that("mlm: sandwich", {
  mod <- lm(cbind(cyl, disp, hp) ~ drat, data = mtcars)
  v1 <- get_varcov(mod, vcov = "HC3")
  v2 <- sandwich::vcovHC(mod)
  expect_equal(v1, v2, tolerance = 1e-4, ignore_attr = TRUE)
})


test_that("warning: not yet supported", {
  skip_if_not_installed("pscl")
  data("bioChemists", package = "pscl")
  mod <- pscl::hurdle(art ~ phd + fem | ment, data = bioChemists, dist = "negbin")
  expect_error(get_varcov(mod, vcov = "HC3"), regexp = "supported by one or")
})


test_that("verbose and deprecated arguments", {
  mod <- lm(mpg ~ hp, data = mtcars)
  v1 <- suppressWarnings(get_varcov(mod, robust = TRUE))
  v2 <- suppressWarnings(get_varcov(mod))
  expect_equal(v1, v2, tolerance = 1e-4, ignore_attr = TRUE)
  expect_warning(get_varcov(mod, robust = TRUE), regexp = "no longer supported")
})


test_that("error: ill-defined model", {
  dd <- data.frame(y = as.difftime(0:5, units = "days"))
  m1 <- lm(y ~ 1, data = dd)
  expect_error(get_varcov(m1), regex = "Can't extract variance-covariance")
})


test_that("error: bad string", {
  m <- lm(mpg ~ hp + factor(carb), data = mtcars)
  expect_error(get_varcov(m, vcov = "bootstrap"), regexp = "not a recognized")
})


test_that("error: from vcov", {
  data(iris)
  mod <- lm(Sepal.Width ~ Sepal.Length, data = iris)
  expect_error(
    get_varcov(mod, vcov = "vcovBS", vcov_args = list(cluster = "Species")),
    regex = "number of observations"
  )
})
