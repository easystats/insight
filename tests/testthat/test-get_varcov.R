skip_if_not_installed("sandwich")
skip_if_not_installed("clubSandwich")

test_that("informative error in get_varcov.default", {
  skip_if_not_installed("lme4")
  mod <- lme4::lmer(mpg ~ hp + (1 | cyl), data = mtcars)
  # sandwich: not supported
  expect_error(get_varcov(mod, vcov = "HC2"))
  # clubSandwich: supported
  expect_equal(
    get_varcov(mod, vcov = "CR0"),
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
  expect_equal(
    get_varcov(mod, vcov = sandwich::vcovOPG),
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


test_that("glmmTMB: sandwich", {
  skip_on_cran()
  skip_if_not_installed("glmmTMB", minimum_version = "1.1.12")
  data(Salamanders, package = "glmmTMB")
  m <- glmmTMB::glmmTMB(
    count ~ spp + cover + mined + (1 | site),
    ziformula = ~ spp + mined,
    dispformula = ~ DOY + (1 | site),
    data = Salamanders,
    family = glmmTMB::nbinom2
  )

  out1 <- get_varcov(m)
  out2 <- get_varcov(m, vcov = "HC")
  out3 <- sandwich::vcovHC(m)

  expect_equal(
    out1[, 1],
    c(
      `(Intercept)` = 0.314644,
      sppPR = 0.003394,
      sppDM = -0.039042,
      `sppEC-A` = -0.035532,
      `sppEC-L` = -0.042076,
      `sppDES-L` = -0.032686,
      sppDF = -0.037002,
      cover = -0.021823,
      minedno = -0.252979
    ),
    tolerance = 1e-4
  )

  expect_equal(
    out2[, 1],
    c(
      `(Intercept)` = 2.636175,
      sppPR = 0.842328,
      sppDM = -0.162162,
      `sppEC-A` = -0.100184,
      `sppEC-L` = -0.125641,
      `sppDES-L` = 0.038835,
      sppDF = -0.08137,
      cover = -0.437675,
      minedno = -2.102398
    ),
    tolerance = 1e-4
  )

  expect_equal(out2, out3, tolerance = 1e-4, ignore_attr = TRUE)

  out1 <- get_varcov(m, component = "zero_inflated")
  out2 <- get_varcov(m, vcov = "HC", component = "zero_inflated")
  out3 <- sandwich::vcovHC(m, full = TRUE)

  expect_equal(
    out1[, 1],
    c(
      `zi~(Intercept)` = 0.496342,
      `zi~sppPR` = -0.189782,
      `zi~sppDM` = -0.185776,
      `zi~sppEC-A` = -0.190133,
      `zi~sppEC-L` = -0.20859,
      `zi~sppDES-L` = -0.188361,
      `zi~sppDF` = 0.259155,
      `zi~minedno` = -0.32785
    ),
    tolerance = 1e-4
  )

  expect_equal(
    out2[, 1],
    c(
      `zi~(Intercept)` = 3.019479,
      `zi~sppPR` = 1.723305,
      `zi~sppDM` = 0.830846,
      `zi~sppEC-A` = -0.03649,
      `zi~sppEC-L` = 0.362275,
      `zi~sppDES-L` = 1.034537,
      `zi~sppDF` = 7.349731,
      `zi~minedno` = -2.580633
    ),
    tolerance = 1e-4
  )

  expect_equal(out2, out3[10:17, 10:17], tolerance = 1e-4, ignore_attr = TRUE)

  out1 <- get_varcov(m, component = "all")
  out2 <- get_varcov(m, vcov = "HC", component = "all")
  expect_identical(dim(out1), dim(out2))
  expect_identical(dim(out1), c(19L, 19L))

  out1 <- get_varcov(m, component = "full")
  out2 <- get_varcov(m, vcov = "HC", component = "full")
  expect_identical(dim(out1), dim(out2))
  expect_identical(dim(out1), c(21L, 21L))
})


test_that("lm: clubSandwich", {
  mod <- lm(mpg ~ hp * wt, data = mtcars)
  expect_equal(
    get_varcov(mod, vcov = "CR", vcov_args = list(cluster = mtcars$cyl, type = "CR0")),
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
