test_that("get_simulated - lm", {
  model <- lm(mpg ~ cyl + hp, data = mtcars)

  out <- get_simulated(model, iterations = 2, seed = 123)
  ref <- stats::simulate(model, nsim = 2, seed = 123)
  names(ref) <- c("iter_1", "iter_2")

  expect_s3_class(out, "data.frame")
  expect_identical(names(out), c("iter_1", "iter_2"))
  expect_identical(nrow(out), nrow(mtcars))
  expect_equal(out, ref, tolerance = 1e-12, ignore_attr = TRUE)
  expect_false(is.null(attributes(out)$seed))
})


test_that("get_simulated - glm, binomial", {
  data(mtcars)
  model <- glm(vs ~ am + wt, data = mtcars, family = "binomial")

  out <- get_simulated(model, iterations = 2, seed = 123)
  ref <- stats::simulate(model, nsim = 2, seed = 123)
  names(ref) <- c("iter_1", "iter_2")

  expect_s3_class(out, "data.frame")
  expect_identical(names(out), c("iter_1", "iter_2"))
  expect_identical(nrow(out), nrow(mtcars))
  expect_equal(out, ref, tolerance = 1e-12, ignore_attr = TRUE)
  expect_false(is.null(attributes(out)$seed))

  model <- glm(vs ~ am + wt, data = mtcars, family = "binomial")
  out <- get_simulated(
    model,
    iterations = 5,
    seed = 123,
    data = insight::get_datagrid(model, "am")
  )
  expect_identical(dim(out), c(2L, 5L))

  skip_if_not_installed("lme4")
  data(cbpp, package = "lme4")

  m <- glm(cbind(incidence, size - incidence) ~ period, data = cbpp, family = binomial)
  out <- get_simulated(m)
  expect_identical(dim(out), c(56L, 1L))
})


test_that("get_simulated - data.frame dispatch", {
  model <- lm(mpg ~ wt + cyl, data = mtcars)
  dg <- get_datagrid(model, wt = c(2, 3), cyl = c(4, 6))

  out <- get_simulated(dg, model, iterations = 3, seed = 123)

  expect_s3_class(out, "data.frame")
  expect_identical(names(out), c("iter_1", "iter_2", "iter_3"))
  expect_identical(nrow(out), nrow(dg))
})


test_that("get_simulated - default method errors", {
  pca <- stats::prcomp(USArrests, scale. = TRUE)
  expect_error(get_simulated(pca), "not yet been implemented")
})


test_that("get_simulated - betareg", {
  skip_if_not_installed("betareg")

  data("GasolineYield", package = "betareg")
  model <- betareg::betareg(yield ~ batch + temp, data = GasolineYield)

  out <- get_simulated(model, iterations = 2, seed = 123)

  expect_s3_class(out, "data.frame")
  expect_identical(names(out), c("iter_1", "iter_2"))
  expect_identical(nrow(out), nrow(GasolineYield))
  expect_false(is.null(attributes(out)$seed))
})


test_that("get_simulated - glmmTMB", {
  skip_if_not_installed("glmmTMB")
  skip_on_cran()

  model <- suppressWarnings(glmmTMB::glmmTMB(
    vs ~ am + (1 | cyl),
    data = mtcars,
    family = "binomial"
  ))

  out <- get_simulated(model, iterations = 2, seed = 123)

  expect_s3_class(out, "data.frame")
  expect_identical(names(out), c("iter_1", "iter_2"))
  expect_false(is.null(attributes(out)$seed))

  expect_error(
    get_simulated(model, data = mtcars[1:4, ], iterations = 1, seed = 123),
    "currently not supported"
  )
})


test_that("get_simulated - merMod", {
  skip_if_not_installed("lme4")

  model <- lme4::lmer(mpg ~ am + (1 | cyl), data = mtcars)
  out <- get_simulated(model, iterations = 2, seed = 123)

  expect_s3_class(out, "data.frame")
  expect_identical(names(out), c("iter_1", "iter_2"))
  expect_identical(nrow(out), nrow(mtcars))
  expect_false(is.null(attributes(out)$seed))

  dg <- get_datagrid(model, am = c(0, 1), cyl = c(4, 6))
  out2 <- get_simulated(
    model,
    data = dg,
    iterations = 2,
    seed = 123,
    allow.new.levels = TRUE
  )
  expect_identical(nrow(out2), nrow(dg))
  expect_identical(names(out2), c("iter_1", "iter_2"))
})
