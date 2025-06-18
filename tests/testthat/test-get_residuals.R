skip_if_not_installed("lme4")

data(sleepstudy, package = "lme4")
data(cbpp, package = "lme4")
set.seed(123)
mtcars$w <- abs(rnorm(nrow(mtcars), mean = 1, 0.3))
sleepstudy$w <- abs(rnorm(nrow(sleepstudy), mean = 1, 0.3))
cbpp$w <- abs(rnorm(nrow(cbpp), mean = 1, 0.3))

test_that("get_residuals - lm", {
  m <- lm(am ~ cyl, weights = w, data = mtcars)
  expect_equal(
    as.vector(get_residuals(m, weighted = FALSE)),
    as.vector(residuals(m))
  )
  expect_equal(
    as.vector(get_residuals(m, weighted = FALSE, type = "deviance")),
    as.vector(residuals(m, type = "deviance"))
  )
  expect_equal(
    get_weights(m),
    weights(m)
  )
  expect_equal(
    as.vector(get_residuals(m)),
    as.vector(residuals(m))
  )
  expect_equal(
    as.vector(get_deviance(m)),
    as.vector(deviance(m))
  )
  expect_equal(
    get_residuals(m, weighted = TRUE),
    as.vector(weighted.residuals(m))
  )
})

test_that("get_residuals - glm", {
  m <- suppressWarnings(glm(am ~ cyl, weights = w, data = mtcars, family = binomial))
  expect_equal(
    as.vector(get_residuals(m, weighted = FALSE, type = "response")),
    as.vector(residuals(m, type = "response"))
  )
  expect_equal(
    as.vector(get_residuals(m, weighted = FALSE)),
    as.vector(residuals(m))
  )
  expect_equal(
    get_weights(m),
    weights(m)
  )
  expect_equal(
    as.vector(get_residuals(m)),
    as.vector(residuals(m))
  )
  expect_equal(
    as.vector(get_deviance(m)),
    as.vector(deviance(m))
  )
  expect_equal(
    get_residuals(m, weighted = TRUE),
    as.vector(weighted.residuals(m))
  )
})

test_that("get_residuals - lmer", {
  m <- lme4::lmer(Reaction ~ Days + (Days | Subject), weights = w, data = sleepstudy)
  expect_equal(
    as.vector(get_residuals(m, weighted = FALSE)),
    as.vector(residuals(m))
  )
  expect_equal(
    as.vector(get_residuals(m, weighted = FALSE, type = "deviance")),
    as.vector(residuals(m, type = "deviance"))
  )
  expect_equal(
    get_weights(m),
    weights(m)
  )
  expect_equal(
    as.vector(get_residuals(m)),
    as.vector(residuals(m))
  )
  expect_equal(
    as.vector(get_deviance(m)),
    as.vector(deviance(m, REML = FALSE))
  )
  expect_equal(
    get_residuals(m, weighted = TRUE),
    as.vector(weighted.residuals(m))
  )
})

test_that("get_residuals - glmer", {
  m <- lme4::glmer(
    cbind(incidence, size - incidence) ~ period + (1 | herd),
    weights = w,
    data = cbpp,
    family = binomial,
    nAGQ = 0
  )

  expect_equal(
    as.vector(get_residuals(m, weighted = FALSE, type = "response")),
    as.vector(residuals(m, type = "response"))
  )
  expect_equal(
    as.vector(get_residuals(m, weighted = FALSE)),
    as.vector(residuals(m))
  )
  expect_equal(
    get_weights(m),
    weights(m)
  )
  expect_equal(
    as.vector(get_residuals(m)),
    as.vector(residuals(m))
  )
  expect_equal(
    as.vector(get_deviance(m)),
    177.4023,
    tolerance = 1e-3
  )
  expect_equal(
    get_residuals(m, weighted = TRUE),
    as.vector(weighted.residuals(m))
  )
})


test_that("get_residuals - psych::fa", {
  skip_if_not_installed("psych")
  skip_if_not_installed("parameters")
  data(Thurstone, package = "psych")

  # PCA
  set.seed(123)
  x1 <- psych::principal(Thurstone, 3)
  expect_equal(
    head(get_residuals(x1)),
    c(-0.028657, -0.064068, -0.064693, -0.00835, -0.001788, 0.008548),
    tolerance = 1e-3,
    ignore_attr = TRUE
  )

  # FA
  set.seed(123)
  x2 <- psych::fa(Thurstone, 3)
  expect_equal(
    head(get_residuals(x2)),
    c(0.005337, 0.000473, -0.005609, -0.006647, 0.00088, 0.007646),
    tolerance = 1e-3,
    ignore_attr = TRUE
  )

  set.seed(123)
  x3 <- parameters::factor_analysis(as.data.frame(Thurstone), 3, standardize = FALSE)
  expect_equal(
    head(get_residuals(x3)),
    c(0.005337, 0.000473, -0.005609, -0.006647, 0.00088, 0.007646),
    tolerance = 1e-3,
    ignore_attr = TRUE
  )
})
