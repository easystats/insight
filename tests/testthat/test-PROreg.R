skip_if_not(getRversion() >= "4.2.0")
skip_if_not_installed("PROreg", minimum_version = "1.3.0")

test_that("PROreg.BBrm", {
  set.seed(1234)

  # defining the parameters
  k <- 100
  m <- 10
  phi <- 0.5
  beta <- c(1.5, -1.1)
  sigma <- 0.5

  # simulating the covariate and random effects
  x <- runif(k, 0, 10)
  X <- model.matrix(~x)
  z <- as.factor(PROreg::rBI(k, 4, 0.5, 2))
  Z <- model.matrix(~ z - 1)
  u <- rnorm(5, 0, sigma)

  # the linear predictor and simulated response variable
  eta <- beta[1] + beta[2] * x + crossprod(t(Z), u)
  p <- 1 / (1 + exp(-eta))
  y <- PROreg::rBB(k, m, p, phi)
  dat <- data.frame(cbind(y, x, z))
  dat$z <- as.factor(dat$z)

  # apply the model
  invisible(capture.output({
    model <- PROreg::BBmm(
      fixed.formula = y ~ x,
      random.formula = ~z,
      m = m,
      data = dat
    )
  }))

  out <- get_parameters(model)
  expect_identical(out$Parameter, c("Intercept", "x"))
  expect_equal(out$Estimate, c(1.05328, -1.06242), tolerance = 1e-4)

  out <- find_parameters(model)
  expect_identical(out$conditional, c("Intercept", "x"))
  expect_identical(out$random, "z")
})
