.runThisTest <- Sys.getenv("RunAllinsightTests") == "yes"

if (.runThisTest &&
  skip_if_not_or_load_if_installed("glmmTMB") &&
  skip_if_not_or_load_if_installed("lme4") &&
  skip_if_not_or_load_if_installed("TMB") &&
  getRversion() >= "4.0.0") {
  data(mtcars)
  m1 <- suppressWarnings(glmer.nb(mpg ~ disp + (1 | cyl) + offset(log(wt)), data = mtcars))
  m2 <- suppressWarnings(glmer.nb(mpg ~ disp + (1 | cyl), offset = log(wt), data = mtcars))

  test_that("null_model with offset", {
    nm1 <- null_model(m1)
    nm2 <- null_model(m2)
    expect_equal(fixef(nm1), fixef(nm2), tolerance = 1e-4)
  })

  skip_on_os("mac") # error: FreeADFunObject
  m1 <- suppressWarnings(glmmTMB(mpg ~ disp + (1 | cyl) + offset(log(wt)), data = mtcars))
  m2 <- suppressWarnings(glmmTMB(mpg ~ disp + (1 | cyl), offset = log(wt), data = mtcars))

  test_that("null_model with offset", {
    nm1 <- null_model(m1)
    nm2 <- null_model(m2)
    expect_equal(fixef(nm1), fixef(nm2), tolerance = 1e-4)
  })


  # set.seed(123)
  # N <- 100 # Samples
  # x <- runif(N, 0, 10) # Predictor
  # off <- rgamma(N, 3, 2) # Offset variable
  # yhat <- -1 + x * 0.5 + log(off) # Prediction on log scale
  #
  # y <- rpois(N, exp(yhat)) # Poisson process
  # y <- ifelse(rbinom(N, 1, 0.3), 0, y) # Zero-inflation process
  #
  # d <<- data.frame(y = y, x, logOff = log(off)) # Storage dataframe
  #
  # m1 <- glm(y ~ x + offset(logOff), data = d, family = "poisson")
  # m2 <- glm(y ~ x, offset = logOff, data = d, family = "poisson")
  #
  # test_that("null_model with offset", {
  #   nm1 <- null_model(m1)
  #   nm2 <- null_model(m2)
  #   expect_equal(coef(nm1), coef(nm2), tolerance = 1e-4)
  # })
}
