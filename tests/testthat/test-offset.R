if (requiet("testthat") &&
  requiet("insight") &&
  requiet("pscl")) {
  # Generate some zero-inflated data
  set.seed(123)
  N <- 100 # Samples
  x <- runif(N, 0, 10) # Predictor
  off <- rgamma(N, 3, 2) # Offset variable
  yhat <- -1 + x * 0.5 + log(off) # Prediction on log scale
  dat <- data.frame(y = NA, x, logOff = log(off)) # Storage dataframe

  dat$y <- rpois(N, exp(yhat)) # Poisson process
  dat$y <- ifelse(rbinom(N, 1, 0.3), 0, dat$y) # Zero-inflation process

  # Fit zeroinfl model using 2 methods of offset input
  m1 <- zeroinfl(y ~ offset(logOff) + x | 1, data = dat, dist = "poisson")
  m2 <- zeroinfl(y ~ x | 1,
    data = dat,
    offset = logOff,
    dist = "poisson"
  )

  # Fit zeroinfl model without offset data
  m3 <- zeroinfl(y ~ x | 1, data = dat, dist = "poisson")

  test_that("offset in get_data()", {
    expect_equal(colnames(get_data(m1)), c("y", "logOff", "x"))
    expect_equal(colnames(get_data(m2)), c("y", "x", "logOff"))
    expect_equal(colnames(get_data(m3)), c("y", "x"))
  })

  test_that("offset in get_data()", {
    expect_equal(find_offset(m1), "logOff")
    expect_equal(find_offset(m2), "logOff")
    expect_null(find_offset(m3))
  })

  test_that("offset in null_model()", {
    nm1 <- null_model(m1)
    nm2 <- null_model(m2)
    expect_equal(coef(nm1), coef(nm2), tolerance = 1e-4)
  })
}
