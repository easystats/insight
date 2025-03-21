skip_if_not_installed("glmmTMB")
skip_if_not_installed("lme4")
skip_if_not_installed("TMB")

test_that("null_model with offset", {
  m1 <- suppressWarnings(lme4::glmer.nb(mpg ~ disp + (1 | cyl) + offset(log(wt)), data = mtcars))
  m2 <- suppressWarnings(lme4::glmer.nb(mpg ~ disp + (1 | cyl), offset = log(wt), data = mtcars))
  nm1 <- null_model(m1)
  nm2 <- null_model(m2)
  expect_equal(glmmTMB::fixef(nm1), glmmTMB::fixef(nm2), tolerance = 1e-4)
})

skip_on_os("mac") # error: FreeADFunObject

test_that("null_model with offset", {
  m1 <- suppressWarnings(glmmTMB::glmmTMB(mpg ~ disp + (1 | cyl) + offset(log(wt)), data = mtcars))
  m2 <- suppressWarnings(glmmTMB::glmmTMB(mpg ~ disp + (1 | cyl), offset = log(wt), data = mtcars))
  nm1 <- null_model(m1)
  nm2 <- null_model(m2)
  expect_equal(glmmTMB::fixef(nm1), glmmTMB::fixef(nm2), tolerance = 1e-4)
})

test_that("null_model zero-inflated", {
  data(fish, package = "insight")
  m0 <- glmmTMB::glmmTMB(
    count ~ (1 | persons),
    ziformula = ~ (1 | persons),
    offset = log(ID),
    data = fish,
    family = poisson()
  )
  m1 <- glmmTMB::glmmTMB(
    count ~ child + camper + (1 | persons),
    ziformula = ~ child + camper + (1 | persons),
    offset = log(ID),
    data = fish,
    family = poisson()
  )
  out <- null_model(m1)
  expect_equal(glmmTMB::fixef(out), glmmTMB::fixef(m0), tolerance = 1e-4)

  m0 <- glmmTMB::glmmTMB(
    count ~ (1 | persons),
    ziformula = ~1,
    offset = log(ID),
    data = fish,
    family = poisson()
  )
  m1 <- glmmTMB::glmmTMB(
    count ~ child + camper + (1 | persons),
    ziformula = ~ child + camper,
    offset = log(ID),
    data = fish,
    family = poisson()
  )
  out <- null_model(m1)
  expect_equal(glmmTMB::fixef(out), glmmTMB::fixef(m0), tolerance = 1e-4)
})


test_that("null_model warns for badly formulated response", {
  data(iris)
  model <- lm(iris[, 2] ~ Species, data = iris)
  expect_warning(null_model(model), regex = "Using indexed")
})


test_that("null_model, multinom, correct base-model with NA", {
  skip_on_cran()
  skip_if_not_installed("nnet")

  n_obs <- 1000
  softmax <- function(x) {
    exp(x - max(x)) / sum(exp(x - max(x)))
  }
  sample_y <- function(x) {
    sample(1:3, size = 1, prob = softmax(c(0.25 * x, -0.1 * x, 0 * x)))
  }
  set.seed(123)
  sim_df <- data.frame(x = rnorm(n_obs, 0, 1), y = NA)

  for (i in 1:nrow(sim_df)) {
    sim_df$y[i] <- sample_y(sim_df$x[i])
  }

  sim_df$x[1:500] <- NA
  sim_df2 <- sim_df[!is.na(sim_df$x), ]

  m1 <- nnet::multinom(y ~ x, data = sim_df, trace = FALSE)
  m2 <- nnet::multinom(y ~ x, data = sim_df2, trace = FALSE)

  out1 <- get_loglikelihood(null_model(m1))
  out2 <- get_loglikelihood(null_model(m1))
  expect_equal(out1, out2, tolerance = 1e-4, ignore_attr = TRUE)
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
