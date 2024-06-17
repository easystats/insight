skip_if(getRversion() > "4.3.3")
skip_if_not_installed("glmmTMB")
skip_if_not_installed("lme4")
skip_if_not_installed("TMB")
skip_if_not(getRversion() >= "4.0.0")


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
