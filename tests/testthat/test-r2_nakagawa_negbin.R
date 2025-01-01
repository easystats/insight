skip_on_cran()

skip_if_not_installed("glmmTMB")
skip_if_not_installed("MuMIn")
skip_if_not_installed("lme4")
skip_if_not_installed("performance", minimum_version = "0.12.1")


# ==============================================================================
# neg-binomial mixed models, lme4 ----
# ==============================================================================

test_that("glmer, negbin", {
  # dataset ---------------------------------
  data(Salamanders, package = "glmmTMB")

  # no random slopes
  m <- lme4::glmer.nb(
    count ~ mined + spp + (1 | site),
    data = Salamanders
  )
  out1 <- suppressWarnings(MuMIn::r.squaredGLMM(m))
  out2 <- performance::r2_nakagawa(m)
  # matches theoretical values
  expect_equal(out1[2, "R2m"], out2$R2_marginal, ignore_attr = TRUE, tolerance = 1e-4)
  expect_equal(out1[2, "R2c"], out2$R2_conditional, ignore_attr = TRUE, tolerance = 1e-4)
})


# ==============================================================================
# neg-binomial1 mixed models, glmmTMB ----
# ==============================================================================

test_that("glmmTMB, Nbinom1", {
  # we skip this test for now, because MuMIn might use a wrong computation
  # of the approximation here. See discussion in #877 for details
  skip_if(TRUE)

  # dataset ---------------------------------
  data(Salamanders, package = "glmmTMB")

  # no random slopes
  m <- glmmTMB::glmmTMB(
    count ~ mined + spp + (1 | site),
    family = glmmTMB::nbinom1(),
    data = Salamanders
  )
  out1 <- suppressWarnings(MuMIn::r.squaredGLMM(m))
  out2 <- performance::r2_nakagawa(m)
  # matches theoretical values
  expect_equal(out1[2, "R2m"], out2$R2_marginal, ignore_attr = TRUE, tolerance = 1e-4)
  expect_equal(out1[2, "R2c"], out2$R2_conditional, ignore_attr = TRUE, tolerance = 1e-4)

  # with random slopes
  m <- suppressWarnings(glmmTMB::glmmTMB(
    count ~ mined + spp + cover + (1 + cover | site),
    family = glmmTMB::nbinom1(),
    data = Salamanders
  ))
  out1 <- suppressWarnings(MuMIn::r.squaredGLMM(m))
  out2 <- performance::r2_nakagawa(m, tolerance = 1e-8)
  # matches theoretical values
  expect_equal(out1[2, "R2m"], out2$R2_marginal, ignore_attr = TRUE, tolerance = 1e-1)
  expect_equal(out1[2, "R2c"], out2$R2_conditional, ignore_attr = TRUE, tolerance = 1e-1)

  # no random slopes, sqrt
  m <- glmmTMB::glmmTMB(
    count ~ mined + spp + (1 | site),
    family = glmmTMB::nbinom1("sqrt"),
    data = Salamanders
  )
  out1 <- suppressWarnings(MuMIn::r.squaredGLMM(m))
  out2 <- performance::r2_nakagawa(m)
  # matches delta values
  expect_equal(out1[1, "R2m"], out2$R2_marginal, ignore_attr = TRUE, tolerance = 1e-4)
  expect_equal(out1[1, "R2c"], out2$R2_conditional, ignore_attr = TRUE, tolerance = 1e-4)
})


# ==============================================================================
# Validate against Nakagawa et al. 2017 paper!
test_that("glmmTMB, Nbinom1", {
  data(Salamanders, package = "glmmTMB")
  glmmTMBr <- glmmTMB::glmmTMB(
    count ~ (1 | site),
    family = glmmTMB::nbinom1(),
    data = Salamanders, REML = TRUE
  )
  glmmTMBf <- glmmTMB::glmmTMB(
    count ~ mined + spp + (1 | site),
    family = glmmTMB::nbinom1(),
    data = Salamanders, REML = TRUE
  )
  # Calculation based on Supplement 2 of Nakagawa et al. 2017
  VarF <- var(as.vector(get_modelmatrix(glmmTMBf) %*% glmmTMB::fixef(glmmTMBf)$cond))
  # this is "mu" in insight
  lambda <- as.numeric(exp(glmmTMB::fixef(glmmTMBr)$cond + 0.5 * (as.numeric(glmmTMB::VarCorr(glmmTMBr)$cond[1]))))
  # this is "sig" in insight
  thetaF <- sigma(glmmTMBf) # note that theta is called alpha in glmmadmb
  # this is what ".variance_distributional()" returns
  VarOdF <- 1 / lambda + 1 / thetaF # the delta method
  VarOlF <- log(1 + (1 / lambda) + (1 / thetaF)) # log-normal approximation
  VarOtF <- trigamma((1 / lambda + 1 / thetaF)^-1) # trigamma function

  # lognormal
  R2glmmM <- VarF / (VarF + sum(as.numeric(glmmTMB::VarCorr(glmmTMBf)$cond)) + VarOlF)
  R2glmmC <- (VarF + sum(as.numeric(glmmTMB::VarCorr(glmmTMBf)$cond))) / (VarF + sum(as.numeric(glmmTMB::VarCorr(glmmTMBf)$cond)) + VarOlF)
  out <- performance::r2_nakagawa(glmmTMBf, null_model = glmmTMBr)
  expect_equal(out$R2_conditional, R2glmmC, tolerance = 1e-4, ignore_attr = TRUE)
  expect_equal(out$R2_marginal, R2glmmM, tolerance = 1e-4, ignore_attr = TRUE)

  # delta
  R2glmmM <- VarF / (VarF + sum(as.numeric(glmmTMB::VarCorr(glmmTMBf)$cond)) + VarOdF)
  R2glmmC <- (VarF + sum(as.numeric(glmmTMB::VarCorr(glmmTMBf)$cond))) / (VarF + sum(as.numeric(glmmTMB::VarCorr(glmmTMBf)$cond)) + VarOdF)
  out <- performance::r2_nakagawa(glmmTMBf, null_model = glmmTMBr, approximation = "delta")
  expect_equal(out$R2_conditional, R2glmmC, tolerance = 1e-4, ignore_attr = TRUE)
  expect_equal(out$R2_marginal, R2glmmM, tolerance = 1e-4, ignore_attr = TRUE)

  # trigamma
  R2glmmM <- VarF / (VarF + sum(as.numeric(glmmTMB::VarCorr(glmmTMBf)$cond)) + VarOtF)
  R2glmmC <- (VarF + sum(as.numeric(glmmTMB::VarCorr(glmmTMBf)$cond))) / (VarF + sum(as.numeric(glmmTMB::VarCorr(glmmTMBf)$cond)) + VarOtF)
  out <- performance::r2_nakagawa(glmmTMBf, null_model = glmmTMBr, approximation = "trigamma")
  expect_equal(out$R2_conditional, R2glmmC, tolerance = 1e-4, ignore_attr = TRUE)
  expect_equal(out$R2_marginal, R2glmmM, tolerance = 1e-4, ignore_attr = TRUE)
})


# ==============================================================================
# neg-binomial2 mixed models, glmmTMB
# ==============================================================================

test_that("glmmTMB, Nbinom2", {
  # dataset ---------------------------------
  data(Salamanders, package = "glmmTMB")

  # no random slopes
  m <- glmmTMB::glmmTMB(
    count ~ mined + spp + (1 | site),
    family = glmmTMB::nbinom2(),
    data = Salamanders
  )
  out1 <- suppressWarnings(MuMIn::r.squaredGLMM(m))
  out2 <- performance::r2_nakagawa(m)
  # matches theoretical values
  expect_equal(out1[2, "R2m"], out2$R2_marginal, ignore_attr = TRUE, tolerance = 1e-4)
  expect_equal(out1[2, "R2c"], out2$R2_conditional, ignore_attr = TRUE, tolerance = 1e-4)

  # with random slopes
  m <- suppressWarnings(glmmTMB::glmmTMB(
    count ~ mined + spp + cover + (1 + cover | site),
    family = glmmTMB::nbinom2(),
    data = Salamanders
  ))
  out1 <- suppressWarnings(MuMIn::r.squaredGLMM(m))
  out2 <- performance::r2_nakagawa(m, tolerance = 1e-8)
  # matches theoretical values
  expect_equal(out1[2, "R2m"], out2$R2_marginal, ignore_attr = TRUE, tolerance = 1e-1)
  expect_equal(out1[2, "R2c"], out2$R2_conditional, ignore_attr = TRUE, tolerance = 1e-1)
})


# ==============================================================================
# Validate against Nakagawa et al. 2017 paper!
test_that("glmmTMB, Nbinom2", {
  data(Salamanders, package = "glmmTMB")
  glmmTMBr <- glmmTMB::glmmTMB(
    count ~ (1 | site),
    family = glmmTMB::nbinom2(),
    data = Salamanders, REML = TRUE
  )
  glmmTMBf <- glmmTMB::glmmTMB(
    count ~ mined + spp + (1 | site),
    family = glmmTMB::nbinom2(),
    data = Salamanders, REML = TRUE
  )
  # Calculation based on Supplement 2 of Nakagawa et al. 2017
  VarF <- var(as.vector(get_modelmatrix(glmmTMBf) %*% glmmTMB::fixef(glmmTMBf)$cond))
  # this is "mu" in insight
  lambda <- as.numeric(exp(glmmTMB::fixef(glmmTMBr)$cond + 0.5 * (as.numeric(glmmTMB::VarCorr(glmmTMBr)$cond[1]))))
  # this is "sig" in insight
  thetaF <- sigma(glmmTMBf) # note that theta is called alpha in glmmadmb
  # this is what ".variance_distributional()" returns
  VarOdF <- 1 / lambda + 1 / thetaF # the delta method
  VarOlF <- log(1 + (1 / lambda) + (1 / thetaF)) # log-normal approximation
  VarOtF <- trigamma((1 / lambda + 1 / thetaF)^-1) # trigamma function

  # lognormal
  R2glmmM <- VarF / (VarF + sum(as.numeric(glmmTMB::VarCorr(glmmTMBf)$cond)) + VarOlF)
  R2glmmC <- (VarF + sum(as.numeric(glmmTMB::VarCorr(glmmTMBf)$cond))) / (VarF + sum(as.numeric(glmmTMB::VarCorr(glmmTMBf)$cond)) + VarOlF)
  out <- performance::r2_nakagawa(glmmTMBf, null_model = glmmTMBr)
  expect_equal(out$R2_conditional, R2glmmC, tolerance = 1e-4, ignore_attr = TRUE)
  expect_equal(out$R2_marginal, R2glmmM, tolerance = 1e-4, ignore_attr = TRUE)

  # delta
  R2glmmM <- VarF / (VarF + sum(as.numeric(glmmTMB::VarCorr(glmmTMBf)$cond)) + VarOdF)
  R2glmmC <- (VarF + sum(as.numeric(glmmTMB::VarCorr(glmmTMBf)$cond))) / (VarF + sum(as.numeric(glmmTMB::VarCorr(glmmTMBf)$cond)) + VarOdF)
  out <- performance::r2_nakagawa(glmmTMBf, null_model = glmmTMBr, approximation = "delta")
  expect_equal(out$R2_conditional, R2glmmC, tolerance = 1e-4, ignore_attr = TRUE)
  expect_equal(out$R2_marginal, R2glmmM, tolerance = 1e-4, ignore_attr = TRUE)

  # trigamma
  R2glmmM <- VarF / (VarF + sum(as.numeric(glmmTMB::VarCorr(glmmTMBf)$cond)) + VarOtF)
  R2glmmC <- (VarF + sum(as.numeric(glmmTMB::VarCorr(glmmTMBf)$cond))) / (VarF + sum(as.numeric(glmmTMB::VarCorr(glmmTMBf)$cond)) + VarOtF)
  out <- performance::r2_nakagawa(glmmTMBf, null_model = glmmTMBr, approximation = "trigamma")
  expect_equal(out$R2_conditional, R2glmmC, tolerance = 1e-4, ignore_attr = TRUE)
  expect_equal(out$R2_marginal, R2glmmM, tolerance = 1e-4, ignore_attr = TRUE)
})
