skip_on_cran()

skip_if_not_installed("glmmTMB")
skip_if_not_installed("MuMIn")
skip_if_not_installed("lme4")
skip_if_not_installed("performance", minimum_version = "0.12.1")


# ==============================================================================
# Poisson mixed models, glmmTMB ----
# ==============================================================================

test_that("glmmTMB, Poisson", {
  # dataset ---------------------------------
  data(Salamanders, package = "glmmTMB")

  # glmmTMB, no random slope -------------------------------------------------
  m <- glmmTMB::glmmTMB(count ~ mined + (1 | site),
    family = poisson(), data = Salamanders
  )
  out1 <- suppressWarnings(MuMIn::r.squaredGLMM(m))
  out2 <- performance::r2_nakagawa(m)
  # matches theoretical values
  expect_equal(out1[2, "R2m"], out2$R2_marginal, ignore_attr = TRUE, tolerance = 1e-4)
  expect_equal(out1[2, "R2c"], out2$R2_conditional, ignore_attr = TRUE, tolerance = 1e-4)

  # glmmTMB, sqrt, no random slope -------------------------------------------------
  m <- glmmTMB::glmmTMB(count ~ mined + (1 | site),
    family = poisson("sqrt"), data = Salamanders
  )
  out1 <- suppressWarnings(MuMIn::r.squaredGLMM(m))
  out2 <- performance::r2_nakagawa(m)
  # matches delta values
  expect_equal(out1[1, "R2m"], out2$R2_marginal, ignore_attr = TRUE, tolerance = 1e-4)
  expect_equal(out1[1, "R2c"], out2$R2_conditional, ignore_attr = TRUE, tolerance = 1e-4)

  # glmmTMB, random slope -------------------------------------------------
  m <- suppressWarnings(glmmTMB::glmmTMB(count ~ mined + cover + (1 + cover | site),
    family = poisson(), data = Salamanders
  ))
  out1 <- suppressWarnings(MuMIn::r.squaredGLMM(m))
  out2 <- performance::r2_nakagawa(m, tolerance = 1e-8)
  # we have slight differences here: MuMIn uses "var(fitted())" to exctract fixed
  # effects variances, while insight uses "var(beta %*% t(mm))". The latter gives
  # different values when random slopes are involved
  expect_equal(out1[2, "R2m"], out2$R2_marginal, ignore_attr = TRUE, tolerance = 1e-1)
  expect_equal(out1[2, "R2c"], out2$R2_conditional, ignore_attr = TRUE, tolerance = 1e-1)

  # glmmTMB, sqrt, random slope -------------------------------------------------
  m <- suppressWarnings(glmmTMB::glmmTMB(count ~ mined + cover + (1 + cover | site),
    family = poisson("sqrt"), data = Salamanders
  ))
  out1 <- suppressWarnings(MuMIn::r.squaredGLMM(m))
  out2 <- performance::r2_nakagawa(m, tolerance = 1e-8)
  # matches delta values
  expect_equal(out1[1, "R2m"], out2$R2_marginal, ignore_attr = TRUE, tolerance = 1e-4)
  expect_equal(out1[1, "R2c"], out2$R2_conditional, ignore_attr = TRUE, tolerance = 1e-4)
})


# ==============================================================================
# Validate against Nakagawa et al. 2017 paper!
test_that("glmmTMB, Poisson, Nakagawa", {
  # dataset ---------------------------------
  data(Salamanders, package = "glmmTMB")

  # glmmTMB, no random slope -------------------------------------------------
  fecmodADMBr <- glmmTMB::glmmTMB(count ~ (1 | site),
    family = poisson(), data = Salamanders
  )
  fecmodADMBf <- glmmTMB::glmmTMB(count ~ mined + (1 | site),
    family = poisson(), data = Salamanders
  )
  VarF <- var(as.vector(model.matrix(fecmodADMBf) %*% glmmTMB::fixef(fecmodADMBf)$cond))
  lambda <- as.numeric(exp(glmmTMB::fixef(fecmodADMBr)$cond + 0.5 * (as.numeric(glmmTMB::VarCorr(fecmodADMBr)$cond))))
  omegaF <- sigma(fecmodADMBf) # overdispersion omega is alpha in glmmadmb
  VarOdF <- omegaF / lambda # the delta method
  VarOlF <- log(1 + omegaF / lambda) # log-normal approximation
  VarOtF <- trigamma(lambda / omegaF) # trigamma function

  # lognormal
  R2glmmM <- VarF / (VarF + sum(as.numeric(glmmTMB::VarCorr(fecmodADMBf)$cond)) + VarOlF)
  R2glmmC <- (VarF + sum(as.numeric(glmmTMB::VarCorr(fecmodADMBf)$cond))) / (VarF + sum(as.numeric(glmmTMB::VarCorr(fecmodADMBf)$cond)) + VarOlF)
  out <- performance::r2_nakagawa(fecmodADMBf)
  expect_equal(out$R2_conditional, R2glmmC, tolerance = 1e-4, ignore_attr = TRUE)
  expect_equal(out$R2_marginal, R2glmmM, tolerance = 1e-4, ignore_attr = TRUE)

  # delta
  R2glmmM <- VarF / (VarF + sum(as.numeric(glmmTMB::VarCorr(fecmodADMBf)$cond)) + VarOdF)
  R2glmmC <- (VarF + sum(as.numeric(glmmTMB::VarCorr(fecmodADMBf)$cond))) / (VarF + sum(as.numeric(glmmTMB::VarCorr(fecmodADMBf)$cond)) + VarOdF)
  out <- performance::r2_nakagawa(fecmodADMBf, approximation = "delta")
  expect_equal(out$R2_conditional, R2glmmC, tolerance = 1e-4, ignore_attr = TRUE)
  expect_equal(out$R2_marginal, R2glmmM, tolerance = 1e-4, ignore_attr = TRUE)

  # trigamma
  R2glmmM <- VarF / (VarF + sum(as.numeric(glmmTMB::VarCorr(fecmodADMBf)$cond)) + VarOtF)
  R2glmmC <- (VarF + sum(as.numeric(glmmTMB::VarCorr(fecmodADMBf)$cond))) / (VarF + sum(as.numeric(glmmTMB::VarCorr(fecmodADMBf)$cond)) + VarOtF)
  out <- performance::r2_nakagawa(fecmodADMBf, approximation = "trigamma")
  expect_equal(out$R2_conditional, R2glmmC, tolerance = 1e-4, ignore_attr = TRUE)
  expect_equal(out$R2_marginal, R2glmmM, tolerance = 1e-4, ignore_attr = TRUE)
})


# ==============================================================================
# Poisson mixed models, lme4 ----
# ==============================================================================

test_that("lme4, Poisson", {
  # dataset ---------------------------------
  data(Salamanders, package = "glmmTMB")

  # lme4, no random slope -------------------------------------------------
  m <- lme4::glmer(count ~ mined + (1 | site),
    family = poisson(), data = Salamanders
  )
  out1 <- suppressWarnings(MuMIn::r.squaredGLMM(m))
  out2 <- performance::r2_nakagawa(m)
  # matches theoretical values
  expect_equal(out1[2, "R2m"], out2$R2_marginal, ignore_attr = TRUE, tolerance = 1e-4)
  expect_equal(out1[2, "R2c"], out2$R2_conditional, ignore_attr = TRUE, tolerance = 1e-4)

  # lme4, sqrt, no random slope -------------------------------------------------
  m <- lme4::glmer(count ~ mined + (1 | site),
    family = poisson("sqrt"), data = Salamanders
  )
  out1 <- suppressWarnings(MuMIn::r.squaredGLMM(m))
  out2 <- performance::r2_nakagawa(m)
  # matches theoretical values
  expect_equal(out1[1, "R2m"], out2$R2_marginal, ignore_attr = TRUE, tolerance = 1e-4)
  expect_equal(out1[1, "R2c"], out2$R2_conditional, ignore_attr = TRUE, tolerance = 1e-4)
})
