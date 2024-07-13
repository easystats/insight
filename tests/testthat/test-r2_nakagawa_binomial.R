skip_on_cran()

skip_if_not_installed("glmmTMB")
skip_if_not_installed("MuMIn")
skip_if_not_installed("lme4")
skip_if_not_installed("performance", minimum_version = "0.12.1")


# ==============================================================================
# Binomial mixed models, lme4 ----
# ==============================================================================

test_that("lme4, binomial", {
  # dataset
  data(cbpp, package = "lme4")

  # lme4, no random slope ----------------------------------------------------
  m <- lme4::glmer(
    cbind(incidence, size - incidence) ~ period + (1 | herd),
    data = cbpp,
    family = binomial()
  )
  out1 <- suppressWarnings(MuMIn::r.squaredGLMM(m))
  out2 <- performance::r2_nakagawa(m)
  # matches theoretical values
  expect_equal(out1[1, "R2m"], out2$R2_marginal, ignore_attr = TRUE, tolerance = 1e-3)
  expect_equal(out1[1, "R2c"], out2$R2_conditional, ignore_attr = TRUE, tolerance = 1e-3)
})


# ==============================================================================
# Binomial mixed models, glmmTMB ----
# ==============================================================================

test_that("glmmTMB, binomial", {
  # dataset
  data(cbpp, package = "lme4")

  # lme4, no random slope ----------------------------------------------------
  m <- glmmTMB::glmmTMB(
    cbind(incidence, size - incidence) ~ period + (1 | herd),
    data = cbpp,
    family = binomial()
  )
  out1 <- suppressWarnings(MuMIn::r.squaredGLMM(m))
  out2 <- performance::r2_nakagawa(m)
  # matches theoretical values
  expect_equal(out1[1, "R2m"], out2$R2_marginal, ignore_attr = TRUE, tolerance = 1e-3)
  expect_equal(out1[1, "R2c"], out2$R2_conditional, ignore_attr = TRUE, tolerance = 1e-3)
})
