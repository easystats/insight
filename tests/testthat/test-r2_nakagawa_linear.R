skip_on_cran()

skip_if_not_installed("glmmTMB")
skip_if_not_installed("MuMIn")
skip_if_not_installed("lme4")
skip_if_not_installed("performance", minimum_version = "0.12.1")


# ==============================================================================
# linear mixed models, glmmTMB ----
# ==============================================================================

test_that("glmmTMB, linear", {
  data(sleepstudy, package = "lme4")

  # no random effects
  m1 <- glmmTMB::glmmTMB(Reaction ~ Days, data = sleepstudy)
  m2 <- lm(Reaction ~ Days, data = sleepstudy)
  out1 <- performance::r2(m1)
  out2 <- performance::r2(m2)
  expect_equal(out1$R2, out2$R2, tolerance = 1e-4)
  expect_equal(out1$R2_adjusted, out2$R2_adjusted, tolerance = 1e-1)

  # linear, no random slope
  m <- glmmTMB::glmmTMB(Reaction ~ Days + (1 | Subject), data = sleepstudy)
  out1 <- suppressWarnings(MuMIn::r.squaredGLMM(m))
  out2 <- performance::r2_nakagawa(m)
  expect_equal(out1[, "R2m"], out2$R2_marginal, ignore_attr = TRUE, tolerance = 1e-4)
  expect_equal(out1[, "R2c"], out2$R2_conditional, ignore_attr = TRUE, tolerance = 1e-4)

  # linear, no random slope, inverse
  m <- suppressWarnings(glmmTMB::glmmTMB(
    Reaction ~ Days + (1 | Subject),
    data = sleepstudy,
    family = gaussian("inverse")
  ))
  out1 <- suppressWarnings(MuMIn::r.squaredGLMM(m))
  out2 <- suppressWarnings(performance::r2_nakagawa(m))
  expect_equal(out1[, "R2m"], out2$R2_marginal, ignore_attr = TRUE, tolerance = 1e-4)
  expect_equal(out1[, "R2c"], out2$R2_conditional, ignore_attr = TRUE, tolerance = 1e-4)

  # linear, with random slope
  m <- glmmTMB::glmmTMB(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)
  out1 <- suppressWarnings(MuMIn::r.squaredGLMM(m))
  out2 <- performance::r2_nakagawa(m)
  expect_equal(out1[, "R2m"], out2$R2_marginal, ignore_attr = TRUE, tolerance = 1e-4)
  expect_equal(out1[, "R2c"], out2$R2_conditional, ignore_attr = TRUE, tolerance = 1e-4)

  # linear, random slope, inverse
  m <- suppressWarnings(glmmTMB::glmmTMB(
    Reaction ~ Days + (1 + Days | Subject),
    data = sleepstudy,
    family = gaussian("inverse")
  ))
  out1 <- suppressWarnings(MuMIn::r.squaredGLMM(m))
  out2 <- suppressWarnings(performance::r2_nakagawa(m))
  expect_equal(out1[, "R2m"], out2$R2_marginal, ignore_attr = TRUE, tolerance = 1e-4)
  expect_equal(out1[, "R2c"], out2$R2_conditional, ignore_attr = TRUE, tolerance = 1e-4)

  # linear, random slope, log
  m <- glmmTMB::glmmTMB(
    Reaction ~ Days + (1 + Days | Subject),
    data = sleepstudy,
    family = gaussian("log")
  )
  out1 <- suppressWarnings(MuMIn::r.squaredGLMM(m))
  out2 <- performance::r2_nakagawa(m)
  expect_equal(out1[, "R2m"], out2$R2_marginal, ignore_attr = TRUE, tolerance = 1e-4)
  expect_equal(out1[, "R2c"], out2$R2_conditional, ignore_attr = TRUE, tolerance = 1e-4)
})


# ==============================================================================
# linear mixed models, lme4 ----
# ==============================================================================

test_that("lme4, linear", {
  data(sleepstudy, package = "lme4")

  # linear, no random slope
  m <- lme4::lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy)
  out1 <- suppressWarnings(MuMIn::r.squaredGLMM(m))
  out2 <- performance::r2_nakagawa(m)
  expect_equal(out1[, "R2m"], out2$R2_marginal, ignore_attr = TRUE, tolerance = 1e-4)
  expect_equal(out1[, "R2c"], out2$R2_conditional, ignore_attr = TRUE, tolerance = 1e-4)

  # linear, with random slope
  m <- lme4::lmer(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)
  out1 <- suppressWarnings(MuMIn::r.squaredGLMM(m))
  out2 <- performance::r2_nakagawa(m)
  expect_equal(out1[, "R2m"], out2$R2_marginal, ignore_attr = TRUE, tolerance = 1e-4)
  expect_equal(out1[, "R2c"], out2$R2_conditional, ignore_attr = TRUE, tolerance = 1e-4)
})
