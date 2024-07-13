skip_on_cran()

skip_if_not_installed("glmmTMB")
skip_if_not_installed("MuMIn")
skip_if_not_installed("performance", minimum_version = "0.12.1")


# ==============================================================================
# Poisson zero-inflated mixed models, glmmTMB
# ==============================================================================

test_that("glmmTMB, Poisson zero-inflated", {
  # dataset ---------------------------------
  data(Salamanders, package = "glmmTMB")

  # glmmTMB, no random slope -------------------------------------------------
  m <- glmmTMB::glmmTMB(count ~ mined + (1 | site),
    ziformula = ~mined,
    family = poisson(), data = Salamanders
  )
  out1 <- suppressWarnings(MuMIn::r.squaredGLMM(m))
  out2 <- performance::r2_nakagawa(m, model_component = "conditional", verbose = FALSE)
  # matches theoretical values
  expect_equal(out2$R2_marginal, 0.4636197, ignore_attr = TRUE, tolerance = 1e-4)
  expect_equal(out2$R2_conditional, 0.5751936, ignore_attr = TRUE, tolerance = 1e-4)
  expect_equal(out1[2, "R2m"], out2$R2_marginal, ignore_attr = TRUE, tolerance = 1e-1)
  expect_equal(out1[2, "R2c"], out2$R2_conditional, ignore_attr = TRUE, tolerance = 1e-1)

  # full model
  out <- performance::r2_nakagawa(m)
  expect_equal(out$R2_marginal, 0.2923215, ignore_attr = TRUE, tolerance = 1e-4)
  expect_equal(out$R2_conditional, 0.362671, ignore_attr = TRUE, tolerance = 1e-4)

  # glmmTMB, sqrt, no random slope -------------------------------------------------
  m <- glmmTMB::glmmTMB(count ~ mined + (1 | site),
    ziformula = ~mined,
    family = poisson("sqrt"), data = Salamanders
  )
  out1 <- suppressWarnings(MuMIn::r.squaredGLMM(m))
  out2 <- performance::r2_nakagawa(m)
  # matches delta values
  expect_equal(out1[1, "R2m"], out2$R2_marginal, ignore_attr = TRUE, tolerance = 1e-4)
  expect_equal(out1[1, "R2c"], out2$R2_conditional, ignore_attr = TRUE, tolerance = 1e-4)

  # glmmTMB, random slope -------------------------------------------------
  m <- suppressWarnings(glmmTMB::glmmTMB(count ~ mined + cover + (1 + cover | site),
    ziformula = ~mined,
    family = poisson(), data = Salamanders
  ))
  out1 <- suppressWarnings(MuMIn::r.squaredGLMM(m))
  out2 <- suppressWarnings(performance::r2_nakagawa(m, tolerance = 1e-8, model_component = "conditional", verbose = FALSE))
  # we have slight differences here: MuMIn uses "var(fitted())" to exctract fixed
  # effects variances, while insight uses "var(beta %*% t(mm))". The latter gives
  # different values when random slopes are involved
  # expect_equal(out1[2, "R2m"], out2$R2_marginal, ignore_attr = TRUE, tolerance = 1e-1)
  # expect_equal(out1[2, "R2c"], out2$R2_conditional, ignore_attr = TRUE, tolerance = 1e-1)
  expect_equal(out2$R2_marginal, 0.524714, ignore_attr = TRUE, tolerance = 1e-1)
  expect_equal(out2$R2_conditional, 0.6498465, ignore_attr = TRUE, tolerance = 1e-1)

  # full model
  out <- suppressWarnings(performance::r2_nakagawa(m))
  expect_equal(out$R2_marginal, 0.3344432, ignore_attr = TRUE, tolerance = 1e-4)
  expect_equal(out$R2_conditional, 0.4142003, ignore_attr = TRUE, tolerance = 1e-4)


  # glmmTMB, sqrt, random slope -------------------------------------------------
  m <- suppressWarnings(glmmTMB::glmmTMB(count ~ mined + cover + (1 + cover | site),
    ziformula = ~mined,
    family = poisson("sqrt"), data = Salamanders
  ))
  out1 <- suppressWarnings(MuMIn::r.squaredGLMM(m))
  out2 <- suppressWarnings(performance::r2_nakagawa(m, tolerance = 1e-8))
  # matches delta values
  expect_equal(out1[1, "R2m"], out2$R2_marginal, ignore_attr = TRUE, tolerance = 1e-4)
  expect_equal(out1[1, "R2c"], out2$R2_conditional, ignore_attr = TRUE, tolerance = 1e-4)
})
