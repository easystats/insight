skip_on_cran()

skip_if_not_installed("glmmTMB")
skip_if_not_installed("MuMIn")
skip_if_not_installed("performance")


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
  out2 <- performance::r2_nakagawa(m)
  # matches theoretical values
  expect_equal(out1[2, "R2m"], out2$R2_marginal, ignore_attr = TRUE, tolerance = 1e-4)
  expect_equal(out1[2, "R2c"], out2$R2_conditional, ignore_attr = TRUE, tolerance = 1e-4)

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
  out2 <- performance::r2_nakagawa(m, tolerance = 1e-8)
  # we have slight differences here: MuMIn uses "var(fitted())" to exctract fixed
  # effects variances, while insight uses "var(beta %*% t(mm))". The latter gives
  # different values when random slopes are involved
  expect_equal(out1[2, "R2m"], out2$R2_marginal, ignore_attr = TRUE, tolerance = 1e-1)
  expect_equal(out1[2, "R2c"], out2$R2_conditional, ignore_attr = TRUE, tolerance = 1e-1)

  # glmmTMB, sqrt, random slope -------------------------------------------------
  m <- suppressWarnings(glmmTMB::glmmTMB(count ~ mined + cover + (1 + cover | site),
    ziformula = ~mined,
    family = poisson("sqrt"), data = Salamanders
  ))
  out1 <- suppressWarnings(MuMIn::r.squaredGLMM(m))
  out2 <- performance::r2_nakagawa(m, tolerance = 1e-8)
  # matches delta values
  expect_equal(out1[1, "R2m"], out2$R2_marginal, ignore_attr = TRUE, tolerance = 1e-4)
  expect_equal(out1[1, "R2c"], out2$R2_conditional, ignore_attr = TRUE, tolerance = 1e-4)
})
