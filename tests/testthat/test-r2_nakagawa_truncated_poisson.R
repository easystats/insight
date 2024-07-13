skip_on_cran()

skip_if_not_installed("glmmTMB")
skip_if_not_installed("MuMIn")
skip_if_not_installed("performance", minimum_version = "0.12.1")


# ==============================================================================
# neg-binomial1 zero-inflated mixed models, glmmTMB
# ==============================================================================

test_that("glmmTMB, truncated_poisson", {
  data(Salamanders, package = "glmmTMB")

  m <- glmmTMB::glmmTMB(count ~ spp + mined + (1 | site),
    ziformula = ~ spp + mined,
    family = glmmTMB::truncated_poisson(), data = glmmTMB::Salamanders
  )

  # truncated only works for full model
  expect_warning(performance::r2_nakagawa(m, model_component = "conditional"))

  # full model
  out <- performance::r2_nakagawa(m)
  expect_equal(out$R2_conditional, 0.6116589, tolerance = 1e-4, ignore_attr = TRUE)
  expect_equal(out$R2_marginal, 0.5411228, tolerance = 1e-4, ignore_attr = TRUE)

  out1 <- suppressWarnings(MuMIn::r.squaredGLMM(m))
  # matches delta values
  expect_equal(out1[1, "R2m"], out$R2_marginal, ignore_attr = TRUE, tolerance = 1e-4)
  expect_equal(out1[1, "R2c"], out$R2_conditional, ignore_attr = TRUE, tolerance = 1e-4)
})
