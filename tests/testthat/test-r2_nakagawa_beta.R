skip_on_cran()

skip_if_not_installed("glmmTMB")
skip_if_not_installed("MuMIn")
skip_if_not_installed("performance", minimum_version = "0.12.1")


# ==============================================================================
# beta mixed models, glmmTMB
# ==============================================================================

skip_if_not_installed("betareg")

test_that("glmmTMB, beta_family", {
  # dataset ---------------------------------
  data(FoodExpenditure, package = "betareg")
  m <- glmmTMB::glmmTMB(
    I(food / income) ~ income + (1 | persons),
    data = FoodExpenditure,
    family = glmmTMB::beta_family()
  )
  out1 <- suppressWarnings(MuMIn::r.squaredGLMM(m))
  out2 <- suppressWarnings(performance::r2_nakagawa(m, verbose = FALSE))
  # matches theoretical values
  expect_equal(out1[1, "R2m"], out2$R2_marginal, ignore_attr = TRUE, tolerance = 1e-4)
  expect_equal(out1[1, "R2c"], out2$R2_conditional, ignore_attr = TRUE, tolerance = 1e-4)
})
