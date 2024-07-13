skip_on_cran()

skip_if_not_installed("glmmTMB", minimum_version = "1.1.8")
skip_if_not_installed("performance", minimum_version = "0.12.1")
skip_if_not_installed("withr")


# ==============================================================================
# ordered beta mixed models, glmmTMB
# ==============================================================================

withr::with_environment(
  new.env(),
  test_that("glmmTMB, ordbeta", {
    # dataset ---------------------------------
    skip_if_not_installed("datawizard")
    skip_if_not_installed("lme4")
    data(sleepstudy, package = "lme4")
    sleepstudy$y <- datawizard::normalize(sleepstudy$Reaction)
    m <- glmmTMB::glmmTMB(
      y ~ Days + (Days | Subject),
      data = sleepstudy,
      family = glmmTMB::ordbeta()
    )
    mnull <- glmmTMB::glmmTMB(
      y ~ 1 + (Days | Subject),
      data = sleepstudy,
      family = glmmTMB::ordbeta()
    )
    out <- suppressWarnings(performance::r2_nakagawa(m, null_model = mnull, verbose = FALSE))
    expect_equal(out$R2_marginal, 0.2799715, ignore_attr = TRUE, tolerance = 1e-4)
    expect_equal(out$R2_conditional, 0.8504158, ignore_attr = TRUE, tolerance = 1e-4)
  })
)
