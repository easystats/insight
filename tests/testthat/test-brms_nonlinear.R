skip_on_cran()
skip_if_not_installed("brms")
skip_if_not_installed("BH")
skip_if_not_installed("RcppEigen")

# non-linear model (`nl = TRUE`), adapted from #1076
m <- suppressWarnings(suppressMessages(brms::brm(
  brms::bf(
    cum ~ ult * (1 - exp(-(dev / theta)^omega)),
    ult ~ 1 + (1 | AY),
    omega ~ 1,
    theta ~ 1 + (1 | AY),
    sigma ~ dev,
    nl = TRUE
  ),
  data = brms::loss,
  family = gaussian(),
  prior = c(
    brms::prior(normal(5000, 1000), nlpar = "ult"),
    brms::prior(normal(1, 2), nlpar = "omega"),
    brms::prior(normal(45, 10), nlpar = "theta")
  ),
  chains = 1,
  iter = 500,
  seed = 123,
  refresh = 0
)))

nl_fixed <- c("b_ult_Intercept", "b_omega_Intercept", "b_theta_Intercept")


test_that("find_parameters, group-level terms of non-linear parameters are random, effects = 'full'", {
  out <- find_parameters(m, effects = "full")
  expect_identical(out$conditional, nl_fixed)
  expect_setequal(
    out$random,
    c(
      sprintf("r_AY__ult[%i,Intercept]", 1991:2000),
      sprintf("r_AY__theta[%i,Intercept]", 1991:2000),
      "sd_AY__ult_Intercept",
      "sd_AY__theta_Intercept"
    )
  )
  expect_identical(out$sigma, c("b_sigma_Intercept", "b_sigma_dev"))
})


test_that("find_auxiliary, non-linear parameters of mu are not auxiliary", {
  expect_identical(find_auxiliary(m), "sigma")
})


test_that("find_predictors, group-level terms of non-linear parameters, effects = 'random'", {
  out <- find_predictors(m, effects = "random")
  expect_identical(out$ult_random, "AY")
  expect_identical(out$theta_random, "AY")
})


test_that("get_parameters, conditional component holds all non-linear coefficients", {
  out <- get_parameters(m, effects = "fixed", component = "conditional")
  expect_named(out, nl_fixed)
})


test_that("get_parameters, empty selection returns NULL (component with no parameters)", {
  expect_null(get_parameters(m, component = "zi"))
  expect_null(get_parameters(m, component = "zi", summary = TRUE))
})


test_that("clean_parameters, non-linear parameters have brms-summary labels", {
  out <- clean_parameters(m)
  fixed_rows <- match(nl_fixed, out$Parameter)
  expect_identical(
    out$Cleaned_Parameter[fixed_rows],
    c("ult_Intercept", "omega_Intercept", "theta_Intercept")
  )
})


test_that("clean_parameters, no two rows share the same labels", {
  out <- clean_parameters(m)
  labels <- out[c("Effects", "Component", "Group", "Cleaned_Parameter")]
  expect_false(anyDuplicated(labels) > 0)
})
