skip_on_cran()
skip_if_not_installed("brms")
skip_if_not_installed("BH")
skip_if_not_installed("RcppEigen")

# Univariate non-linear model (`nl = TRUE`), adapted from #1076. The
# non-linear parameters `ult`, `omega` and `theta` are parameters of `mu`,
# `ult` and `theta` have group-level terms, and `sigma` is modelled as a
# distributional parameter.
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
nl_elements <- c("ult", "omega", "theta", "ult_random", "theta_random")


test_that("find_parameters, non-linear coefficients are conditional, default effects", {
  out <- find_parameters(m)
  expect_identical(out$conditional, nl_fixed)
  expect_identical(out$sigma, c("b_sigma_Intercept", "b_sigma_dev"))
  expect_false(any(nl_elements %in% names(out)))
})


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
  expect_false(any(nl_elements %in% names(out)))
})


test_that("find_auxiliary, non-linear parameters of mu are not auxiliary", {
  expect_identical(find_auxiliary(m), "sigma")
})


test_that("find_predictors, group-level terms of non-linear parameters, effects = 'random'", {
  # the formulas of the non-linear parameters are still separate elements
  out <- find_predictors(m, effects = "random")
  expect_identical(out$ult_random, "AY")
  expect_identical(out$theta_random, "AY")
})


test_that("get_parameters, select one non-linear coefficient by name", {
  out <- get_parameters(
    m,
    effects = "fixed",
    component = "conditional",
    parameters = "b_ult_Intercept"
  )
  expect_s3_class(out, "data.frame")
  expect_named(out, "b_ult_Intercept")
})


test_that("get_parameters, conditional component holds all non-linear coefficients", {
  out <- get_parameters(m, effects = "fixed", component = "conditional")
  expect_s3_class(out, "data.frame")
  expect_named(out, nl_fixed)
})


test_that("get_parameters, empty selection returns NULL (component with no parameters)", {
  expect_null(get_parameters(m, component = "zi"))
  expect_null(get_parameters(m, component = "zi", summary = TRUE))
})


test_that("get_parameters, empty selection returns NULL (pattern that matches nothing)", {
  expect_null(get_parameters(m, parameters = "^does_not_exist$"))
  expect_null(get_parameters(m, parameters = "^does_not_exist$", summary = TRUE))
})


test_that("clean_parameters, non-linear parameters are conditional", {
  out <- clean_parameters(m)
  nl_rows <- grepl("^(b|r|sd)_(AY__)?(ult|omega|theta)", out$Parameter)
  # 3 fixed rows, 2 x 10 group-level rows, 2 SD rows
  expect_identical(sum(nl_rows), 25L)
  expect_true(all(out$Component[nl_rows] == "conditional"))
})


test_that("clean_parameters, non-linear parameters have brms-summary labels", {
  out <- clean_parameters(m)
  fixed_rows <- match(nl_fixed, out$Parameter)
  expect_identical(
    out$Cleaned_Parameter[fixed_rows],
    c("ult_Intercept", "omega_Intercept", "theta_Intercept")
  )

  r_row <- out[out$Parameter == "r_AY__ult[1991,Intercept]", ]
  expect_identical(r_row$Group, "ult_Intercept: AY")
  expect_identical(r_row$Cleaned_Parameter, "AY.1991")

  sd_row <- out[out$Parameter == "sd_AY__ult_Intercept", ]
  expect_identical(sd_row$Group, "SD/Cor: AY")
  expect_identical(sd_row$Cleaned_Parameter, "ult_Intercept")
})


test_that("clean_parameters, no two rows share the same labels", {
  out <- clean_parameters(m)
  labels <- out[c("Effects", "Component", "Group", "Cleaned_Parameter")]
  expect_false(anyDuplicated(labels) > 0)
})
