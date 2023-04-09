skip_if_not_installed("mclogit")

data(Transport, package = "mclogit")
mod_mb <- mclogit::mblogit(factor(gear) ~ mpg + hp, data = mtcars, trace = FALSE)
mod_mc <- mclogit::mclogit(resp | suburb ~ distance + cost, data = Transport, trace = FALSE)

test_that("mblogit and mclogit is not linear", {
  skip_if_not(packageVersion("mclogit") >= "0.9.1")
  expect_false(model_info(mod_mb)$is_linear)
  expect_true(model_info(mod_mb)$is_logit)
  expect_true(is_model(mod_mb))
  expect_true(is_model_supported(mod_mb))

  expect_false(model_info(mod_mc)$is_linear)
  expect_true(model_info(mod_mc)$is_logit)
  expect_true(is_model(mod_mc))
  expect_true(is_model_supported(mod_mc))
})

test_that("get_parameters", {
  out <- get_parameters(mod_mb)
  expect_equal(
    out$Estimate,
    c(-5.76561, -30.95279, 0.5077, 1.05108, -0.03696, 0.0582),
    tolerance = 1e-4
  )
  expect_identical(
    out$Parameter,
    gsub("(.*)~(.*)", "\\2", names(coef(mod_mb)))
  )
  expect_identical(
    out$Response,
    c("4", "5", "4", "5", "4", "5")
  )
  out <- get_parameters(mod_mc)
  expect_equal(out$Estimate, c(-1.4394, -0.97753), tolerance = 1e-4)
  expect_identical(colnames(out), c("Parameter", "Estimate"))
})

test_that("get_statistic", {
  out <- get_statistic(mod_mb)
  expect_equal(
    out$Statistic,
    c(-0.52735, -2.51803, 1.30274, 2.44884, -1.04056, 2.16406),
    tolerance = 1e-4
  )
  expect_identical(
    out$Parameter,
    gsub("(.*)~(.*)", "\\2", names(coef(mod_mb)))
  )
  expect_identical(
    out$Response,
    c("4", "5", "4", "5", "4", "5")
  )
  out <- get_statistic(mod_mc)
  expect_equal(out$Statistic, c(-27.06905, -24.51836), tolerance = 1e-4)
  expect_identical(colnames(out), c("Parameter", "Statistic"))
})

test_that("find_predictors", {
  expect_identical(
    find_predictors(mod_mb),
    list(conditional = c("mpg", "hp"))
  )
  expect_identical(
    find_predictors(mod_mc),
    list(conditional = c("distance", "cost"))
  )
})

test_that("find_formula", {
  expect_equal(
    find_formula(mod_mb),
    list(conditional = factor(gear) ~ mpg + hp),
    ignore_attr = TRUE
  )
  expect_equal(
    find_formula(mod_mc),
    list(conditional = cbind(resp, suburb) ~ distance + cost),
    ignore_attr = TRUE
  )
})

test_that("find_response", {
  expect_identical(find_response(mod_mb), "gear")
  expect_identical(find_response(mod_mc), "cbind(resp, suburb)")
  expect_identical(find_response(mod_mc, combine = FALSE), c("resp", "suburb"))
})

test_that("find_statistic", {
  expect_identical(find_statistic(mod_mb), "z-statistic")
  expect_identical(find_statistic(mod_mc), "z-statistic")
})
