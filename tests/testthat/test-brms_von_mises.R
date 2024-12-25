skip_on_cran()
skip_if_offline()
skip_on_os("mac")
skip_if_not_installed("brms")
skip_if_not_installed("httr2")

# Tests -------------------------------------------------------------------
test_that("brms-Intercept", {
  m <- suppressWarnings(insight::download_model("brms_von_mises_1"))
  skip_if(is.null(m))

  out <- find_predictors(m)
  expect_identical(out, list(conditional = "x", kappa = "kappa"))

  out <- find_predictors(m, component = "location")
  expect_identical(out, list(conditional = "x"))

  out <- find_variables(m)
  expect_identical(out, list(response = "y", conditional = "x", kappa = "kappa"))

  out <- find_variables(m, component = "location")
  expect_identical(out, list(conditional = "x"))

  out <- find_parameters(m)
  expect_identical(out, list(conditional = c("b_Intercept", "b_xb", "b_kappa_Intercept")))

  expect_true(has_intercept(m))

  out <- model_info(m)
  expect_false(out$is_linear)
  expect_true(out$is_dispersion)
  expect_identical(out$family, "von_mises")

  expect_equal(link_inverse(m)(2), 2.214297, tolerance = 1e-4)
  expect_equal(link_function(m)(2), 1.557408, tolerance = 1e-4)

  out <- get_data(m)
  expect_named(out, c("y", "x"))
})
