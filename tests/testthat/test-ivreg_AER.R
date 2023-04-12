skip_if_not_installed("AER")

data(CigarettesSW, package = "AER")
CigarettesSW$rprice <- with(CigarettesSW, price / cpi)
CigarettesSW$rincome <- with(CigarettesSW, income / population / cpi)
CigarettesSW$tdiff <- with(CigarettesSW, (taxs - tax) / cpi)

cig_data <<- CigarettesSW

mod_aer_ivreg <- AER::ivreg(
  log(packs) ~ log(rprice) + log(rincome) | log(rincome) + tdiff + I(tax / cpi),
  data = cig_data,
  subset = year == "1995"
)

test_that("model_info", {
  expect_true(model_info(mod_aer_ivreg)$is_linear)
})

test_that("find_predictors", {
  expect_identical(
    find_predictors(mod_aer_ivreg),
    list(
      conditional = c("rprice", "rincome"),
      instruments = c("rincome", "tdiff", "tax", "cpi")
    )
  )
  expect_identical(
    find_predictors(mod_aer_ivreg, flatten = TRUE),
    c("rprice", "rincome", "tdiff", "tax", "cpi")
  )
  expect_null(find_predictors(mod_aer_ivreg, effects = "random"))
})

test_that("find_random", {
  expect_null(find_random(mod_aer_ivreg))
})

test_that("get_random", {
  expect_warning(get_random(mod_aer_ivreg))
})

test_that("find_response", {
  expect_identical(find_response(mod_aer_ivreg), "packs")
})

test_that("get_response", {
  expect_equal(get_response(mod_aer_ivreg), cig_data$packs[cig_data$year == "1995"])
})

test_that("get_predictors", {
  expect_equal(
    colnames(get_predictors(mod_aer_ivreg)),
    c("rprice", "rincome", "tdiff", "tax", "cpi")
  )
})

test_that("link_inverse", {
  expect_equal(link_inverse(mod_aer_ivreg)(0.2), 0.2, tolerance = 1e-5)
})

test_that("get_data", {
  expect_equal(nrow(get_data(mod_aer_ivreg)), 48)
  expect_equal(
    colnames(get_data(mod_aer_ivreg)),
    c("packs", "rprice", "rincome", "tdiff", "tax", "cpi", "year")
  )
})

test_that("find_formula", {
  expect_length(find_formula(mod_aer_ivreg), 2)
  expect_equal(
    find_formula(mod_aer_ivreg),
    list(
      conditional = as.formula("log(packs) ~ log(rprice) + log(rincome)"),
      instruments = as.formula("~log(rincome) + tdiff + I(tax/cpi)")
    ),
    ignore_attr = TRUE
  )
})

test_that("find_variables", {
  expect_equal(
    find_variables(mod_aer_ivreg),
    list(
      response = "packs",
      conditional = c("rprice", "rincome"),
      instruments = c("rincome", "tdiff", "tax", "cpi")
    )
  )
  expect_equal(
    find_variables(mod_aer_ivreg, flatten = TRUE),
    c("packs", "rprice", "rincome", "tdiff", "tax", "cpi")
  )
})

test_that("n_obs", {
  expect_equal(n_obs(mod_aer_ivreg), 48)
})

test_that("linkfun", {
  expect_false(is.null(link_function(mod_aer_ivreg)))
})

test_that("find_parameters", {
  expect_equal(
    find_parameters(mod_aer_ivreg),
    list(
      conditional = c("(Intercept)", "log(rprice)", "log(rincome)")
    )
  )
  expect_equal(nrow(get_parameters(mod_aer_ivreg)), 3)
  expect_equal(
    get_parameters(mod_aer_ivreg)$Parameter,
    c("(Intercept)", "log(rprice)", "log(rincome)")
  )
})

test_that("is_multivariate", {
  expect_false(is_multivariate(mod_aer_ivreg))
})

test_that("find_terms", {
  expect_equal(
    find_terms(mod_aer_ivreg),
    list(
      response = "log(packs)",
      conditional = c("log(rprice)", "log(rincome)"),
      instruments = c("log(rincome)", "tdiff", "I(tax/cpi)")
    )
  )
  expect_equal(nrow(get_parameters(mod_aer_ivreg)), 3)
  expect_equal(
    get_parameters(mod_aer_ivreg)$Parameter,
    c("(Intercept)", "log(rprice)", "log(rincome)")
  )
})

test_that("find_statistic", {
  expect_identical(find_statistic(mod_aer_ivreg), "t-statistic")
})
