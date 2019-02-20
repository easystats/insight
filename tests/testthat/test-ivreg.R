if (require("testthat") && require("insight") && require("AER")) {
  context("insight, model_info")

  data(CigarettesSW)
  CigarettesSW$rprice <- with(CigarettesSW, price / cpi)
  CigarettesSW$rincome <- with(CigarettesSW, income / population / cpi)
  CigarettesSW$tdiff <- with(CigarettesSW, (taxs - tax) / cpi)

  m1 <- ivreg(log(packs) ~ log(rprice) + log(rincome) | log(rincome) + tdiff + I(tax / cpi), data = CigarettesSW, subset = year == "1995")

  test_that("model_info", {
    expect_true(model_info(m1)$is_linear)
  })

  test_that("find_predictors", {
    expect_identical(find_predictors(m1), list(conditional = c("rprice", "rincome"), instruments = c("rincome", "tdiff", "tax", "cpi")))
    expect_identical(find_predictors(m1, flatten = TRUE), c("rprice", "rincome", "tdiff", "tax", "cpi"))
    expect_null(find_predictors(m1, effects = "random"))
  })

  test_that("find_random", {
    expect_null(find_random(m1))
  })

  test_that("get_random", {
    expect_warning(get_random(m1))
  })

  test_that("find_response", {
    expect_identical(find_response(m1), "packs")
  })

  test_that("get_response", {
    expect_equal(get_response(m1), log(CigarettesSW$packs[CigarettesSW$year == "1995"]))
  })

  test_that("get_predictors", {
    expect_equal(colnames(get_predictors(m1)), c("rprice", "rincome", "tdiff", "tax", "cpi"))
  })

  test_that("link_inverse", {
    expect_equal(link_inverse(m1)(.2), .2, tolerance = 1e-5)
  })

  test_that("get_data", {
    expect_equal(nrow(get_data(m1)), 48)
    expect_equal(colnames(get_data(m1)), c("packs", "rprice", "rincome", "tdiff", "tax", "cpi"))
  })

  test_that("find_formula", {
    expect_length(find_formula(m1), 2)
    expect_equal(
      find_formula(m1),
      list(
        conditional = as.formula("log(packs) ~ log(rprice) + log(rincome)"),
        instruments = as.formula("~log(rincome) + tdiff + I(tax/cpi)")
      )
    )
  })

  test_that("find_terms", {
    expect_equal(find_terms(m1), list(response = "packs", conditional = c("rprice", "rincome"), instruments = c("rincome", "tdiff", "tax", "cpi")))
    expect_equal(find_terms(m1, flatten = TRUE), c("packs", "rprice", "rincome", "tdiff", "tax", "cpi"))
  })

  test_that("n_obs", {
    expect_equal(n_obs(m1), 48)
  })

  test_that("linkfun", {
    expect_false(is.null(link_function(m1)))
  })

  test_that("find_parameters", {
    expect_equal(
      find_parameters(m1),
      list(
        conditional = c("(Intercept)", "log(rprice)", "log(rincome)")
      )
    )
    expect_equal(nrow(get_parameters(m1)), 3)
    expect_equal(get_parameters(m1)$parameter, c("(Intercept)", "log(rprice)", "log(rincome)"))
  })

  test_that("is_multivariate", {
    expect_false(is_multivariate(m1))
  })

  test_that("find_variables", {
    expect_equal(
      find_variables(m1),
      list(
        response = "log(packs)",
        conditional = c("log(rprice)", "log(rincome)"),
        instruments = c("log(rincome)", "tdiff", "I(tax/cpi)")
      )
    )
    expect_equal(nrow(get_parameters(m1)), 3)
    expect_equal(get_parameters(m1)$parameter, c("(Intercept)", "log(rprice)", "log(rincome)"))
  })

}
