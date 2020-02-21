if (require("testthat") &&
  require("insight") &&
  require("ordinal") &&
  require("MASS")) {
  context("insight, model_info")

  data(housing, package = "MASS")
  m1 <- clm2(Sat ~ Infl + Type + Cont, weights = Freq, data = housing)

  test_that("model_info", {
    expect_true(model_info(m1)$is_ordinal)
    expect_false(model_info(m1)$is_multinomial)
  })

  test_that("find_predictors", {
    expect_identical(find_predictors(m1), list(conditional = c("Infl", "Type", "Cont")))
    expect_identical(
      find_predictors(m1, flatten = TRUE),
      c("Infl", "Type", "Cont")
    )
    expect_null(find_predictors(m1, effects = "random"))
  })

  test_that("find_random", {
    expect_null(find_random(m1))
  })

  test_that("get_random", {
    expect_warning(get_random(m1))
  })

  test_that("find_response", {
    expect_identical(find_response(m1), "Sat")
  })

  test_that("get_response", {
    expect_equal(get_response(m1), housing$Sat)
  })

  test_that("get_predictors", {
    expect_equal(colnames(get_predictors(m1)), c("Infl", "Type", "Cont"))
  })

  test_that("link_inverse", {
    expect_equal(link_inverse(m1)(.2), plogis(.2), tolerance = 1e-5)
  })

  test_that("find_weights", {
    expect_equal(find_weights(m1), "Freq")
  })

  test_that("get_weights", {
    expect_equal(get_weights(m1), housing$Freq)
  })

  test_that("get_data", {
    expect_equal(nrow(get_data(m1)), 72)
    expect_equal(
      colnames(get_data(m1)),
      c("Sat", "Infl", "Type", "Cont", "(weights)", "Freq")
    )
  })

  test_that("find_formula", {
    expect_length(find_formula(m1), 1)
    expect_equal(
      find_formula(m1),
      list(conditional = as.formula("Sat ~ Infl + Type + Cont"))
    )
  })

  test_that("find_terms", {
    expect_equal(find_terms(m1), list(
      response = "Sat",
      conditional = c("Infl", "Type", "Cont")
    ))
    expect_equal(
      find_terms(m1, flatten = TRUE),
      c("Sat", "Infl", "Type", "Cont")
    )
  })

  test_that("n_obs", {
    expect_equal(n_obs(m1), 1681)
  })

  test_that("linkfun", {
    expect_false(is.null(link_function(m1)))
  })

  test_that("linkinv", {
    expect_false(is.null(link_inverse(m1)))
  })

  test_that("find_parameters", {
    expect_equal(
      find_parameters(m1),
      list(
        conditional = c(
          "Low|Medium",
          "Medium|High",
          "InflMedium",
          "InflHigh",
          "TypeApartment",
          "TypeAtrium",
          "TypeTerrace",
          "ContHigh"
        )
      )
    )
    expect_equal(nrow(get_parameters(m1)), 8)
    expect_equal(
      get_parameters(m1)$Parameter,
      c(
        "Low|Medium",
        "Medium|High",
        "InflMedium",
        "InflHigh",
        "TypeApartment",
        "TypeAtrium",
        "TypeTerrace",
        "ContHigh"
      )
    )
  })

  test_that("is_multivariate", {
    expect_false(is_multivariate(m1))
  })

  test_that("find_statistic", {
    expect_identical(find_statistic(m1), "z-statistic")
  })
}
