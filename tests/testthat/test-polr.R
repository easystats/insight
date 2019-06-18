if (require("testthat") && require("insight") && require("MASS")) {
  context("insight, polr")

  data(housing, package = "MASS")

  m1 <- polr(Sat ~ Infl + Type + Cont, data = housing, weights = Freq)

  test_that("model_info", {
    expect_true(model_info(m1)$is_ordinal)
  })

  test_that("find_predictors", {
    expect_identical(find_predictors(m1), list(conditional = c("Infl", "Type", "Cont")))
    expect_identical(find_predictors(m1, flatten = TRUE), c("Infl", "Type", "Cont"))
    expect_null(find_predictors(m1, effects = "random"))
  })

  test_that("find_response", {
    expect_identical(find_response(m1), "Sat")
  })

  test_that("link_inverse", {
    expect_equal(link_inverse(m1)(.2), plogis(.2), tolerance = 1e-5)
  })

  test_that("get_data", {
    expect_equal(nrow(get_data(m1)), 72)
    expect_equal(colnames(get_data(m1)), c("Sat", "Infl", "Type", "Cont", "(weights)", "Freq"))
  })

  test_that("find_formula", {
    expect_length(find_formula(m1), 1)
    expect_equal(
      find_formula(m1),
      list(conditional = as.formula("Sat ~ Infl + Type + Cont"))
    )
  })

  test_that("find_terms", {
    expect_equal(find_terms(m1), list(response = "Sat", conditional = c("Infl", "Type", "Cont")))
    expect_equal(find_terms(m1, flatten = TRUE), c("Sat", "Infl", "Type", "Cont"))
  })

  test_that("n_obs", {
    expect_equal(n_obs(m1), 1681)
  })

  test_that("linkfun", {
    expect_false(is.null(link_function(m1)))
  })


  test_that("find_parameters", {
    expect_equal(
      find_parameters(m1),
      list(conditional =
             c("Intercept: Low|Medium", "Intercept: Medium|High", "InflMedium",
               "InflHigh", "TypeApartment", "TypeAtrium", "TypeTerrace", "ContHigh"))
    )
  })

  test_that("get_parameters", {
    expect_equal(
      get_parameters(m1),
      data.frame(
        parameter = c("Intercept: Low|Medium", "Intercept: Medium|High", "InflMedium", "InflHigh", "TypeApartment", "TypeAtrium", "TypeTerrace", "ContHigh"),
        estimate = c(-0.4961353438375, 0.690708290379271, 0.566393738890106, 1.28881906381232, -0.572350146429611, -0.366186566153346, -1.09101490767244, 0.360284149947385),
        stringsAsFactors = FALSE,
        row.names = NULL
      )
    )
  })
}
