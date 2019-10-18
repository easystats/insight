if (require("testthat") &&
  require("insight") &&
  require("ordinal")) {
  context("insight, model_info")

  data(wine, package = "ordinal")
  m1 <- clm(rating ~ temp * contact, data = wine)

  test_that("model_info", {
    expect_true(model_info(m1)$is_ordinal)
  })

  test_that("find_predictors", {
    expect_identical(find_predictors(m1), list(conditional = c("temp", "contact")))
    expect_identical(find_predictors(m1, flatten = TRUE), c("temp", "contact"))
    expect_null(find_predictors(m1, effects = "random"))
  })

  test_that("find_random", {
    expect_null(find_random(m1))
  })

  test_that("get_random", {
    expect_warning(get_random(m1))
  })

  test_that("find_response", {
    expect_identical(find_response(m1), "rating")
  })

  test_that("get_response", {
    expect_equal(get_response(m1), wine$rating)
  })

  test_that("get_predictors", {
    expect_equal(colnames(get_predictors(m1)), c("temp", "contact"))
  })

  test_that("link_inverse", {
    expect_equal(link_inverse(m1)(.2), plogis(.2), tolerance = 1e-5)
  })

  test_that("get_data", {
    expect_equal(nrow(get_data(m1)), 72)
    expect_equal(colnames(get_data(m1)), c("rating", "temp", "contact"))
  })

  test_that("find_formula", {
    expect_length(find_formula(m1), 1)
    expect_equal(
      find_formula(m1),
      list(conditional = as.formula("rating ~ temp * contact"))
    )
  })

  test_that("find_terms", {
    expect_equal(find_terms(m1), list(
      response = "rating",
      conditional = c("temp", "contact")
    ))
    expect_equal(
      find_terms(m1, flatten = TRUE),
      c("rating", "temp", "contact")
    )
  })

  test_that("n_obs", {
    expect_equal(n_obs(m1), 72)
  })

  test_that("linkfun", {
    expect_false(is.null(link_function(m1)))
  })

  test_that("find_parameters", {
    expect_equal(
      find_parameters(m1),
      list(
        conditional = c(
          "1|2",
          "2|3",
          "3|4",
          "4|5",
          "tempwarm",
          "contactyes",
          "tempwarm:contactyes"
        )
      )
    )
    expect_equal(nrow(get_parameters(m1)), 7)
    expect_equal(
      get_parameters(m1)$Parameter,
      c(
        "1|2",
        "2|3",
        "3|4",
        "4|5",
        "tempwarm",
        "contactyes",
        "tempwarm:contactyes"
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
