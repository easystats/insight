if (require("testthat") &&
    require("insight") &&
    require("cplm")) {

  data("FineRoot")
  m1 <- cpglmm(RLD ~ Stock + Spacing +  (1|Plant), data = FineRoot)

  test_that("model_info", {
    expect_true(model_info(m1)$is_count)
  })

  test_that("find_predictors", {
    expect_equal(
      find_predictors(m1, effects = "all"),
      list(conditional = c("Stock", "Spacing"), random = "Plant")
    )
    expect_equal(
      find_predictors(m1, effects = "all", flatten = TRUE),
      c("Stock", "Spacing", "Plant")
    )
    expect_equal(
      find_predictors(m1, effects = "fixed"),
      list(conditional = c("Stock", "Spacing"))
    )
    expect_equal(
      find_predictors(m1, effects = "fixed", flatten = TRUE),
      c("Stock", "Spacing")
    )
    expect_equal(
      find_predictors(m1, effects = "random"),
      list(random = "Plant")
    )
    expect_equal(
      find_predictors(m1, effects = "random", flatten = TRUE),
      "Plant"
    )
  })

  test_that("find_random", {
    expect_equal(find_random(m1), list(random = "Plant"))
    expect_equal(find_random(m1, flatten = TRUE), "Plant")
  })

  test_that("find_response", {
    expect_identical(find_response(m1), "RLD")
  })

  test_that("get_response", {
    expect_equal(get_response(m1), FineRoot$RLD)
  })


  test_that("get_data", {
    expect_equal(colnames(get_data(m1)), c("RLD", "Stock", "Spacing", "Plant"))
    expect_equal(colnames(get_data(m1, effects = "all")), c("RLD", "Stock", "Spacing", "Plant"))
    expect_equal(colnames(get_data(m1, effects = "random")), "Plant")
  })

  test_that("find_formula", {
    expect_length(find_formula(m1), 2)
    expect_equal(
      find_formula(m1, component = "conditional"),
      list(
        conditional = as.formula("RLD ~ Stock + Spacing"),
        random = as.formula("~1 | Plant")
      )
    )
  })

  test_that("find_terms", {
    expect_identical(
      find_terms(m1),
      list(
        response = "RLD",
        conditional = c("Stock", "Spacing"),
        random = "Plant"
      )
    )
    expect_identical(
      find_terms(m1, flatten = TRUE),
      c("RLD", "Stock", "Spacing", "Plant")
    )
  })


  test_that("link_function", {
    expect_equal(link_function(m1)(.2), log(.2), tolerance = 1e-3)
  })

  test_that("link_inverse", {
    expect_equal(link_inverse(m1)(.2), exp(.2), tolerance = 1e-3)
  })


  test_that("find_variables", {
    expect_identical(
      find_variables(m1),
      list(
        response = "RLD",
        conditional = c("Stock", "Spacing"),
        random = "Plant"
      )
    )
    expect_identical(
      find_variables(m1, flatten = TRUE),
      c("RLD", "Stock", "Spacing", "Plant")
    )
  })

  test_that("get_predictors", {
    expect_identical(colnames(get_predictors(m1)), c("Stock", "Spacing"))
  })

  test_that("get_random", {
    expect_identical(colnames(get_random(m1)), "Plant")
  })

  test_that("clean_names", {
    expect_identical(clean_names(m1), c("RLD", "Stock", "Spacing", "Plant"))
  })

  test_that("find_parameters", {
    expect_equal(
      find_parameters(m1),
      list(
        conditional = c("(Intercept)", "Stock", "Spacing"),
        random = list(Subject = c("(Intercept)", "Stock", "Spacing"))
      )
    )
    expect_equal(nrow(get_parameters(m1)), 2)
    expect_equal(get_parameters(m1)$Parameter, c("(Intercept)", "Stock", "Spacing"))
  })

  test_that("is_multivariate", {
    expect_false(is_multivariate(m1))
  })

  test_that("get_variance", {
    skip_on_cran()
    skip_on_travis()

    expect_equal(
      get_variance(m1),
      list(
        var.fixed = 0.1687617,
        var.random = 0.0002706301,
        var.residual = 2.763129,
        var.distribution = 2.763129,
        var.dispersion = 0,
        var.intercept = c(Plant = 0.0002706301)
      ),
      tolerance = 1e-3
    )
  })

  test_that("find_random_slopes", {
    expect_null(find_random_slopes(m1))
  })

  test_that("find_statistic", {
    expect_identical(find_statistic(m1), "t-statistic")
  })
}
