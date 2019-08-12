if (require("testthat") && require("insight") && require("splines")) {
  context("insight, namespace, splines")

  data(iris)
  m1 <- lm(Sepal.Length ~ splines::bs(Petal.Width, df = 4) + Species, data = iris)

  test_that("model_info", {
    expect_true(model_info(m1)$is_linear)
  })

  test_that("find_predictors", {
    expect_identical(find_predictors(m1), list(conditional = c("Petal.Width", "Species")))
    expect_identical(find_predictors(m1, flatten = TRUE), c("Petal.Width", "Species"))
    expect_null(find_predictors(m1, effects = "random"))
  })

  test_that("get_predictors", {
    expect_equal(get_predictors(m1), iris[, c("Petal.Width", "Species")])
  })

  test_that("find_response", {
    expect_identical(find_response(m1), "Sepal.Length")
  })

  test_that("get_response", {
    expect_identical(get_response(m1), iris$Sepal.Length)
  })

  test_that("link_inverse", {
    expect_equal(link_inverse(m1)(.2), .2, tolerance = 1e-4)
  })

  test_that("link_function", {
    expect_equal(link_function(m1)(.2), .2, tolerance = 1e-4)
  })

  test_that("get_data", {
    expect_equal(nrow(get_data(m1)), 150)
    expect_equal(colnames(get_data(m1)), c("Sepal.Length", "Species", "Petal.Width"))
  })

  test_that("find_formula", {
    expect_length(find_formula(m1), 1)
    expect_equal(
      find_formula(m1),
      list(conditional = as.formula("Sepal.Length ~ splines::bs(Petal.Width, df = 4) + Species"))
    )
  })

  test_that("find_variables", {
    expect_equal(find_variables(m1), list(response = "Sepal.Length", conditional = c("Petal.Width", "Species")))
    expect_equal(find_variables(m1, flatten = TRUE), c("Sepal.Length", "Petal.Width", "Species"))
  })

  test_that("find_parameters", {
    expect_equal(
      find_parameters(m1),
      list(
        conditional = c(
          "(Intercept)", "splines::bs(Petal.Width, df = 4)1",
          "splines::bs(Petal.Width, df = 4)2", "splines::bs(Petal.Width, df = 4)3",
          "splines::bs(Petal.Width, df = 4)4", "Speciesversicolor",
          "Speciesvirginica"
        )
      )
    )
  })

  test_that("get_parameters", {
    expect_equal(nrow(get_parameters(m1)), 7)
    expect_equal(get_parameters(m1)$parameter, c(
      "(Intercept)", "splines::bs(Petal.Width, df = 4)1", "splines::bs(Petal.Width, df = 4)2",
      "splines::bs(Petal.Width, df = 4)3", "splines::bs(Petal.Width, df = 4)4",
      "Speciesversicolor", "Speciesvirginica"
    ))
  })

  test_that("find_algorithm", {
    expect_equal(find_algorithm(m1), list(algorithm = "OLS"))
  })

  test_that("find_terms", {
    expect_equal(
      find_terms(m1),
      list(
        response = "Sepal.Length",
        conditional = c("splines", "bs(Petal.Width, df = 4)", "Species")
      )
    )
  })
}
