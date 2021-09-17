if (requiet("testthat") &&
  requiet("insight") &&
  requiet("speedglm")) {
  data(iris)
  data(mtcars)

  m1 <- speedlm(Sepal.Length ~ Petal.Width + Species, data = iris)
  m2 <-
    speedlm(log(mpg) ~ log(hp) + cyl + I(cyl^2) + poly(wt, degree = 2, raw = TRUE),
      data = mtcars
    )

  test_that("model_info", {
    expect_true(model_info(m1)$is_linear)
  })

  test_that("find_predictors", {
    expect_identical(find_predictors(m1), list(conditional = c("Petal.Width", "Species")))
    expect_identical(
      find_predictors(m1, flatten = TRUE),
      c("Petal.Width", "Species")
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
    expect_identical(find_response(m1), "Sepal.Length")
  })

  test_that("get_response", {
    expect_equal(get_response(m1), iris$Sepal.Length)
  })

  test_that("get_predictors", {
    expect_equal(colnames(get_predictors(m1)), c("Petal.Width", "Species"))
  })

  test_that("link_inverse", {
    expect_equal(link_inverse(m1)(.2), .2, tolerance = 1e-5)
  })

  test_that("linkfun", {
    expect_equal(link_function(m1)(.2), .2, tolerance = 1e-5)
  })

  test_that("get_data", {
    expect_equal(nrow(get_data(m1)), 150)
    expect_equal(
      colnames(get_data(m1)),
      c("Sepal.Length", "Petal.Width", "Species")
    )
    expect_equal(colnames(get_data(m2)), c("mpg", "hp", "cyl", "wt"))
  })

  test_that("find_formula", {
    expect_length(find_formula(m1), 1)
    expect_equal(
      find_formula(m1),
      list(conditional = as.formula("Sepal.Length ~ Petal.Width + Species")),
      ignore_attr = TRUE
    )
    expect_equal(
      find_formula(m2),
      list(
        conditional = as.formula(
          "log(mpg) ~ log(hp) + cyl + I(cyl^2) + poly(wt, degree = 2, raw = TRUE)"
        )
      ),
      ignore_attr = TRUE
    )
  })

  test_that("find_variables", {
    expect_equal(
      find_variables(m1),
      list(
        response = "Sepal.Length",
        conditional = c("Petal.Width", "Species")
      )
    )
    expect_equal(
      find_variables(m1, flatten = TRUE),
      c("Sepal.Length", "Petal.Width", "Species")
    )
    expect_equal(
      find_variables(m2, flatten = TRUE),
      c("mpg", "hp", "cyl", "wt")
    )
  })

  test_that("n_obs", {
    expect_equal(n_obs(m1), 150)
    expect_equal(n_obs(m2), 32)
  })

  test_that("find_parameters", {
    expect_equal(
      find_parameters(m1),
      list(
        conditional = c(
          "(Intercept)",
          "Petal.Width",
          "Speciesversicolor",
          "Speciesvirginica"
        )
      )
    )
    expect_equal(
      find_parameters(m2),
      list(
        conditional = c(
          "(Intercept)",
          "log(hp)",
          "cyl",
          "I(cyl^2)",
          "poly(wt, degree = 2, raw = TRUE)1",
          "poly(wt, degree = 2, raw = TRUE)2"
        )
      )
    )
    expect_equal(nrow(get_parameters(m1)), 4)
    expect_equal(
      get_parameters(m1)$Parameter,
      c(
        "(Intercept)",
        "Petal.Width",
        "Speciesversicolor",
        "Speciesvirginica"
      )
    )
  })

  test_that("is_multivariate", {
    expect_false(is_multivariate(m1))
  })

  test_that("find_terms", {
    expect_equal(
      find_terms(m1),
      list(
        response = "Sepal.Length",
        conditional = c("Petal.Width", "Species")
      )
    )
    expect_equal(
      find_terms(m2),
      list(
        response = "log(mpg)",
        conditional = c(
          "log(hp)",
          "cyl",
          "I(cyl^2)",
          "poly(wt, degree = 2, raw = TRUE)"
        )
      )
    )
  })

  test_that("find_algorithm", {
    expect_equal(find_algorithm(m1), list(algorithm = "eigen"))
  })

  test_that("find_statistic", {
    expect_identical(find_statistic(m1), "t-statistic")
    expect_identical(find_statistic(m2), "t-statistic")
  })
}
