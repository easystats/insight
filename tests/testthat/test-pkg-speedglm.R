requiet("speedglm")
requiet("glmmTMB")

test_that("speedglm()", {
    data(Salamanders)
    Salamanders$cover <- abs(Salamanders$cover)

    m1 <- speedglm(count ~ mined + log(cover) + sample,
      family = poisson(),
      data = Salamanders)

    # model_info
    expect_true(model_info(m1)$is_poisson)
    expect_true(model_info(m1)$is_count)
    expect_false(model_info(m1)$is_negbin)
    expect_false(model_info(m1)$is_binomial)
    expect_false(model_info(m1)$is_linear)

    # find_predictors
    expect_identical(find_predictors(m1), list(conditional = c("mined", "cover", "sample")))
    expect_identical(
      find_predictors(m1, flatten = TRUE),
      c("mined", "cover", "sample"))
    expect_null(find_predictors(m1, effects = "random"))

    # find_random
    expect_null(find_random(m1))

    # get_random
    expect_warning(get_random(m1))

    # find_response
    expect_identical(find_response(m1), "count")

    # get_response
    expect_equal(get_response(m1), Salamanders$count)

    # get_predictors
    expect_equal(colnames(get_predictors(m1)), c("mined", "cover", "sample"))

    # link_inverse
    expect_equal(link_inverse(m1)(.2), exp(.2), tolerance = 1e-5)

    # linkfun
    expect_equal(link_function(m1)(.2), log(.2), tolerance = 1e-5)

    # get_data
    expect_equal(nrow(get_data(m1)), 644)
    expect_equal(
      colnames(get_data(m1)),
      c("count", "mined", "cover", "sample"))

    # find_formula
    expect_length(find_formula(m1), 1)
    expect_equal(
      find_formula(m1),
      list(conditional = as.formula("count ~ mined + log(cover) + sample")),
      ignore_attr = TRUE)

    # find_variables
    expect_equal(
      find_variables(m1),
      list(
        response = "count",
        conditional = c("mined", "cover", "sample")
      ))
    expect_equal(
      find_variables(m1, flatten = TRUE),
      c("count", "mined", "cover", "sample"))

    # n_obs
    expect_equal(n_obs(m1), 644)

    # find_parameters
    expect_equal(
      find_parameters(m1),
      list(conditional = c("(Intercept)", "minedno", "log(cover)", "sample")))
    expect_equal(nrow(get_parameters(m1)), 4)
    expect_equal(
      get_parameters(m1)$Parameter,
      c("(Intercept)", "minedno", "log(cover)", "sample"))

    # is_multivariate
    expect_false(is_multivariate(m1))

    # find_terms
    expect_equal(
      find_terms(m1),
      list(response = "count",
           conditional = c("mined", "log(cover)", "sample")))

    # find_algorithm
    expect_equal(find_algorithm(m1), list(algorithm = "eigen"))

    # find_statistic
    expect_identical(find_statistic(m1), "z-statistic")
})


test_that("speedlm()", {
  data(iris)
  data(mtcars)
  m1 <- speedlm(Sepal.Length ~ Petal.Width + Species, data = iris)
  m2 <- speedlm(log(mpg) ~ log(hp) + cyl + I(cyl^2) + poly(wt, degree = 2, raw = TRUE),
                data = mtcars)

  # model_info
  expect_true(model_info(m1)$is_linear)

  # find_predictors
  expect_identical(find_predictors(m1), list(conditional = c("Petal.Width", "Species")))
  expect_identical(find_predictors(m1, flatten = TRUE), c("Petal.Width", "Species"))
  expect_null(find_predictors(m1, effects = "random"))

  # find_random
  expect_null(find_random(m1))

  # get_random
  expect_warning(get_random(m1))

  # find_response
  expect_identical(find_response(m1), "Sepal.Length")

  # get_response
  expect_equal(get_response(m1), iris$Sepal.Length)

  # get_predictors
  expect_equal(colnames(get_predictors(m1)), c("Petal.Width", "Species"))

  # link_inverse
  expect_equal(link_inverse(m1)(.2), .2, tolerance = 1e-5)

  # link_function
  expect_equal(link_function(m1)(.2), .2, tolerance = 1e-5)

  # get_data
  expect_equal(nrow(get_data(m1)), 150)
  expect_equal(
    colnames(get_data(m1)),
    c("Sepal.Length", "Petal.Width", "Species"))
  expect_equal(colnames(get_data(m2)), c("mpg", "hp", "cyl", "wt"))

  # find_formula
  expect_length(find_formula(m1), 1)
  expect_equal(
    find_formula(m1),
    list(conditional = as.formula("Sepal.Length ~ Petal.Width + Species")),
    ignore_attr = TRUE
  )
  expect_equal(
    find_formula(m2),
    list(conditional = as.formula("log(mpg) ~ log(hp) + cyl + I(cyl^2) + poly(wt, degree = 2, raw = TRUE)")),
    ignore_attr = TRUE)

  # find_variables
  expect_equal(
    find_variables(m1),
    list(
      response = "Sepal.Length",
      conditional = c("Petal.Width", "Species")))
  expect_equal(
    find_variables(m1, flatten = TRUE),
    c("Sepal.Length", "Petal.Width", "Species"))
  expect_equal(
    find_variables(m2, flatten = TRUE),
    c("mpg", "hp", "cyl", "wt"))

  # n_obs
  expect_equal(n_obs(m1), 150)
  expect_equal(n_obs(m2), 32)

  # find_parameters
  expect_equal(
    find_parameters(m1),
    list(
      conditional = c(
        "(Intercept)",
        "Petal.Width",
        "Speciesversicolor",
        "Speciesvirginica")))
  expect_equal(
    find_parameters(m2),
    list(
      conditional = c(
        "(Intercept)",
        "log(hp)",
        "cyl",
        "I(cyl^2)",
        "poly(wt, degree = 2, raw = TRUE)1",
        "poly(wt, degree = 2, raw = TRUE)2")))
  expect_equal(nrow(get_parameters(m1)), 4)
  expect_equal(
    get_parameters(m1)$Parameter,
    c("(Intercept)",
      "Petal.Width",
      "Speciesversicolor",
      "Speciesvirginica"))

  # is_multivariate
  expect_false(is_multivariate(m1))

  # find_terms
  expect_equal(
    find_terms(m1),
    list(
      response = "Sepal.Length",
      conditional = c("Petal.Width", "Species")))
  expect_equal(
    find_terms(m2),
    list(
      response = "log(mpg)",
      conditional = c(
        "log(hp)",
        "cyl",
        "I(cyl^2)",
        "poly(wt, degree = 2, raw = TRUE)")))

  # find_algorithm
  expect_equal(find_algorithm(m1), list(algorithm = "eigen"))

  # find_statistic
  expect_identical(find_statistic(m1), "t-statistic")
  expect_identical(find_statistic(m2), "t-statistic")
})
