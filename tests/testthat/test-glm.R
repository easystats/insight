if (requiet("testthat") &&
  requiet("insight") &&
  requiet("glmmTMB")) {
  data(Salamanders)
  Salamanders$cover <- abs(Salamanders$cover)

  m1 <- glm(count ~ mined + log(cover) + sample,
    family = poisson,
    data = Salamanders
  )

  test_that("model_info", {
    expect_true(model_info(m1)$is_poisson)
    expect_true(model_info(m1)$is_count)
    expect_false(model_info(m1)$is_negbin)
    expect_false(model_info(m1)$is_binomial)
    expect_false(model_info(m1)$is_linear)
  })

  test_that("loglik", {
    expect_equal(get_loglikelihood(m1), logLik(m1), ignore_attr = TRUE)
  })

  test_that("get_df", {
    expect_equal(get_df(m1), df.residual(m1), ignore_attr = TRUE)
    expect_equal(get_df(m1, type = "model"), attr(logLik(m1), "df"), ignore_attr = TRUE)
  })


  test_that("find_predictors", {
    expect_identical(find_predictors(m1), list(conditional = c("mined", "cover", "sample")))
    expect_identical(
      find_predictors(m1, flatten = TRUE),
      c("mined", "cover", "sample")
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
    expect_identical(find_response(m1), "count")
  })

  test_that("get_response", {
    expect_equal(get_response(m1), Salamanders$count)
  })

  test_that("get_predictors", {
    expect_equal(colnames(get_predictors(m1)), c("mined", "cover", "sample"))
  })

  test_that("link_inverse", {
    expect_equal(link_inverse(m1)(.2), exp(.2), tolerance = 1e-5)
  })

  test_that("linkfun", {
    expect_equal(link_function(m1)(.2), -1.609438, tolerance = 1e-4)
  })

  test_that("get_data", {
    expect_equal(nrow(get_data(m1)), 644)
    expect_equal(
      colnames(get_data(m1)),
      c("count", "mined", "cover", "sample")
    )
  })

  test_that("get_call", {
    expect_equal(class(get_call(m1)), "call")
  })

  test_that("find_formula", {
    expect_length(find_formula(m1), 1)
    expect_equal(
      find_formula(m1),
      list(conditional = as.formula("count ~ mined + log(cover) + sample")),
      ignore_attr = TRUE
    )
  })

  test_that("find_variables", {
    expect_equal(
      find_variables(m1),
      list(
        response = "count",
        conditional = c("mined", "cover", "sample")
      )
    )
    expect_equal(
      find_variables(m1, flatten = TRUE),
      c("count", "mined", "cover", "sample")
    )
  })

  test_that("n_obs", {
    expect_equal(n_obs(m1), 644)
  })

  test_that("find_parameters", {
    expect_equal(
      find_parameters(m1),
      list(
        conditional = c("(Intercept)", "minedno", "log(cover)", "sample")
      )
    )
    expect_equal(nrow(get_parameters(m1)), 4)
    expect_equal(
      get_parameters(m1)$Parameter,
      c("(Intercept)", "minedno", "log(cover)", "sample")
    )
  })

  test_that("is_multivariate", {
    expect_false(is_multivariate(m1))
  })

  test_that("find_terms", {
    expect_equal(
      find_terms(m1),
      list(
        response = "count",
        conditional = c("mined", "log(cover)", "sample")
      )
    )
  })

  test_that("find_algorithm", {
    expect_equal(find_algorithm(m1), list(algorithm = "ML"))
  })

  test_that("find_statistic", {
    expect_identical(find_statistic(m1), "z-statistic")
  })

  test_that("get_statistic", {
    expect_equal(get_statistic(m1)$Statistic, c(-10.7066515607315, 18.1533878215937, -1.68918157150882, 2.23541768590273), tolerance = 1e-4)
  })
}
