if (require("testthat") &&
  require("insight") &&
  require("multgee")) {
  data(arthritis)
  m1 <- ordLORgee(
    y ~ factor(time) + factor(trt) + factor(baseline),
    data = arthritis,
    id = id,
    LORstr = "uniform",
    repeated = time
  )

  test_that("model_info", {
    expect_true(model_info(m1)$is_ordinal)
    expect_false(model_info(m1)$is_multinomial)
    expect_true(model_info(m1)$is_logit)
    expect_false(model_info(m1)$is_linear)
  })

  test_that("find_predictors", {
    expect_identical(find_predictors(m1), list(conditional = c("time", "trt", "baseline")))
    expect_identical(
      find_predictors(m1, flatten = TRUE),
      c("time", "trt", "baseline")
    )
    expect_identical(find_predictors(m1, effects = "random"), list(random = "id"))
    expect_identical(
      find_predictors(m1, effects = "all", flatten = TRUE),
      c("time", "trt", "baseline", "id")
    )
  })

  test_that("find_response", {
    expect_identical(find_response(m1), "y")
  })

  test_that("get_response", {
    expect_equal(get_response(m1), na.omit(arthritis)$y)
  })

  test_that("find_random", {
    expect_equal(find_random(m1), list(random = "id"))
  })

  test_that("get_random", {
    expect_equal(get_random(m1), arthritis[, "id", drop = FALSE])
  })

  test_that("get_predictors", {
    expect_equal(get_predictors(m1), na.omit(arthritis)[, c("time", "trt", "baseline"), drop = FALSE])
  })

  test_that("link_inverse", {
    expect_equal(link_inverse(m1)(.2), plogis(.2), tolerance = 1e-5)
  })

  test_that("get_data", {
    expect_equal(nrow(get_data(m1)), 888)
    expect_equal(
      colnames(get_data(m1)),
      c("y", "time", "trt", "baseline", "id")
    )
  })

  test_that("find_formula", {
    expect_length(find_formula(m1), 2)
    expect_equivalent(
      find_formula(m1),
      list(
        conditional = as.formula("y ~ factor(time) + factor(trt) + factor(baseline)"),
        random = as.formula("~id")
      ),
      ignore_attr = TRUE
    )
  })

  test_that("find_terms", {
    expect_length(find_terms(m1), 3)
    expect_equal(
      find_terms(m1),
      list(
        response = "y",
        conditional = c("factor(time)", "factor(trt)", "factor(baseline)"),
        random = "id"
      )
    )
  })

  test_that("find_variables", {
    expect_equal(
      find_variables(m1),
      list(
        response = "y",
        conditional = c("time", "trt", "baseline"),
        random = "id"
      )
    )
    expect_equal(
      find_variables(m1, flatten = TRUE),
      c("y", "time", "trt", "baseline", "id")
    )
  })

  test_that("n_obs", {
    expect_equal(n_obs(m1), 888)
  })

  test_that("linkfun", {
    expect_false(is.null(link_function(m1)))
  })

  test_that("find_parameters", {
    expect_equal(
      find_parameters(m1),
      list(
        conditional = c(
          "beta10",
          "beta20",
          "beta30",
          "beta40",
          "factor(time)3",
          "factor(time)5",
          "factor(trt)2",
          "factor(baseline)2",
          "factor(baseline)3",
          "factor(baseline)4",
          "factor(baseline)5"
        )
      )
    )
    expect_equal(nrow(get_parameters(m1)), 11)
    expect_equal(
      get_parameters(m1)$Parameter,
      c(
        "beta10",
        "beta20",
        "beta30",
        "beta40",
        "factor(time)3",
        "factor(time)5",
        "factor(trt)2",
        "factor(baseline)2",
        "factor(baseline)3",
        "factor(baseline)4",
        "factor(baseline)5"
      )
    )
  })

  test_that("is_multivariate", {
    expect_false(is_multivariate(m1))
  })

  test_that("find_algorithm", {
    expect_equal(find_algorithm(m1), list(algorithm = "Fisher's scoring ML"))
  })

  test_that("find_statistic", {
    expect_identical(find_statistic(m1), "z-statistic")
  })
}
