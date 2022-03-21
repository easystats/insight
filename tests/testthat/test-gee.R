if (requiet("testthat") &&
  requiet("insight") &&
  requiet("gee")) {
  data(warpbreaks)
  void <- capture.output(suppressMessages(
    m1 <- gee(breaks ~ tension, id = wool, data = warpbreaks)
  ))

  test_that("model_info", {
    expect_true(model_info(m1)$is_linear)
  })

  test_that("find_predictors", {
    expect_identical(find_predictors(m1), list(conditional = "tension"))
    expect_identical(find_predictors(m1, flatten = TRUE), "tension")
    expect_identical(
      find_predictors(m1, effects = "random"),
      list(random = "wool")
    )
    expect_identical(
      find_predictors(m1, effects = "all", flatten = TRUE),
      c("tension", "wool")
    )
  })

  test_that("find_response", {
    expect_identical(find_response(m1), "breaks")
  })

  test_that("get_response", {
    expect_equal(get_response(m1), warpbreaks$breaks)
  })

  test_that("find_random", {
    expect_equal(find_random(m1), list(random = "wool"))
  })

  test_that("get_random", {
    expect_equal(get_random(m1), warpbreaks[, "wool", drop = FALSE], ignore_attr = TRUE)
  })

  test_that("get_predictors", {
    expect_equal(get_predictors(m1), warpbreaks[, "tension", drop = FALSE])
  })

  test_that("link_inverse", {
    expect_equal(link_inverse(m1)(.2), .2, tolerance = 1e-5)
  })

  test_that("get_data", {
    expect_equal(nrow(get_data(m1)), 54)
    expect_equal(colnames(get_data(m1)), c("breaks", "tension", "wool"))
  })

  test_that("find_formula", {
    expect_length(find_formula(m1), 2)
    expect_equal(
      find_formula(m1),
      list(
        conditional = as.formula("breaks ~ tension"),
        random = as.formula("~wool")
      ),
      ignore_attr = TRUE
    )
  })

  test_that("find_terms", {
    expect_equal(
      find_terms(m1),
      list(
        response = "breaks",
        conditional = "tension",
        random = "wool"
      )
    )
    expect_equal(
      find_terms(m1, flatten = TRUE),
      c("breaks", "tension", "wool")
    )
  })

  test_that("n_obs", {
    expect_equal(n_obs(m1), 54)
  })

  test_that("linkfun", {
    expect_false(is.null(link_function(m1)))
  })

  test_that("find_parameters", {
    expect_equal(
      find_parameters(m1),
      list(conditional = c(
        "(Intercept)", "tensionM", "tensionH"
      ))
    )
    expect_equal(nrow(get_parameters(m1)), 3)
    expect_equal(
      get_parameters(m1)$Parameter,
      c("(Intercept)", "tensionM", "tensionH")
    )
  })

  test_that("is_multivariate", {
    expect_false(is_multivariate(m1))
  })

  test_that("find_algorithm", {
    expect_equal(find_algorithm(m1), list(algorithm = "ML"))
  })

  test_that("find_statistic", {
    expect_identical(find_statistic(m1), "z-statistic")
  })
}
