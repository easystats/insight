if (require("testthat") && require("insight") && require("glmmTMB")) {
  context("insight, model_info")

  data(Salamanders)
  Salamanders$cover <- abs(Salamanders$cover)

  m1 <- glm(count ~ mined + log(cover) + sample, family = poisson, data = Salamanders)

  test_that("model_info", {
    expect_true(model_info(m1)$is_poisson)
    expect_true(model_info(m1)$is_count)
    expect_false(model_info(m1)$is_negbin)
    expect_false(model_info(m1)$is_binomial)
  })

  test_that("find_predictors", {
    expect_identical(find_predictors(m1), list(conditional = c("mined", "cover", "sample")))
    expect_identical(find_predictors(m1, flatten = TRUE), c("mined", "cover", "sample"))
    expect_null(find_predictors(m1, effects = "random"))
  })

  test_that("find_response", {
    expect_identical(find_response(m1), "count")
  })

  test_that("link_inverse", {
    expect_identical(link_inverse(m1)(.2), exp(.2))
  })

  test_that("get_data", {
    expect_equal(nrow(get_data(m1)), 644)
    expect_equal(colnames(get_data(m1)), c("count", "mined", "cover", "sample"))
  })

  test_that("find_formula", {
    expect_length(find_formula(m1), 1)
    expect_equal(
      find_formula(m1),
      list(conditional = as.formula("count ~ mined + log(cover) + sample"))
    )
  })

  test_that("find_terms", {
    expect_equal(find_terms(m1), list(response = "count", conditional = c("mined", "cover", "sample")))
    expect_equal(find_terms(m1, flatten = TRUE), c("count", "mined", "cover", "sample"))
  })

  test_that("n_obs", {
    expect_equal(n_obs(m1), 644)
  })
}
