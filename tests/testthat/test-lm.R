if (require("testthat") && require("insight")) {
  context("insight, model_info")

  m1 <- lm(Sepal.Length ~ Petal.Width + Species, data = iris)

  test_that("model_info", {
    expect_true(model_info(m1)$is_linear)
  })

  test_that("find_predictors", {
    expect_identical(find_predictors(m1), list(conditional = c("Petal.Width", "Species")))
    expect_identical(find_predictors(m1, flatten = TRUE), c("Petal.Width", "Species"))
    expect_null(find_predictors(m1, effects = "random"))
  })

  test_that("find_response", {
    expect_identical(find_response(m1), "Sepal.Length")
  })

  test_that("link_inverse", {
    expect_identical(link_inverse(m1)(.2), .2)
  })

  test_that("get_data", {
    expect_equal(nrow(get_data(m1)), 150)
  })

  test_that("find_formula", {
    expect_length(find_formula(m1), 1)
    expect_equal(
      find_formula(m1),
      list(conditional = as.formula("Sepal.Length ~ Petal.Width + Species"))
    )
  })
}
