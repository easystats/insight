if (require("testthat") && require("insight")) {
  context("insight, model_info")

  data(iris)
  data(mtcars)

  m1 <- lm(Sepal.Length ~ Petal.Width + Species, data = iris)
  m2 <- lm(log(mpg) ~ log(hp) + cyl + I(cyl^2) + poly(wt, degree = 2, raw = TRUE), data = mtcars)

  test_that("model_info", {
    expect_true(model_info(m1)$is_linear)
  })

  test_that("find_predictors", {
    expect_identical(find_predictors(m1), list(conditional = c("Petal.Width", "Species")))
    expect_identical(find_predictors(m1, flatten = TRUE), c("Petal.Width", "Species"))
    expect_null(find_predictors(m1, effects = "random"))

    expect_identical(find_predictors(m2), list(conditional = c("hp", "cyl", "wt")))
    expect_identical(find_predictors(m2, flatten = TRUE), c("hp", "cyl", "wt"))
    expect_null(find_predictors(m2, effects = "random"))
  })

  test_that("find_response", {
    expect_identical(find_response(m1), "Sepal.Length")
    expect_identical(find_response(m2), "mpg")
  })

  test_that("link_inverse", {
    expect_identical(link_inverse(m1)(.2), .2)
    expect_identical(link_inverse(m2)(.2), .2)
  })

  test_that("get_data", {
    expect_equal(nrow(get_data(m1)), 150)
    expect_equal(colnames(get_data(m1)), c("Sepal.Length", "Petal.Width", "Species"))
    expect_equal(nrow(get_data(m2)), 32)
    expect_equal(colnames(get_data(m2)), c("mpg", "hp", "cyl", "wt"))
  })

  test_that("find_formula", {
    expect_length(find_formula(m1), 1)
    expect_equal(
      find_formula(m1),
      list(conditional = as.formula("Sepal.Length ~ Petal.Width + Species"))
    )

    expect_length(find_formula(m2), 1)
    expect_equal(
      find_formula(m2),
      list(conditional = as.formula("log(mpg) ~ log(hp) + cyl + I(cyl^2) + poly(wt, degree = 2, raw = TRUE)"))
    )
  })

  test_that("find_terms", {
    expect_equal(find_terms(m1), list(response = "Sepal.Length", conditional = c("Petal.Width", "Species")))
    expect_equal(find_terms(m2), list(response = "mpg", conditional = c("hp", "cyl", "wt")))
    expect_equal(find_terms(m1, flatten = TRUE), c("Sepal.Length", "Petal.Width", "Species"))
    expect_equal(find_terms(m2, flatten = TRUE), c("mpg", "hp", "cyl", "wt"))
  })

}
