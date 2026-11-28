expect_model_accessor_errors <- function(data) {
  expect_error(find_parameters(data), "A data frame is no valid object")
  expect_error(find_formula(data), "A data frame is not a valid object")
  expect_error(find_statistic(data), "The entered object is not a model object")
}

test_that("model accessors reject data frames", {
  expect_model_accessor_errors(iris)
})

test_that("model accessors reject tibbles", {
  skip_if_not_installed("tibble")
  expect_model_accessor_errors(tibble::as_tibble(iris))
})

test_that("model accessors reject data tables", {
  skip_if_not_installed("data.table")
  expect_model_accessor_errors(data.table::as.data.table(iris))
})
