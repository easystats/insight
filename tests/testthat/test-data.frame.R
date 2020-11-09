if (require("testthat") && require("insight")) {
  data(iris)

  test_that("find_parameters", {
    expect_error(find_parameters(iris))
  })

  test_that("find_formula", {
    expect_error(find_formula(iris))
  })

  test_that("find_statistic", {
    expect_error(find_statistic(iris))
  })
}
