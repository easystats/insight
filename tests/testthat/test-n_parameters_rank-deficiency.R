if (requiet("testthat") && requiet("insight")) {
  set.seed(123)
  data(mtcars)
  m <- lm(formula = wt ~ am * cyl * vs, data = mtcars)

  test_that("n_parameters-rank_deficiency", {
    expect_equal(n_parameters(m), 8)
    expect_equal(n_parameters(m, remove_nonestimable = TRUE), m$rank)
  })
}
