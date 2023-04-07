if (skip_if_not_installed("insight")) {
  data(mtcars)
  m <- glm(am ~ mpg, mtcars, family = binomial())
  test_that("find_random", {
    expect_null(find_random(m))
  })
}
