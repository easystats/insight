if (skip_if_not_installed("insight")) {
 
  m <- glm(am ~ mpg, mtcars, family = binomial())
  test_that("find_random", {
    expect_null(find_random(m))
  })
}
