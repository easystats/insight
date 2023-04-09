test_that("find_random", {
  m <- glm(am ~ mpg, mtcars, family = binomial())
  expect_null(find_random(m))
})
