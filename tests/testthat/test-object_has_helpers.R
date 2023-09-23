test_that("object_has_* helpers", {
  expect_true(object_has_names(mtcars, "am"))
  expect_equal(object_has_names(anscombe, c("x1", "z1", "y1")), c(TRUE, FALSE, TRUE))
  expect_equal(object_has_names(list(x = 1, y = 2), c("x", "a")), c(TRUE, FALSE))

  expect_true(object_has_rownames(mtcars))
  expect_false(object_has_rownames(iris))
  expect_false(object_has_rownames(data.frame()))
  expect_error(object_has_rownames(list(x = 1, y = 2)))
})

test_that("object_has_* helpers, additional test", {
  skip_if_not_installed("psych")
  expect_true(object_has_rownames(psych::bfi))
})
