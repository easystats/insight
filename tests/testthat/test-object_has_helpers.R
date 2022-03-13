test_that("object_has_* helpers", {
  expect_true(object_has_names(mtcars, "am"))
  expect_equal(object_has_names(anscombe, c("x1", "z1", "y1")), c(TRUE, FALSE, TRUE))
  expect_equal(object_has_names(list("x" = 1, "y" = 2), c("x", "a")), c(TRUE, FALSE))

  expect_true(object_has_rownames(mtcars))
  expect_error(object_has_rownames(list("x" = 1, "y" = 2)))
})
