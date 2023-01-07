test_that("compact_list works as expected", {
  expect_equal(compact_list(list(NULL, 1, c(NA, NA))), list(1, c(NA, NA)))
  expect_equal(compact_list(c(1, NA, NA)), c(1, NA, NA))
  expect_equal(compact_list(list(NULL, 1, list(NULL, NULL))), list(1))
  expect_equal(compact_list(c(1, NA, NA), remove_na = TRUE), 1)
  expect_equal(compact_list(c(1, 2, 3), remove_na = TRUE), c(1, 2, 3))
  expect_equal(compact_list(""), "")
  expect_null(compact_list(NULL))
  expect_equal(compact_list(logical(0)), logical(0))
})

test_that("compact_list, logical > 1", {
  x <- list(a = 1, b = c(1, 2), c = NA)
  expect_equal(compact_list(x, remove_na = TRUE), list(a = 1, b = c(1, 2)))
  expect_equal(compact_list(x, remove_na = FALSE), list(a = 1, b = c(1, 2), c = NA))
  x <- list(a = 1, b = c(NA, NA), c = NA)
  expect_equal(compact_list(x, remove_na = TRUE), list(a = 1))
  expect_equal(compact_list(x, remove_na = FALSE), list(a = 1, b = c(NA, NA), c = NA))
})
