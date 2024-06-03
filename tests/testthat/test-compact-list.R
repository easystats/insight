test_that("compact_list works as expected", {
  expect_identical(compact_list(list(NULL, 1, c(NA, NA))), list(1, c(NA, NA)))
  expect_identical(compact_list(c(1, NA, NA)), c(1, NA, NA))
  expect_identical(compact_list(list(NULL, 1, list(NULL, NULL))), list(1))
  expect_identical(compact_list(c(1, NA, NA), remove_na = TRUE), 1)
  expect_identical(compact_list(c(1, 2, 3), remove_na = TRUE), c(1, 2, 3))
  expect_identical(compact_list(""), "")
  expect_null(compact_list(NULL))
  expect_identical(compact_list(logical(0)), logical(0))
})

test_that("compact_list, logical > 1", {
  x <- list(a = 1, b = c(1, 2), c = NA)
  expect_identical(compact_list(x, remove_na = TRUE), list(a = 1, b = c(1, 2)))
  expect_identical(compact_list(x, remove_na = FALSE), list(a = 1, b = c(1, 2), c = NA))
  x <- list(a = 1, b = c(NA, NA), c = NA)
  expect_identical(compact_list(x, remove_na = TRUE), list(a = 1))
  expect_identical(compact_list(x, remove_na = FALSE), list(a = 1, b = c(NA, NA), c = NA))
})

test_that("compact_list, vctrs", {
  data(mtcars)
  class(mtcars$mpg) <- c("haven_labelled", "vctrs_vctr", "double")
  attr(mtcars$mpg, "labels") <- c(`21` = 21)
  out <- compact_list(mtcars)
  expect_true(all(vapply(out, class, character(1)) == "numeric"))
})
