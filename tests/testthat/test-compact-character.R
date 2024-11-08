test_that("compact character works as expected", {
  expect_identical(compact_character(c("x", "y", NA)), c("x", "y"))
  expect_identical(compact_character(c("x", "y", NA_real_)), c("x", "y"))
  expect_identical(compact_character(c("x", "NULL", "", "y")), c("x", "y"))

  # scalar
  expect_identical(compact_character(""), character(0))

  # date
  x <- as.Date(c("1900-05-06", "", "1900-05-07", NA))
  expect_identical(compact_character(x), structure(c(-25442, -25441), class = "Date"))
})
