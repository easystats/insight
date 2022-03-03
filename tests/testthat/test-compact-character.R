test_that("compact character works as expected", {
  expect_equal(compact_character(c("x", "y", NA)), c("x", "y"))
  expect_equal(compact_character(c("x", "y", NA_real_)), c("x", "y"))
  expect_equal(compact_character(c("x", "NULL", "", "y")), c("x", "y"))

  # scalar
  expect_equal(compact_character(""), character(0))
})
