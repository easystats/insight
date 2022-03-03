test_that("is_empty_object works as expected", {
  # not empty
  expect_false(is_empty_object(list(NULL, 1, c(NA, NA))))
  expect_false(is_empty_object(c(1, NA, NA)))
  expect_false(is_empty_object(""))

  # empty
  expect_true(is_empty_object(logical(0)))
  expect_true(is_empty_object(NULL))
  expect_true(is_empty_object(list(NULL, list(NULL, NULL))))
  expect_true(is_empty_object(NA))
  expect_true(is_empty_object(c()))
  expect_true(is_empty_object(list()))
})
