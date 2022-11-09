test_that("there are no duplicate entries in supported models", {
  expect_equal(anyDuplicated(supported_models()), 0L)
})
