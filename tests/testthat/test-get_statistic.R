test_that("get_statistic warns, not errors, when no summary method", {
  fit <- structure(list(), class = "foo")
  expect_warning(get_statistic(fit), regex = "Could not access")
})
