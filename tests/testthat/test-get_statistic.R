test_that("get_statistic warns, not errors, when no summary method", {
  fit <- structure(list(), class = "foo")
  summary.foo <- function(object, ...) {
    list(fixef = "a coefmat", ranef = "another coefmat")
  }
  expect_warning(get_statistic(fit), regex = "Could not access")
})
