skip_if_not_or_load_if_installed("logitr")
skip_if_not_installed("logitr", minimum_version = "0.8.0")

mod <- suppressMessages(logitr::logitr(
  data    = yogurt,
  outcome = "choice",
  obsID   = "obsID",
  pars    = c("price", "feat", "brand")
))

test_that("minimalist tests", {
  d <- get_data(mod)
  expect_s3_class(d, "data.frame")
  v <- find_variables(mod)
  expect_equal(v$response, "choice")
  expect_equal(v$conditional, c("price", "feat", "brand"))
  expect_equal(v$cluster, "obsID")
})
