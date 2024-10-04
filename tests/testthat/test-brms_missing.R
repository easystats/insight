skip_on_cran()
skip_if_offline()
skip_on_os("mac")
skip_if_not_installed("brms")
skip_if_not_installed("httr2")

# Model fitting -----------------------------------------------------------

miss_1 <- suppressWarnings(download_model("brms_miss_1"))
skip_if(is.null(miss_1))

# Tests -------------------------------------------------------------------
test_that("get_response brms aterms-trials 1", {
  expect_equal(
    find_formula(miss_1),
    structure(
      list(
        survived = list(conditional = survived ~ woman * mi(age) + passengerClass),
        age = list(conditional = age | mi() ~ passengerClass + woman)
      ),
      is_mv = "1",
      class = c("insight_formula", "list")
    ),
    ignore_attr = TRUE
  )
  expect_identical(
    find_response(miss_1),
    c(survived = "survived", age = "age")
  )
  expect_true(is_multivariate(miss_1))
  out <- get_response(miss_1)
  expect_named(out, c("survived", "age"))
  expect_equal(head(out$age), c(29, 0.9167, 2, 30, 25, 48), tolerance = 1e-4, ignore_attr = TRUE)
  expect_equal(head(out$survived), c(1, 1, 0, 0, 0, 1), tolerance = 1e-4, ignore_attr = TRUE)
})
