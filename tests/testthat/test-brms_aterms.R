skip_on_cran()
skip_if_offline()
skip_on_os("mac")
skip_if_not_installed("brms")
skip_if_not_installed("httr2")

# Model fitting -----------------------------------------------------------

aterm_1 <- suppressWarnings(download_model("brms_aterm_1"))
aterm_2 <- suppressWarnings(download_model("brms_aterm_2"))
aterm_3 <- suppressWarnings(download_model("brms_aterm_3"))
aterm_4 <- suppressWarnings(download_model("brms_aterm_4"))

all_loaded <- !vapply(list(aterm_1, aterm_2, aterm_3, aterm_4), is.null, TRUE)
skip_if(!all(all_loaded))

# Tests -------------------------------------------------------------------
test_that("get_response brms aterms-trials 1", {
  expect_identical(find_response(aterm_1), "am")
  expect_length(get_response(aterm_1, source = "env"), 32)
  expect_length(get_response(aterm_1, source = "mf"), 32)
})

test_that("get_response brms aterms-trials 2", {
  expect_identical(find_response(aterm_2), c("am", "cyl"))
  expect_named(get_response(aterm_2, source = "env"), c("am", "cyl"))
  expect_named(get_response(aterm_2, source = "mf"), c("am", "cyl"))
  expect_identical(nrow(get_response(aterm_2)), 32L)
})

test_that("get_response brms aterms-cens 1", {
  expect_identical(find_response(aterm_3), c("time", "censored"))
  expect_named(get_response(aterm_3, source = "env"), c("time", "censored"))
  expect_named(get_response(aterm_3, source = "mf"), c("time", "censored"))
  expect_identical(nrow(get_response(aterm_3)), 76L)
})

test_that("get_response brms aterms-cens 2", {
  expect_identical(find_response(aterm_4), c("time", "censored"))
  expect_named(get_response(aterm_4, source = "env"), c("time", "censored"))
  expect_named(get_response(aterm_4, source = "mf"), c("time", "censored"))
  expect_identical(nrow(get_response(aterm_4)), 76L)
})
