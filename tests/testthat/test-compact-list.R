test_that("compact_list works as expected", {
  expect_identical(compact_list(list(NULL, 1, c(NA, NA))), list(1, c(NA, NA)))
  expect_identical(compact_list(c(1, NA, NA)), c(1, NA, NA))
  expect_identical(compact_list(list(NULL, 1, list(NULL, NULL))), list(1))
  expect_identical(compact_list(c(1, NA, NA), remove_na = TRUE), 1)
  expect_identical(compact_list(c(1, 2, 3), remove_na = TRUE), c(1, 2, 3))
  expect_identical(compact_list(""), "")
  expect_null(compact_list(NULL))
  expect_identical(compact_list(logical(0)), logical(0))
  # date
  x <- as.Date(c("1900-05-06", "", "1900-05-07", NA))
  y <- c(NA, 1, 5)
  expect_identical(
    compact_list(list(x, NULL, y)),
    list(structure(c(-25442, NA, -25441, NA), class = "Date"), c(NA, 1, 5))
  )
})

test_that("compact_list, logical > 1", {
  x <- list(a = 1, b = c(1, 2), c = NA)
  expect_identical(compact_list(x, remove_na = TRUE), list(a = 1, b = c(1, 2)))
  expect_identical(compact_list(x, remove_na = FALSE), list(a = 1, b = c(1, 2), c = NA))
  x <- list(a = 1, b = c(NA, NA), c = NA)
  expect_identical(compact_list(x, remove_na = TRUE), list(a = 1))
  expect_identical(compact_list(x, remove_na = FALSE), list(a = 1, b = c(NA, NA), c = NA))
})

test_that("compact_list, this must work!", {
  skip_if_not_installed("bayestestR")
  out <- lapply(
    mtcars[, 1:3, drop = FALSE],
    bayestestR::ci,
    ci = c(0.9, 0.8),
    verbose = FALSE
  )
  result <- compact_list(out)
  expect_identical(
    lapply(result, class),
    list(mpg = c(
      "bayestestR_eti", "see_eti", "bayestestR_ci", "see_ci",
      "data.frame"
    ), cyl = c(
      "bayestestR_eti", "see_eti", "bayestestR_ci",
      "see_ci", "data.frame"
    ), disp = c(
      "bayestestR_eti", "see_eti",
      "bayestestR_ci", "see_ci", "data.frame"
    ))
  )
})
