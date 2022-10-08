test_that("trim_ws() works with data frames", {
  test_trim <- data.frame(
    x = 1:5,
    y = c("a", "b c", " d", "e f ", "g "),
    stringsAsFactors = FALSE
  )
  expect_equal(
    trim_ws(test_trim),
    data.frame(
      x = 1:5,
      y = c("a", "b c", "d", "e f", "g"),
      stringsAsFactors = FALSE
    )
  )
})

test_that("trim_ws() works with list", {
  test_trim <- list(
    x = 1:5,
    y = c("a", "b c", " d", "e f ", "g ")
  )
  expect_equal(
    trim_ws(test_trim),
    list(
      x = 1:5,
      y = c("a", "b c", "d", "e f", "g")
    )
  )
})

test_that("n_unique() works with NULL", {
  expect_equal(n_unique(NULL), 0)
})

test_that("n_unique() works with list", {
  test_n_unique <- list(
    x = 1:3,
    y = c("a", "b")
  )
  expect_equal(
    n_unique(test_n_unique),
    list(x = 3, y = 2)
  )
})
