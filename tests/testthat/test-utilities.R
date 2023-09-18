test_that("trim_ws() works with data frames", {
  test_trim <- data.frame(
    x = 1:5,
    y = c("a", "b c", " d", "e f ", "g "),
    stringsAsFactors = FALSE
  )
  expect_identical(
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
  expect_identical(
    trim_ws(test_trim),
    list(
      x = 1:5,
      y = c("a", "b c", "d", "e f", "g")
    )
  )
})

test_that("n_unique() works with NULL", {
  expect_identical(n_unique(NULL), 0)
})

test_that("n_unique() works with list", {
  test_n_unique <- list(
    x = 1:3,
    y = c("a", "b")
  )
  expect_identical(
    n_unique(test_n_unique),
    list(x = 3L, y = 2L)
  )
})

test_that("has_single_value() works", {
  x <- c(1, 1)
  expect_true(has_single_value(x))

  x <- c("a", "a")
  expect_true(has_single_value(x))

  x <- factor(c("a", "a"))
  expect_true(has_single_value(x))

  x <- c(NA, 1)
  expect_false(has_single_value(x))
  expect_true(has_single_value(x, na.rm = TRUE))

  x <- c(2, 1)
  expect_false(has_single_value(x))

  x <- NULL
  expect_false(has_single_value(x))

  x <- c(NA, NA)
  expect_false(has_single_value(x))
  expect_false(has_single_value(x, na.rm = TRUE))
})

test_that("safe_deparse_symbol() works", {
  expect_identical(safe_deparse_symbol(as.name("test")), "test")
  expect_null(safe_deparse_symbol("test"))
  expect_identical(safe_deparse(as.name("test")), "test")
  expect_identical(safe_deparse("test"), "\"test\"")
})

test_that("trim_ws() works with non-ascii chars", {
  expect_identical(
    trim_ws(c("test ", " Se\x96ora ", "works \x97fine ", "this too", "yeah")),
    c("test", "Se\x96ora", "works \x97fine", "this too", "yeah")
  )
})
