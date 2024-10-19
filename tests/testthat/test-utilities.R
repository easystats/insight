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
  expect_true(has_single_value(x, remove_na = TRUE))

  x <- c(2, 1)
  expect_false(has_single_value(x))

  x <- NULL
  expect_false(has_single_value(x))

  x <- c(NA, NA)
  expect_false(has_single_value(x))
  expect_false(has_single_value(x, remove_na = TRUE))
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

skip_if_not_installed("withr")

withr::with_environment(
  new.env(),
  test_that("validate_argument", {
    foo1 <- function(test = "short_distance") {
      validate_argument(test, c("short_distance", "long_distance", "medium_distance"))
    }
    # match
    expect_identical(foo1("medium_distance"), "medium_distance")
    # typo
    expect_error(
      foo1("medium_ditsance"),
      regex = "Otherwise, use one"
    )
    # no match
    expect_error(
      foo1("abcabcabc"),
      regex = "Please use one"
    )

    foo2 <- function(test = "short_distance") {
      validate_argument(test, "short_distance")
    }
    # match
    expect_identical(foo2("short_distance"), "short_distance")
    # typo
    expect_error(
      foo2("short_ditsance"),
      regex = "Did you mean \"short"
    )
    # no match
    expect_error(
      foo2("abcabcabcabc"),
      regex = "Please use one"
    )

    foo3 <- function(test = "short_distance") {
      validate_argument(test, c("short_distance", "shorter_distance", "shortest_distance"))
    }
    # match
    expect_identical(foo3("short_distance"), "short_distance")
    # typo
    expect_error(
      foo3("short_ditsance"),
      regex = "Did you mean one"
    )
    # no match
    expect_error(
      foo3("aaaaaaaaaaaaaaaaaaaa"),
      regex = "Please use one"
    )
  })
)
