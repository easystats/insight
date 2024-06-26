test_that("format_message", {
  foo1 <- function(msg) {
    format_warning("This warning waits.")
    cat(msg)
  }
  foo2 <- function(msg) {
    format_warning("This warning is in a hurry.", immediate = TRUE)
    cat(msg)
  }
  expect_snapshot(foo1("we can wait\n"))
  expect_snapshot(foo2("we don't want to wait\n"))
})
