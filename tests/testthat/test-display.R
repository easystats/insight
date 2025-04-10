test_that("display, matrix", {
  mdat <- matrix(
    c(1, 2, 3, 11, 12, 13),
    nrow = 2,
    ncol = 3,
    byrow = TRUE,
    dimnames = list(c("row1", "row2"), c("C.1", "C.2", "C.3"))
  )
  expect_snapshot(display(mdat))
  expect_snapshot(print_md(mdat))
})
