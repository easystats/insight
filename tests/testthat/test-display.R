test_that("display, matrix", {
  skip_if_not_installed("knitr")
  mdat <- matrix(
    c(1, 2, 3, 11, 12, 13),
    nrow = 2,
    ncol = 3,
    byrow = TRUE,
    dimnames = list(c("row1", "row2"), c("C.1", "C.2", "C.3"))
  )
  expect_snapshot(as.character(display(mdat)))
  expect_snapshot(as.character(print_md(mdat)))
})


test_that("display, table", {
  skip_if_not_installed("knitr")
  skip_if_not_installed("datawizard", minimum_version = "1.2.0")
  set.seed(123)
  dance_tib <- data.frame(
    dance = sample(c("salsa", "hiphop"), size = 200, replace = TRUE),
    training = sample(c("yes", "no"), size = 200, replace = TRUE)
  )

  cat_xtbl <- datawizard::data_tabulate(
    x = dance_tib,
    select = "training",
    by = "dance",
    remove_na = TRUE
  )
  cat_chi <- chisq.test(as.table(cat_xtbl, simplify = TRUE))
  res <- get_residuals(cat_chi)
  expect_snapshot(as.character(display(res)))
  expect_snapshot(as.character(print_md(res)))
})
