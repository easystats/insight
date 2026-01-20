test_that("display, matrix", {
  skip_if_not_installed("knitr")
  mdat <- matrix(
    c(1, 2, 3, 11, 12, 13),
    nrow = 2,
    ncol = 3,
    byrow = TRUE,
    dimnames = list(c("row1", "row2"), c("C.1", "C.2", "C.3"))
  )
  expect_identical(
    as.character(display(mdat)),
    c(
      "|     | C.1| C.2| C.3|",
      "|:----|---:|---:|---:|",
      "|row1 |   1|   2|   3|",
      "|row2 |  11|  12|  13|"
    )
  )
  expect_identical(
    as.character(print_md(mdat)),
    c(
      "|     | C.1| C.2| C.3|",
      "|:----|---:|---:|---:|",
      "|row1 |   1|   2|   3|",
      "|row2 |  11|  12|  13|"
    )
  )
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
  expect_identical(
    as.character(display(res)),
    c(
      "|    | hiphop| salsa|",
      "|:---|------:|-----:|",
      "|no  |  -0.99|  0.96|",
      "|yes |   0.91| -0.89|"
    )
  )
  expect_identical(
    as.character(print_md(res)),
    c(
      "|    | hiphop| salsa|",
      "|:---|------:|-----:|",
      "|no  |  -0.99|  0.96|",
      "|yes |   0.91| -0.89|"
    )
  )
})
