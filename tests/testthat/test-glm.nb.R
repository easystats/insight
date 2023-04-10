skip_if_not_installed("MASS")
data(quine, package = "MASS")
set.seed(123)
m1 <- MASS::glm.nb(Days ~ Sex / (Age + Eth * Lrn), data = quine)

test_that("get_df", {
  expect_equal(
    get_df(m1, type = "residual"),
    df.residual(m1),
    ignore_attr = TRUE
  )
  expect_equal(
    get_df(m1, type = "normal"),
    Inf,
    ignore_attr = TRUE
  )
  expect_equal(
    get_df(m1, type = "wald"),
    Inf,
    ignore_attr = TRUE
  )
})
