if (requiet("testthat") && requiet("insight") && requiet("MASS")) {
  data(quine)
  set.seed(123)
  m1 <- glm.nb(Days ~ Sex / (Age + Eth * Lrn), data = quine)

  if (requiet("parameters")) {
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
        parameters::degrees_of_freedom(m1, method = "wald"),
        ignore_attr = TRUE
      )
    })
  }
}
