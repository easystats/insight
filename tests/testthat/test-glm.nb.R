if (requiet("testthat") && requiet("insight") && requiet("MASS")) {
  data(quine)
  set.seed(123)
  m1 <- glm.nb(Days ~ Sex / (Age + Eth * Lrn), data = quine)

  if (requiet("parameters")) {
    test_that("get_df", {
      expect_equal(
        get_df(m1, type = "residual"),
        parameters::degrees_of_freedom(m1, method = "residual"),
        ignore_attr = TRUE
      )
      expect_equal(
        get_df(m1, type = "normal"),
        parameters::degrees_of_freedom(m1, method = "normal"),
        ignore_attr = TRUE
      )
      expect_equal(
        get_df(m1, type = "analytical"),
        parameters::degrees_of_freedom(m1, method = "analytical"),
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
