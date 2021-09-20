if (requiet("testthat") && requiet("insight")) {
  d <- data.frame(CI = 0.97, CI_low = 1, CI_high = 3)
  test_that("format_table with ci-level", {
    ft <- insight::format_table(d)
    expect_equal(colnames(ft), "97% CI")

    d$CI <- 0.788
    ft <- insight::format_table(d)
    expect_equal(colnames(ft), "78.8% CI")
  })
}
