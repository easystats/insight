if (requiet("testthat") && requiet("insight")) {

  test_that("format_table with ci-level", {
    d <- data.frame(CI = 0.97, CI_low = 1, CI_high = 3)
    ft <- insight::format_table(d)
    expect_equal(colnames(ft), "97% CI")

    d$CI <- 0.788
    ft <- insight::format_table(d)
    expect_equal(colnames(ft), "78.8% CI")

    d$CI <- NULL
    attr(d, "ci") <- 0.9
    ft <- insight::format_table(d)
    expect_equal(colnames(ft), "90% CI")
  })

  test_that("format_table with multiple ci-levels", {
    d <- data.frame(CI_low_0.97 = 1, CI_high_0.97 = 3,
                    CI_low_0.2 = 1, CI_high_0.2 = 3)
    ft <- insight::format_table(d)
    expect_equal(colnames(ft), c("97% CI", "20% CI"))
  })


  test_that("format_table with si-level", {
    d <- data.frame(CI = 0.97, CI_low = 1, CI_high = 3)
    attr(d, "ci_method") <- "SI"
    ft <- insight::format_table(d)
    expect_equal(colnames(ft), "BF = 0.97 SI")

    d$CI <- 0.788
    ft <- insight::format_table(d)
    expect_equal(colnames(ft), "BF = 0.788 SI")

    d$CI <- NULL
    attr(d, "ci") <- 0.9
    ft <- insight::format_table(d)
    expect_equal(colnames(ft), "BF = 0.9 SI")
  })


  test_that("format_table with multiple si-levels", {
    d <- data.frame(CI_low_3 = 1, CI_high_3 = 3,
                    CI_low_0.2 = 1, CI_high_0.2 = 3)
    attr(d, "ci_method") <- "SI"
    ft <- insight::format_table(d)
    expect_equal(colnames(ft), c("BF = 3 SI", "BF = 0.2 SI"))
  })

  .runThisTest <- Sys.getenv("RunAllinsightTests") == "yes"
  if (.runThisTest && requiet("bayestestR")) {
    set.seed(1234)
    test_that("format_table with multiple si-levels", {
      d <- bayestestR::distribution_normal(1000)
      x <- bayestestR::hdi(d, ci = c(.80, .90))
      out <- capture.output(print(x))
      expect_equal(
        out,
        c("Highest Density Interval",
          "",
          "80% HDI       |       90% HDI",
          "-----------------------------",
          "[-1.28, 1.28] | [-1.65, 1.64]"
        )
      )
    })
  }
}
