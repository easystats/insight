if (require("testthat") &&
  require("insight") &&
  require("stats") &&
  require("parameters")) {
  .runThisTest <- Sys.getenv("RunAllinsightTests") == "yes"

  test_that("standardize_names works", {
    set.seed(123)

    # lm object
    lm_mod <- lm(wt ~ mpg, mtcars)
    x <- as.data.frame(parameters::model_parameters(lm_mod))

    if (packageVersion("parameters") > "0.11.0") {
      expect_equal(
        names(standardize_names(x, style = "broom")),
        c(
          "term", "estimate", "std.error", "ci.width", "conf.low", "conf.high",
          "statistic", "df.error", "p.value"
        )
      )
    }

    # aov object
    aov_mod <- aov(wt ~ mpg, mtcars)
    y <- as.data.frame(parameters::model_parameters(aov_mod))

    expect_equal(
      names(standardize_names(y, style = "broom")),
      c("term", "sumsq", "df", "meansq", "statistic", "p.value")
    )
  })


  ## TODO remove once on CRAN

  # t-test (this is yet to be finalized)
  if (packageVersion("parameters") > "0.11.0") {
    z <- as.data.frame(parameters::model_parameters(t.test(1:10, y = c(7:20))))

    expect_equal(
      names(standardize_names(z, style = "broom")),
      c(
        "parameter1", "parameter2", "mean.parameter1", "mean.parameter2", "estimate",
        "ci.width", "conf.low", "conf.high", "statistic", "df.error", "p.value", "method"
      )
    )
  }

  # chi-square test
  chi <- as.data.frame(parameters::model_parameters(chisq.test(matrix(c(12, 5, 7, 7), ncol = 2))))

  expect_equal(
    names(standardize_names(chi, style = "broom")),
    c("statistic", "df", "p.value", "method")
  )
}
