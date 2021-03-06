if (require("testthat") &&
  require("insight") &&
  require("stats") &&
  require("parameters")) {
  .runThisTest <- Sys.getenv("RunAllinsightTests") == "yes"

  if (packageVersion("parameters") >= "0.14.0") {
    test_that("standardize_names works", {
      set.seed(123)

      # lm object
      lm_mod <- lm(wt ~ mpg, mtcars)
      x <- as.data.frame(parameters::model_parameters(lm_mod))

      expect_equal(
        names(standardize_names(x, style = "broom")),
        c(
          "term", "estimate", "std.error", "conf.level", "conf.low", "conf.high",
          "statistic", "df.error", "p.value"
        )
      )

      expect_equal(
        names(standardize_names(x, style = "easystats")),
        c(
          "Parameter", "Coefficient", "SE", "CI", "CI_low", "CI_high",
          "Statistic", "df", "p"
        )
      )

      # aov object
      aov_mod <- aov(wt ~ mpg, mtcars)
      y <- as.data.frame(parameters::model_parameters(aov_mod))

      expect_equal(
        names(standardize_names(y, style = "broom")),
        c("term", "sumsq", "df", "meansq", "statistic", "p.value")
      )
    })


    # t-test (this is yet to be finalized)
    z <- as.data.frame(parameters::model_parameters(t.test(1:10, y = c(7:20))))

    ## TODO enable later

    # expect_equal(
    #   names(standardize_names(z, style = "broom")),
    #   c(
    #     "parameter1", "parameter2", "mean.parameter1", "mean.parameter2", "estimate",
    #     "conf.level", "conf.low", "conf.high", "statistic", "df.error", "p.value",
    #     "method", "alternative"
    #   )
    # )

    # chi-square test
    chi <- as.data.frame(parameters::model_parameters(chisq.test(matrix(c(12, 5, 7, 7), ncol = 2))))

    # expect_equal(
    #   names(standardize_names(chi, style = "broom")),
    #   c("statistic", "df", "p.value", "method", "alternative")
    # )
  }
}
