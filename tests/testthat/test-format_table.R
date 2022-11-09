.runThisTest <- Sys.getenv("RunAllinsightTests") == "yes"

win_os <- tryCatch(
  {
    si <- Sys.info()
    if (!is.null(si["sysname"])) {
      si["sysname"] == "Windows" || grepl("^mingw", R.version$os)
    } else {
      FALSE
    }
  },
  error = function(e) {
    TRUE
  }
)

if (requiet("testthat") && requiet("insight")) {
  # test for bayesian models -----------------
  if (.runThisTest && win_os && requiet("bayestestR")) {
    m1 <- insight::download_model("stanreg_glm_1")
    set.seed(123)
    x <- as.data.frame(bayestestR::describe_posterior(m1, test = c("pd", "bf")))

    test_that("format_table with stars bayes", {
      out <- format_table(x)
      expect_equal(colnames(out), c("Parameter", "Median", "95% CI", "pd", "Rhat", "ESS", "BF"))
      expect_equal(out$BF, c("62.73", "114.21"))
      expect_equal(out$pd, c("99.98%", "100%"))

      out <- format_table(x, stars = TRUE)
      expect_equal(colnames(out), c("Parameter", "Median", "95% CI", "pd", "Rhat", "ESS", "BF"))
      expect_equal(out$BF, c("62.73***", "114.21***"))
      expect_equal(out$pd, c("99.98%***", "100%***"))

      out <- format_table(x, stars = c("pd", "BF"))
      expect_equal(colnames(out), c("Parameter", "Median", "95% CI", "pd", "Rhat", "ESS", "BF"))
      expect_equal(out$BF, c("62.73***", "114.21***"))
      expect_equal(out$pd, c("99.98%***", "100%***"))

      out <- format_table(x, stars = "pd")
      expect_equal(colnames(out), c("Parameter", "Median", "95% CI", "pd", "Rhat", "ESS", "BF"))
      expect_equal(out$BF, c("62.73", "114.21"))
      expect_equal(out$pd, c("99.98%***", "100%***"))

      out <- format_table(x, stars = "BF")
      expect_equal(colnames(out), c("Parameter", "Median", "95% CI", "pd", "Rhat", "ESS", "BF"))
      expect_equal(out$BF, c("62.73***", "114.21***"))
      expect_equal(out$pd, c("99.98%", "100%"))
    })
  }

  # test for freq models -----------------
  if (requiet("parameters")) {
    x <- as.data.frame(model_parameters(lm(Sepal.Length ~ Species + Sepal.Width, data = iris)))

    test_that("format_table with stars freq", {
      out <- format_table(x)
      expect_equal(colnames(out), c("Parameter", "Coefficient", "SE", "95% CI", "t(146)", "p"))
      expect_equal(out$p, c("< .001", "< .001", "< .001", "< .001"))

      out <- format_table(x, stars = TRUE)
      expect_equal(out$p, c("< .001***", "< .001***", "< .001***", "< .001***"))

      out <- format_table(x, stars = c("pd", "BF"))
      expect_equal(out$p, c("< .001", "< .001", "< .001", "< .001"))

      out <- format_table(x, stars = "pd")
      expect_equal(out$p, c("< .001", "< .001", "< .001", "< .001"))

      out <- format_table(x, stars = c("BF", "p"))
      expect_equal(out$p, c("< .001***", "< .001***", "< .001***", "< .001***"))
    })
  }
}
