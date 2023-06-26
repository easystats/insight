skip_if_offline()
skip_on_os(c("mac", "linux", "solaris"))
skip_if_not_installed("bayestestR")
skip_if_not_installed("rstanarm")

# test for bayesian models -----------------
m1 <- insight::download_model("stanreg_glm_1")
skip_if(is.null(m1))

set.seed(123)
x <- suppressWarnings(as.data.frame(bayestestR::describe_posterior(m1, test = c("pd", "bf"))))

test_that("format_table with stars bayes", {
  out <- format_table(x)
  expect_identical(colnames(out), c("Parameter", "Median", "95% CI", "pd", "BF", "Rhat", "ESS"))
  expect_identical(out$BF[2], "114.21")
  expect_identical(out$pd, c("99.98%", "100%"))

  out <- format_table(x, stars = TRUE)
  expect_identical(colnames(out), c("Parameter", "Median", "95% CI", "pd", "BF", "Rhat", "ESS"))
  expect_identical(out$BF[2], "114.21***")
  expect_identical(out$pd, c("99.98%***", "100%***"))

  out <- format_table(x, stars = c("pd", "BF"))
  expect_identical(colnames(out), c("Parameter", "Median", "95% CI", "pd", "BF", "Rhat", "ESS"))
  expect_identical(out$BF[2], "114.21***")
  expect_identical(out$pd, c("99.98%***", "100%***"))

  out <- format_table(x, stars = "pd")
  expect_identical(colnames(out), c("Parameter", "Median", "95% CI", "pd", "BF", "Rhat", "ESS"))
  expect_identical(out$BF[2], "114.21")
  expect_identical(out$pd, c("99.98%***", "100%***"))

  out <- format_table(x, stars = "BF")
  expect_identical(colnames(out), c("Parameter", "Median", "95% CI", "pd", "BF", "Rhat", "ESS"))
  expect_identical(out$BF[2], "114.21***")
  expect_identical(out$pd, c("99.98%", "100%"))
})


set.seed(123)
x <- suppressWarnings(as.data.frame(bayestestR::describe_posterior(m1, test = c("pd", "bf", "rope"))))

test_that("format_table with column order", {
  out <- format_table(x)
  expect_named(
    out,
    c(
      "Parameter", "Median", "95% CI", "pd", "ROPE", "% in ROPE",
      "BF", "Rhat", "ESS"
    )
  )
  expect_named(
    standardize_column_order(x),
    c(
      "Parameter", "Median", "CI", "CI_low", "CI_high", "pd", "ROPE_CI",
      "ROPE_low", "ROPE_high", "ROPE_Percentage", "log_BF", "Rhat",
      "ESS"
    )
  )
})


# test for freq models -----------------
skip_if_not_installed("parameters")
x <- as.data.frame(parameters::model_parameters(lm(Sepal.Length ~ Species + Sepal.Width, data = iris)))

test_that("format_table with stars freq", {
  out <- format_table(x)
  expect_identical(colnames(out), c("Parameter", "Coefficient", "SE", "95% CI", "t(146)", "p"))
  expect_identical(out$p, c("< .001", "< .001", "< .001", "< .001"))

  out <- format_table(x, stars = TRUE)
  expect_identical(out$p, c("< .001***", "< .001***", "< .001***", "< .001***"))

  out <- format_table(x, stars = c("pd", "BF"))
  expect_identical(out$p, c("< .001", "< .001", "< .001", "< .001"))

  out <- format_table(x, stars = "pd")
  expect_identical(out$p, c("< .001", "< .001", "< .001", "< .001"))

  out <- format_table(x, stars = c("BF", "p"))
  expect_identical(out$p, c("< .001***", "< .001***", "< .001***", "< .001***"))
})

# test for freq models -----------------
skip_if_not_installed("parameters")
test_that("formatting ROPE CI", {
  data(iris)
  d <- iris
  d$Sepal.Length10 <- 10 * d$Sepal.Length
  m10 <- lm(Sepal.Length10 ~ Sepal.Width + Species, data = d)
  expect_snapshot(print(parameters::equivalence_test(m10)))
})
