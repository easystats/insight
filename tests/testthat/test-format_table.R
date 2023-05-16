skip_if_offline()
skip_on_os(c("mac", "linux", "solaris"))
skip_if_not_installed("bayestestR")

# test for bayesian models -----------------
m1 <- insight::download_model("stanreg_glm_1")
set.seed(123)
x <- suppressWarnings(as.data.frame(bayestestR::describe_posterior(m1, test = c("pd", "bf"))))

test_that("format_table with stars bayes", {
  out <- format_table(x)
  expect_identical(colnames(out), c("Parameter", "Median", "95% CI", "pd", "Rhat", "ESS", "BF"))
  expect_identical(out$BF, c("62.73", "114.21"))
  expect_identical(out$pd, c("99.98%", "100%"))

  out <- format_table(x, stars = TRUE)
  expect_identical(colnames(out), c("Parameter", "Median", "95% CI", "pd", "Rhat", "ESS", "BF"))
  expect_identical(out$BF, c("62.73***", "114.21***"))
  expect_identical(out$pd, c("99.98%***", "100%***"))

  out <- format_table(x, stars = c("pd", "BF"))
  expect_identical(colnames(out), c("Parameter", "Median", "95% CI", "pd", "Rhat", "ESS", "BF"))
  expect_identical(out$BF, c("62.73***", "114.21***"))
  expect_identical(out$pd, c("99.98%***", "100%***"))

  out <- format_table(x, stars = "pd")
  expect_identical(colnames(out), c("Parameter", "Median", "95% CI", "pd", "Rhat", "ESS", "BF"))
  expect_identical(out$BF, c("62.73", "114.21"))
  expect_identical(out$pd, c("99.98%***", "100%***"))

  out <- format_table(x, stars = "BF")
  expect_identical(colnames(out), c("Parameter", "Median", "95% CI", "pd", "Rhat", "ESS", "BF"))
  expect_identical(out$BF, c("62.73***", "114.21***"))
  expect_identical(out$pd, c("99.98%", "100%"))
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
