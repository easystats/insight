skip_if_offline()
skip_on_os(c("mac", "linux", "solaris"))
skip_if_not_installed("bayestestR")
skip_if_not_installed("rstanarm")
skip_if_not_installed("httr")

# test for bayesian models -----------------
m1 <- insight::download_model("stanreg_glm_1")
skip_if(is.null(m1))

test_that("format_table with stars bayes", {
  set.seed(123)
  x <- suppressWarnings(as.data.frame(bayestestR::describe_posterior(m1, test = c("pd", "bf"))))

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


test_that("format_table with column order", {
  set.seed(123)
  x <- suppressWarnings(as.data.frame(bayestestR::describe_posterior(m1, test = c("pd", "bf", "rope"))))

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
test_that("format_table with stars freq", {
  skip_if_not_installed("parameters")
  x <- as.data.frame(parameters::model_parameters(lm(Sepal.Length ~ Species + Sepal.Width, data = iris)))

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
test_that("formatting ROPE CI", {
  skip_if_not_installed("parameters")
  data(iris)
  d <- iris
  d$Sepal.Length10 <- 10 * d$Sepal.Length
  m10 <- lm(Sepal.Length10 ~ Sepal.Width + Species, data = d)
  expect_snapshot(print(parameters::equivalence_test(m10)))
})


test_that("reorder columns BF", {
  # brms_bf <- suppressWarnings(download_model("brms_bf_1"))
  out <- data.frame(
    Parameter = c("b_Intercept", "b_wt", "sigma"),
    Component = c("conditional", "conditional", "sigma"),
    Median = c(32.22175, -3.755645, 3.461165),
    CI = c(0.95, 0.95, 0.95),
    CI_low = c(27.2244525, -4.9688055, 2.6517275),
    CI_high = c(35.75887, -2.21074025, 4.69652725),
    pd = c(1, 1, 1),
    ROPE_Percentage = c(0, 0, 0),
    log_BF = c(14.4924732349718, 5.79962753110103, 8.89383915455679),
    Rhat = c(1.00438747198895, 1.00100407213689, 0.992006699276081),
    ESS = c(88.3152312142069, 91.7932788446396, 167.822262320689),
    stringsAsFactors = FALSE
  )

  expect_snapshot(format_table(out), variant = "windows")
})
