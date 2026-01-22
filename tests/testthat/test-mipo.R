skip_on_cran()
skip_on_os("mac")
skip_if_not_installed("mice")
skip_if_not_installed("nnet")

test_that("param", {
  set.seed(1234)
  d <- suppressWarnings(mice::ampute(mtcars)) ## Ampute mtcars and impute two data sets
  imp <- suppressWarnings(mice::mice(d$amp, m = 2, printFlag = FALSE))
  imp.l <- mice::complete(imp, action = "long")
  model <- list() ## Fit and pool models
  for (i in 1:2) {
    capture.output({
      model[[i]] <- nnet::multinom(cyl ~ disp + hp, data = imp.l, subset = .imp == i)
    })
  }
  pooled <- mice::pool(model)
  out1 <- get_parameters(pooled)
  out2 <- get_statistic(pooled)
  out1$Estimate <- round(out1$Estimate, 4)
  out2$Statistic <- round(out2$Statistic, 4)
  expect_equal(out1$Response, c("6", "6", "6", "8", "8", "8"))
  expect_equal(out2$Response, c("6", "6", "6", "8", "8", "8"))
  expect_identical(
    capture.output(out1),
    c(
      "    Parameter  Estimate Response",
      "1 (Intercept)  -64.4763        6",
      "2        disp    0.2340        6",
      "3          hp    0.2832        6",
      "4 (Intercept) -124.0253        8",
      "5        disp    0.2899        8",
      "6          hp    0.6234        8"
    )
  )
  expect_identical(
    capture.output(out2),
    c(
      "    Parameter Statistic Response",
      "1 (Intercept)   -0.9588        6",
      "2        disp    0.5503        6",
      "3          hp    0.4947        6",
      "4 (Intercept)   -1.6898        8",
      "5        disp    0.4413        8",
      "6          hp    0.6006        8"
    )
  )
  expect_identical(
    find_parameters(pooled),
    list(conditional = c("(Intercept)", "disp", "hp"))
  )
})
