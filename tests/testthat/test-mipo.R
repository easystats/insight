skip_on_cran()
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

  expect_snapshot(get_parameters(pooled))
  expect_snapshot(get_statistic(pooled))
  expect_identical(find_parameters(pooled), list(conditional = c("(Intercept)", "disp", "hp")))
})
