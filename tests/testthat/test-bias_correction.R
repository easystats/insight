skip_on_cran()
skip_if_not_installed("lme4")

test_that("get_predicted bias_correction", {
  set.seed(123)
  dat <- data.frame(
    outcome = rbinom(n = 100, size = 1, prob = 0.35),
    var_binom = as.factor(rbinom(n = 100, size = 1, prob = 0.7)),
    var_cont = rnorm(n = 100, mean = 10, sd = 7),
    grp = as.factor(sample(letters[1:4], size = 100, replace = TRUE))
  )
  m1 <- lme4::glmer(
    outcome ~ var_binom + var_cont + (1 | grp),
    data = dat,
    family = binomial(link = "logit")
  )
  d <- get_datagrid(m1, "var_binom", include_random = TRUE)
  out <- as.data.frame(get_predicted(m1, data = d, ci = 0.95))
  expect_equal(out$Predicted, c(0.34234, 0.32517), tolerance = 1e-4)
  out <- as.data.frame(get_predicted(m1, data = d, ci = 0.95, bias_correction = TRUE))
  expect_equal(out$Predicted, c(0.45912, 0.45138), tolerance = 1e-4)
  out <- as.data.frame(get_predicted(m1, data = d, ci = 0.95, bias_correction = TRUE, sigma = 2.5))
  expect_equal(out$Predicted, c(0.56419, 0.56494), tolerance = 1e-4)
})
