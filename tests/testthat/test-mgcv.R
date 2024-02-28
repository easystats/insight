skip_on_cran()
skip_if_not_installed("mgcv")

test_that("find_predictors", {
  set.seed(123)
  n <- 500
  xn <- rep(c(1, 2, 3), n)
  levels <- sort(unique(xn))
  labels <- c("low", "med", "high")
  x <- factor(xn, levels = levels, labels = labels)
  z <- sample(c(1, 2, 3, 3, 4, 4, 5, 5, 6, 7, 7, 7, 7), size = length(x), replace = TRUE)
  y.raw <- xn * z
  e <- rnorm(length(x), sd = sd(y.raw))
  y <- y.raw + e
  data <- data.frame(x, y, z)

  w <- 3
  m1 <- mgcv::gam(y ~ s(z, by = x, k = 3) + x, data = data)
  m2 <- mgcv::gam(y ~ s(z, by = x, k = w) + x, data = data)
  expect_identical(find_predictors(m1), find_predictors(m2))
  expect_identical(find_predictors(m2)$conditional, c("z", "x"))
})
