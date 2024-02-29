skip_on_cran()
skip_if_not_installed("mgcv")

test_that("finds_*", {
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

  # find_predictors()
  expect_identical(find_predictors(m1), find_predictors(m2))
  expect_identical(find_predictors(m2)$conditional, c("z", "x"))

  # find_variables()
  expect_identical(find_variables(m1), find_variables(m2))
  expect_identical(find_variables(m2)$conditional, c("z", "x"))

  # find_terms()
  expect_identical(
    find_terms(m1),
    list(response = "y", conditional = c("s(z, by = x, k = 3)", "x"))
  )
  expect_identical(
    find_terms(m2),
    list(response = "y", conditional = c("s(z, by = x, k = w)", "x"))
  )

  # get_predictors()
  out <- head(get_predictors(m1))
  expect_named(out, c("z", "x"))
})
