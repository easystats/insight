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


test_that("finds_*, multinom", {
  set.seed(6)

  ## simulate some data from a three class model
  n <- 1000
  f1 <- function(x) sin(3 * pi * x) * exp(-x)
  f2 <- function(x) x^3
  f3 <- function(x) .5 * exp(-x^2) - .2
  f4 <- function(x) 1
  x1 <- runif(n)
  x2 <- runif(n)
  eta1 <- 2 * (f1(x1) + f2(x2)) - .5
  eta2 <- 2 * (f3(x1) + f4(x2)) - 1
  p <- exp(cbind(0, eta1, eta2))
  p <- p / rowSums(p) ## prob. of each category
  cp <- t(apply(p, 1, cumsum)) ## cumulative prob.
  y <- apply(cp, 1, function(x) min(which(x > runif(1)))) - 1
  b <- mgcv::gam(list(y ~ s(x1) + s(x2), ~ s(x1) + s(x2)), family = mgcv::multinom(K = 2))

  out <- find_response(b)
  expect_identical(out, "y")
  out <- find_predictors(b)
  expect_identical(
    out,
    list(
      y = list(conditional = c("x1", "x2")),
      y = list(conditional = c("x1", "x2"))
    )
  )
  out <- find_formula(b)
  expect_equal(
    out,
    list(
      y = list(conditional = y ~ s(x1) + s(x2)),
      y = list(conditional = y ~ s(x1) + s(x2))
    ),
    ignore_attr = TRUE
  )
})


test_that("get_statistic", {
  skip_if_not_installed("lme4")
  data("sleepstudy", package = "lme4")
  m_gam <- mgcv::gam(
    Reaction ~ poly(Days, 2) + s(Subject, bs = "re"),
    family = gaussian(),
    data = sleepstudy,
    method = "ML"
  )

  out <- get_statistic(m_gam)
  s <- summary(m_gam)
  expect_equal(
    out$Statistic,
    as.vector(c(s$p.table[, "t value"], s$s.table[, "F"])),
    tolerance = 1e-4,
    ignore_attr = TRUE
  )

  out <- get_statistic(m_gam, re_test = FALSE)
  s <- summary(m_gam, re.test = FALSE)
  expect_equal(
    out$Statistic,
    as.vector(s$p.table[, "t value"]),
    tolerance = 1e-4,
    ignore_attr = TRUE
  )

  s <- summary(m_gam)
  out <- get_statistic(s)
  expect_equal(
    out$Statistic,
    as.vector(c(s$p.table[, "t value"], s$s.table[, "F"])),
    tolerance = 1e-4,
    ignore_attr = TRUE
  )
})
