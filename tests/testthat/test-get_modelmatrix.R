pkgs <- c("insight", "estimatr", "ivreg")
invisible(sapply(pkgs, requiet))



test_that("Issue #612: factor padding", {
  # stats::model.matrix() breaks on contrasts when a column of `data` has
  # only 1 factor level

  # no factor
  mod <- glm(vs ~ cyl, data = mtcars, family = binomial)
  mm <- get_modelmatrix(mod)
  expect_equal(nrow(mm), 32)
  mm <- get_modelmatrix(mod, data = mtcars)
  expect_equal(nrow(mm), 32)
  mm <- get_modelmatrix(mod, data = head(mtcars))
  expect_equal(nrow(mm), 6)

  # one factor
  dat <- mtcars
  dat$cyl <- factor(dat$cyl)
  mod <- glm(vs ~ cyl, data = dat, family = binomial)

  # no data argument
  mm <- get_modelmatrix(mod)
  expect_equal(nrow(mm), 32)

  # enough factor levels
  mm <- get_modelmatrix(mod, data = head(dat))
  expect_equal(nrow(mm), 6)

  # not enough factor levels
  mm <- get_modelmatrix(mod, data = dat[3, ])
  expect_equal(nrow(mm), 1)
})



# iv_robust --------------------------------------------------------------
# =========================================================================

test_that("get_modelmatrix - iv_robust", {
  data(Kmenta, package = "ivreg")
  x <- iv_robust(Q ~ P + D | D + F + A, se_type = "stata", data = Kmenta)

  out1 <- get_modelmatrix(x)
  out2 <- model.matrix(terms(x), data = Kmenta)
  expect_equal(out1, out2, tolerance = 1e-3, ignore_attr = TRUE)

  out1 <- get_modelmatrix(x, data = get_datagrid(x, at = "P"))
  out2 <- model.matrix(terms(x), data = get_datagrid(x, at = "P", include_response = TRUE))
  expect_equal(out1, out2, tolerance = 1e-3, ignore_attr = TRUE)
  expect_equal(nrow(get_datagrid(x, at = "P")), nrow(out2))
})



# ivreg --------------------------------------------------------------
# ====================================================================

test_that("get_modelmatrix - ivreg", {
  data(Kmenta, package = "ivreg")
  x <- ivreg(Q ~ P + D | D + F + A, data = Kmenta)

  out1 <- get_modelmatrix(x)
  out2 <- model.matrix(x, data = Kmenta)
  expect_equal(out1, out2, tolerance = 1e-3, ignore_attr = TRUE)

  out1 <- get_modelmatrix(x, data = get_datagrid(x, at = "P"))
  out2 <- model.matrix(terms(x), data = get_datagrid(x, at = "P", include_response = TRUE))
  expect_equal(out1, out2, tolerance = 1e-3, ignore_attr = TRUE)
  expect_equal(nrow(get_datagrid(x, at = "P")), nrow(out2))
})



# ivreg --------------------------------------------------------------
# ====================================================================

test_that("get_modelmatrix - lm_robust", {
  set.seed(15)
  N <- 1:40
  dat <<- data.frame(
    N = N,
    y = rpois(N, lambda = 4),
    x = rnorm(N),
    z = rbinom(N, 1, prob = 0.4)
  )

  x <- lm_robust(y ~ x + z, data = dat)

  out1 <- get_modelmatrix(x)
  out2 <- model.matrix(x, data = dat)
  expect_equal(out1, out2, tolerance = 1e-3, ignore_attr = TRUE)

  out1 <- get_modelmatrix(x, data = get_datagrid(x, at = "x"))
  out2 <- model.matrix(x, data = get_datagrid(x, at = "x", include_response = TRUE))
  expect_equal(out1, out2, tolerance = 1e-3, ignore_attr = TRUE)
  expect_equal(nrow(get_datagrid(x, at = "x")), nrow(out2))
})



test_that("Issue #693", {
  set.seed(12345)
  n <- 500
  x <- sample(1:3, n, replace = TRUE)
  w <- sample(1:4, n, replace = TRUE)
  y <- rnorm(n)
  z <- ifelse(x + y + rlogis(n) > 1.5, 1, 0)
  dat <<- data.frame(x = factor(x), w = factor(w), y = y, z = z)
  m <- glm(z ~ x + w + y, family = binomial, data = dat)
  nd <- head(dat, 2)
  mm <- get_modelmatrix(m, data = head(dat, 1))
  expect_true(all(c("x2", "x3", "w2", "w3", "w4") %in% colnames(mm)))
})
