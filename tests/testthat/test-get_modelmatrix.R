test_that("Issue #612: factor padding", {
  # stats::model.matrix() breaks on contrasts when a column of `data` has
  # only 1 factor level

  # no factor
  mod <- glm(vs ~ cyl, data = mtcars, family = binomial)
  mm <- get_modelmatrix(mod)
  expect_identical(nrow(mm), 32L)
  mm <- get_modelmatrix(mod, data = mtcars)
  expect_identical(nrow(mm), 32L)
  mm <- get_modelmatrix(mod, data = head(mtcars))
  expect_identical(nrow(mm), 6L)

  # one factor
  dat <- mtcars
  dat$cyl <- factor(dat$cyl)
  mod <- glm(vs ~ cyl, data = dat, family = binomial)

  # no data argument
  mm <- get_modelmatrix(mod)
  expect_identical(nrow(mm), 32L)

  # enough factor levels
  mm <- get_modelmatrix(mod, data = head(dat))
  expect_identical(nrow(mm), 6L)

  # not enough factor levels
  mm <- get_modelmatrix(mod, data = dat[3, ])
  expect_identical(nrow(mm), 1L)
})


# iv_robust --------------------------------------------------------------
# =========================================================================

test_that("get_modelmatrix - iv_robust", {
  skip_if_not_installed("ivreg")
  skip_if_not_installed("estimatr")
  data(Kmenta, package = "ivreg")

  x <- estimatr::iv_robust(Q ~ P + D | D + F + A, se_type = "stata", data = Kmenta)

  out1 <- get_modelmatrix(x)
  out2 <- model.matrix(terms(x), data = Kmenta)
  expect_equal(out1, out2, tolerance = 1e-3, ignore_attr = TRUE)

  out1 <- get_modelmatrix(x, data = get_datagrid(x, by = "P"))
  out2 <- model.matrix(terms(x), data = get_datagrid(x, by = "P", include_response = TRUE))
  expect_equal(out1, out2, tolerance = 1e-3, ignore_attr = TRUE)
  expect_identical(nrow(get_datagrid(x, by = "P")), nrow(out2))
})


# ivreg --------------------------------------------------------------
# ====================================================================

test_that("get_modelmatrix - ivreg", {
  skip_if(getRversion() < "4.2.0")
  skip_if_not_installed("ivreg")
  data(Kmenta, package = "ivreg")
  d_kmenta <<- Kmenta

  set.seed(15)
  x <- ivreg::ivreg(Q ~ P + D | D + F + A, data = d_kmenta)

  out1 <- get_modelmatrix(x)
  out2 <- model.matrix(x, data = d_kmenta)
  expect_equal(out1, out2, tolerance = 1e-3, ignore_attr = TRUE)

  out1 <- get_modelmatrix(x, data = get_datagrid(x, by = "P"))
  out2 <- model.matrix(terms(x), data = get_datagrid(x, by = "P", include_response = TRUE))
  expect_equal(out1, out2, tolerance = 1e-3, ignore_attr = TRUE)
  expect_identical(nrow(get_datagrid(x, by = "P")), nrow(out2))
})


# ivreg --------------------------------------------------------------
# ====================================================================

test_that("get_modelmatrix - lm_robust", {
  skip_if_not_installed("estimatr")

  set.seed(15)
  N <- 1:40
  dat <<- data.frame(
    N = N,
    y = rpois(N, lambda = 4),
    x = rnorm(N),
    z = rbinom(N, 1, prob = 0.4)
  )

  x <- estimatr::lm_robust(y ~ x + z, data = dat)

  out1 <- get_modelmatrix(x)
  out2 <- model.matrix(x, data = dat)
  expect_equal(out1, out2, tolerance = 1e-3, ignore_attr = TRUE)

  out1 <- get_modelmatrix(x, data = get_datagrid(x, by = "x"))
  out2 <- model.matrix(x, data = get_datagrid(x, by = "x", include_response = TRUE))
  expect_equal(out1, out2, tolerance = 1e-3, ignore_attr = TRUE)
  expect_identical(nrow(get_datagrid(x, by = "x")), nrow(out2))
})


test_that("Issue #693", {
  set.seed(12345)
  n <- 500
  x <- sample.int(3, n, replace = TRUE)
  w <- sample.int(4, n, replace = TRUE)
  y <- rnorm(n)
  z <- as.numeric(x + y + rlogis(n) > 1.5)
  dat <<- data.frame(x = factor(x), w = factor(w), y = y, z = z)
  m <- glm(z ~ x + w + y, family = binomial, data = dat)
  nd <- head(dat, 2)
  mm <- get_modelmatrix(m, data = head(dat, 1))
  expect_true(all(c("x2", "x3", "w2", "w3", "w4") %in% colnames(mm)))
})
