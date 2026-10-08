skip_on_cran()
skip_if_not_installed("brms")
skip_if_not_installed("BH")
skip_if_not_installed("RcppEigen")

# The matrix argument of `sar()`, `car()` and `fcor()` is an object in `data2`,
# not a variable in the data, so it is no predictor.
set.seed(123)
d_autocor <- data.frame(
  y = rnorm(20),
  x = rnorm(20),
  g = factor(rep(1:4, each = 5))
)
W <- matrix(0, 20, 20)
for (i in 1:19) {
  W[i, i + 1] <- W[i + 1, i] <- 1
}
Wg <- matrix(0, 4, 4, dimnames = list(1:4, 1:4))
for (i in 1:3) {
  Wg[i, i + 1] <- Wg[i + 1, i] <- 1
}

m_sar <- suppressWarnings(suppressMessages(brms::brm(
  y ~ x + sar(W),
  data = d_autocor,
  data2 = list(W = W),
  chains = 1,
  iter = 200,
  seed = 123,
  refresh = 0
)))

m_car <- suppressWarnings(suppressMessages(brms::brm(
  y ~ x + car(Wg, gr = g),
  data = d_autocor,
  data2 = list(Wg = Wg),
  chains = 1,
  iter = 200,
  seed = 123,
  refresh = 0
)))

test_that("find_predictors and find_variables drop the sar() matrix", {
  expect_identical(find_predictors(m_sar), list(conditional = "x"))
  expect_identical(find_variables(m_sar), list(response = "y", conditional = "x"))
  expect_named(get_data(m_sar), c("y", "x"))
})

test_that("find_predictors keeps the car() grouping variable", {
  expect_identical(find_predictors(m_car), list(conditional = c("x", "g")))
  expect_identical(
    find_variables(m_car),
    list(response = "y", conditional = c("x", "g"))
  )
  expect_named(get_data(m_car), c("y", "x", "g"))
})

test_that("the autocorrelation matrix is dropped from formulas", {
  # named `M`, also after other arguments, and `fcor()`
  f <- list(
    conditional = y ~ x + sar(M = W, type = "lag"),
    sigma = ~ z + car(gr = g, M = Wg),
    mu2 = ~ v + brms::fcor(V)
  )
  out <- insight:::.prepare_predictors_brms(
    NULL,
    f,
    c("conditional", "sigma", "mu2")
  )
  expect_identical(all.vars(out$conditional), "x")
  expect_identical(all.vars(out$sigma), c("z", "g"))
  expect_identical(all.vars(out$mu2), "v")
  expect_identical(
    all.vars(insight:::.remove_brms_autocor_matrix(y ~ x + fcor(V))),
    c("y", "x")
  )
})
