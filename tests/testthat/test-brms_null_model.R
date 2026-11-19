skip_on_cran()
skip_if_not_installed("brms")
skip_if_not_installed("BH")
skip_if_not_installed("RcppEigen")
skip_if_not_installed("lme4")

set.seed(123)
d_null <- data.frame(g = factor(rep(1:20, each = 10)), x = rnorm(200))
d_null$y <- stats::rpois(
  200,
  exp(0.5 + 0.3 * d_null$x + rep(rnorm(20, sd = 0.6), each = 10))
)

m_pois <- suppressWarnings(suppressMessages(brms::brm(
  y ~ x + (1 | g),
  data = d_null,
  family = poisson(),
  chains = 1,
  iter = 500,
  seed = 123,
  refresh = 0
)))

test_that("get_variance computes the null model of a brms model", {
  m_glmer <- lme4::glmer(y ~ x + (1 | g), data = d_null, family = poisson())
  expect_equal(
    get_variance_distribution(m_pois),
    get_variance_distribution(m_glmer),
    tolerance = 0.2,
    ignore_attr = TRUE
  )
})
