test_that("get_loglikelihood_adjustment handles identity and expm1 transforms", {
  m_id <- lm(wt ~ mpg, data = mtcars)
  expect_equal(get_loglikelihood_adjustment(m_id), 0)

  # negative response values should also return 0 for identity
  d_neg <- data.frame(y = -mtcars$wt, x = mtcars$mpg)
  m_neg <- lm(y ~ x, data = d_neg)
  expect_equal(get_loglikelihood_adjustment(m_neg), 0)

  # d/dy expm1(y) = exp(y) = d/dy exp(y), so adjusted logLik must match exp(y)
  m_exp <- lm(exp(wt) ~ mpg, data = mtcars)
  m_expm1 <- lm(expm1(wt) ~ mpg, data = mtcars)
  expect_equal(
    get_loglikelihood_adjustment(m_expm1),
    get_loglikelihood_adjustment(m_exp),
    tolerance = 1e-6
  )
  expect_equal(
    as.numeric(get_loglikelihood(m_expm1, check_response = TRUE)),
    as.numeric(get_loglikelihood(m_exp, check_response = TRUE)),
    tolerance = 1e-6
  )

  # weighted expm1
  set.seed(123)
  d_w <- mtcars
  d_w$w <- abs(rnorm(nrow(d_w), mean = 1))
  m_exp_w <- lm(exp(wt) ~ mpg, weights = w, data = d_w)
  m_expm1_w <- lm(expm1(wt) ~ mpg, weights = w, data = d_w)
  expect_equal(
    get_loglikelihood_adjustment(m_expm1_w),
    get_loglikelihood_adjustment(m_exp_w),
    tolerance = 1e-6
  )
})

test_that("get_loglikelihood_adjustment uses Jacobian fallback without weights", {
  # log(y + 1) must match log1p(y)
  m_log1p <- lm(log1p(wt) ~ mpg, data = mtcars)
  m_log_plus1 <- lm(log(wt + 1) ~ mpg, data = mtcars)
  expect_equal(
    get_loglikelihood_adjustment(m_log_plus1),
    get_loglikelihood_adjustment(m_log1p),
    tolerance = 1e-6
  )
  expect_equal(
    as.numeric(get_loglikelihood(m_log_plus1, check_response = TRUE)),
    as.numeric(get_loglikelihood(m_log1p, check_response = TRUE)),
    tolerance = 1e-6
  )

  # log(y, base = 2) must match log2(y)
  m_log2 <- lm(log2(wt) ~ mpg, data = mtcars)
  m_log_base2 <- lm(log(wt, base = 2) ~ mpg, data = mtcars)
  expect_equal(
    get_loglikelihood_adjustment(m_log_base2),
    get_loglikelihood_adjustment(m_log2),
    tolerance = 1e-6
  )

  # log(log(y)) matches analytic Jacobian sum(log(1 / (y * log(y))))
  m_loglog <- lm(log(log(mpg)) ~ wt, data = mtcars)
  expected_loglog <- sum(log(1 / (mtcars$mpg * log(mtcars$mpg))))
  expect_equal(
    get_loglikelihood_adjustment(m_loglog),
    expected_loglog,
    tolerance = 1e-5
  )

  # unsupported transformations return NULL and warn instead of adding dlnorm()
  m_log_plus2 <- lm(log(wt + 2) ~ mpg, data = mtcars)
  expect_null(get_loglikelihood_adjustment(m_log_plus2))
  expect_warning(
    get_loglikelihood(m_log_plus2, check_response = TRUE),
    regex = "Could not compute"
  )
})
