# get_data() recovers the data object by name from the environment. If that
# object was overwritten after the fit, model variables can be missing from it.
# Then get_data() should fall back to the model frame (#1210).

set.seed(1210)
base_data <- data.frame(
  y = rbinom(20, 1, 0.5),
  a = rnorm(20),
  b = rnorm(20),
  c = rnorm(20),
  g = sample(1:3, 20, replace = TRUE)
)


test_that("get_data, lm and glm with `y ~ .`, data object overwritten after the fit", {
  d <- base_data[c("y", "a", "b")]
  m_lm <- lm(y ~ ., data = d)
  m_glm <- glm(y ~ ., family = binomial, data = d)
  # overwrite the data object: `a` and `b` are now missing
  d <- base_data[c("y", "c")]

  for (m in list(m_lm, m_glm)) {
    mf <- stats::model.frame(m)
    warnings <- testthat::capture_warnings({
      out <- get_data(m)
    })
    expect_length(warnings, 1)
    expect_match(warnings, "`a`", fixed = TRUE)
    expect_named(out, c("y", "a", "b"))
    expect_identical(nrow(out), nrow(mf))
    expect_equal(out$a, mf$a, ignore_attr = TRUE)

    expect_silent({
      out_quiet <- get_data(m, verbose = FALSE)
    })
    expect_identical(out_quiet, out)
  }
})


test_that("get_predicted and get_loglikelihood, data object overwritten after the fit", {
  d <- base_data[c("y", "a", "b")]
  m_lm <- lm(y ~ ., data = d)
  m_glm <- glm(y ~ ., family = binomial, data = d)
  # overwrite the data object: `a` and `b` are now missing
  d <- base_data[c("y", "c")]

  for (m in list(m_lm, m_glm)) {
    expect_silent(get_predicted(m))
    expect_equal(
      as.numeric(get_loglikelihood(m)),
      as.numeric(stats::logLik(m)),
      tolerance = 1e-8
    )
  }
})


test_that("get_data, predictor `z` taken from the workspace, not from `data`", {
  d <- base_data[c("y", "a")]
  z <- base_data$c
  m <- lm(y ~ a + z, data = d)
  expect_warning(
    {
      out <- get_data(m)
    },
    "`z`",
    fixed = TRUE
  )
  expect_true(all(c("y", "a", "z") %in% colnames(out)))
  expect_equal(out$z, stats::model.frame(m)$z, ignore_attr = TRUE)
})


test_that("get_data, data object unchanged, environment data is returned", {
  d <- base_data[c("y", "a", "g")]
  m <- lm(y ~ a + factor(g), data = d)
  expect_silent({
    out <- get_data(m)
  })
  # `g` is numeric in the data; the model frame has the factor column `factor(g)`
  expect_identical(out$g, d$g)
})


test_that("get_data, nlmer, nonlinear parameters are not data columns", {
  skip_if_not_installed("lme4")
  startvec <- c(Asym = 200, xmid = 725, scal = 350)
  nm1 <- lme4::nlmer(
    formula = circumference ~ SSlogis(age, Asym, xmid, scal) ~ Asym | Tree,
    data = Orange,
    start = startvec
  )
  expect_silent({
    out <- get_data(nm1)
  })
  expect_identical(nrow(out), nrow(Orange))
})


test_that("get_data, data object unchanged, workspace objects as term arguments", {
  d <- base_data[c("y", "a", "b", "g")]
  d$cnt <- rpois(20, 3)
  k <- 2
  br <- c(-Inf, 0, Inf)
  c0 <- 10
  off <- rep(0.5, 20)

  # `k` is an argument of `poly()`
  m <- lm(y ~ poly(a, degree = k), data = d)
  expect_silent(get_data(m))

  # `br` is an argument of `cut()`, predictions must still work
  m <- lm(y ~ cut(a, breaks = br), data = d)
  expect_silent({
    out <- get_data(m)
  })
  expect_named(out, c("y", "a"))
  expect_silent(get_predicted(m))

  # `c0` is used inside `log()`
  m <- lm(y ~ log(b + c0), data = d)
  expect_silent(get_data(m))

  # `off` is an offset from the workspace
  m <- glm(cnt ~ a + offset(off), family = poisson, data = d)
  expect_silent({
    out <- get_data(m)
  })
  expect_false("off" %in% colnames(out))
})
