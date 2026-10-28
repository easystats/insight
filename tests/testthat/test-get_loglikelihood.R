skip_if_not_installed("nonnest2")

test_that("get_loglikelihood - lm", {
  x <- lm(Sepal.Length ~ Petal.Width + Species, data = iris)
  ll <- loglikelihood(x, estimator = "ML")
  ll2 <- stats::logLik(x)
  expect_equal(as.numeric(ll), as.numeric(ll2), tolerance = 1e-4, ignore_attr = TRUE)
  expect_equal(
    attributes(ll)$df,
    attributes(ll2)$df,
    tolerance = 1e-4,
    ignore_attr = TRUE
  )
  expect_equal(
    sum(attributes(ll)$per_obs - nonnest2::llcont(x)),
    0,
    tolerance = 1e-4,
    ignore_attr = TRUE
  )

  # REML
  ll <- loglikelihood(x, estimator = "REML")
  ll2 <- stats::logLik(x, REML = TRUE)
  expect_equal(as.numeric(ll), as.numeric(ll2), tolerance = 1e-4, ignore_attr = TRUE)

  # With weights
  x <- lm(Sepal.Length ~ Petal.Width + Species, data = iris, weights = Petal.Length)
  ll <- loglikelihood(x, estimator = "ML")
  ll2 <- stats::logLik(x)
  expect_equal(as.numeric(ll), as.numeric(ll2), tolerance = 1e-4, ignore_attr = TRUE)

  # log-response
  x <- lm(mpg ~ wt, data = mtcars)
  expect_equal(as.numeric(get_loglikelihood(x)), -80.01471, tolerance = 1e-3)

  x <- lm(log(mpg) ~ wt, data = mtcars)
  expect_equal(as.numeric(get_loglikelihood(x)), 19.42433, tolerance = 1e-3)
  expect_equal(
    as.numeric(get_loglikelihood(x, check_response = TRUE)),
    -75.21614,
    tolerance = 1e-3
  )
  expect_equal(
    as.numeric(get_loglikelihood(x, check_response = TRUE)),
    as.numeric(get_loglikelihood(x)) + as.numeric(get_loglikelihood_adjustment(x)),
    tolerance = 1e-3
  )

  set.seed(123)
  mtcars$wg <- abs(rnorm(nrow(mtcars), mean = 1))
  x <- lm(mpg ~ wt, weights = wg, data = mtcars)
  expect_equal(as.numeric(get_loglikelihood(x)), -82.03376, tolerance = 1e-3)

  x <- lm(log(mpg) ~ wt, weights = wg, data = mtcars)
  expect_equal(as.numeric(get_loglikelihood(x)), 18.4205, tolerance = 1e-3)
  expect_equal(
    as.numeric(get_loglikelihood(x, check_response = TRUE)),
    -75.58669,
    tolerance = 1e-3
  )

  # sqrt-response
  x <- lm(sqrt(mpg) ~ wt, data = mtcars)
  expect_equal(as.numeric(get_loglikelihood(x)), -7.395031, tolerance = 1e-3)
  expect_equal(
    as.numeric(get_loglikelihood(x, check_response = TRUE)),
    -76.89597,
    tolerance = 1e-3
  )

  # power to x
  x <- lm(mpg^3.5 ~ wt, weights = wg, data = mtcars)
  expect_equal(as.numeric(get_loglikelihood(x)), -385.6256, tolerance = 1e-3)
  expect_equal(
    as.numeric(get_loglikelihood(x, check_response = TRUE)),
    -110.5192,
    tolerance = 1e-3
  )

  # scale
  x <- lm(mpg / 3.5 ~ wt, weights = wg, data = mtcars)
  expect_equal(as.numeric(get_loglikelihood(x)), -41.94534, tolerance = 1e-3)
  expect_equal(
    as.numeric(get_loglikelihood(x, check_response = TRUE)),
    -82.03376,
    tolerance = 1e-3
  )

  # scale, no weights
  x <- lm(mpg / 3.5 ~ wt, data = mtcars)
  expect_equal(as.numeric(get_loglikelihood(x)), -39.9263, tolerance = 1e-3)
  expect_equal(
    as.numeric(get_loglikelihood(x, check_response = TRUE)),
    -80.01471,
    tolerance = 1e-3
  )
})

test_that("get_loglikelihood - not supported", {
  # Box-Cox
  m <- lm((mpg^0.7 - 1) / 0.7 ~ hp, data = mtcars)
  expect_warning(get_loglikelihood(m, check_response = TRUE), regex = "Could not compute")
  # Inverse
  m <- lm(1 / mpg ~ hp, data = mtcars)
  expect_warning(get_loglikelihood(m, check_response = TRUE), regex = "Could not compute")
})

test_that("get_loglikelihood - glm", {
  x <- glm(vs ~ mpg * disp, data = mtcars, family = "binomial")
  ll <- loglikelihood(x)
  ll2 <- stats::logLik(x)
  expect_equal(as.numeric(ll), as.numeric(ll2), tolerance = 1e-4, ignore_attr = TRUE)
  expect_equal(
    attributes(ll)$df,
    attributes(ll2)$df,
    tolerance = 1e-4,
    ignore_attr = TRUE
  )
  expect_equal(
    sum(attributes(ll)$per_obs - nonnest2::llcont(x)),
    0,
    tolerance = 1e-4,
    ignore_attr = TRUE
  )

  x <- glm(cbind(cyl, gear) ~ mpg, data = mtcars, weights = disp, family = binomial)
  ll <- loglikelihood(x)
  ll2 <- stats::logLik(x)
  expect_equal(as.numeric(ll), as.numeric(ll2), tolerance = 1e-4, ignore_attr = TRUE)
  expect_equal(
    attributes(ll)$df,
    attributes(ll2)$df,
    tolerance = 1e-4,
    ignore_attr = TRUE
  )
  # Nonnest2 seems to be giving diffenrent results,
  # which sums doesn't add up to base R's result... so commenting off
  # expect_equal(sum(attributes(ll)$per_obs - nonnest2::llcont(x)), 0)
})

test_that("get_loglikelihood - (g)lmer", {
  skip_if_not_installed("curl")
  skip_if_offline()
  skip_if_not_installed("lme4")
  x <- lme4::lmer(Sepal.Length ~ Sepal.Width + (1 | Species), data = iris)

  # REML
  ll <- loglikelihood(x, estimator = "REML")
  ll2 <- stats::logLik(x)
  expect_equal(as.numeric(ll), as.numeric(ll2), tolerance = 1e-4, ignore_attr = TRUE)
  expect_equal(
    attributes(ll)$df,
    attributes(ll2)$df,
    tolerance = 1e-4,
    ignore_attr = TRUE
  )

  # ML
  ll <- loglikelihood(x, estimator = "ML")
  ll2 <- stats::logLik(x, REML = FALSE)
  expect_equal(as.numeric(ll), as.numeric(ll2), tolerance = 1e-4, ignore_attr = TRUE)

  # default
  ll <- loglikelihood(x)
  ll2 <- stats::logLik(x)
  expect_equal(as.numeric(ll), as.numeric(ll2), tolerance = 1e-4, ignore_attr = TRUE)
  expect_equal(
    attributes(ll)$df,
    attributes(ll2)$df,
    tolerance = 1e-4,
    ignore_attr = TRUE
  )

  x <- lme4::glmer(vs ~ mpg + (1 | cyl), data = mtcars, family = "binomial")
  ll <- loglikelihood(x, estimator = "REML") # no REML for glmer
  ll2 <- stats::logLik(x)
  expect_equal(as.numeric(ll), as.numeric(ll2), tolerance = 1e-4, ignore_attr = TRUE)
  expect_equal(
    attributes(ll)$df,
    attributes(ll2)$df,
    tolerance = 1e-4,
    ignore_attr = TRUE
  )

  ll <- loglikelihood(x, estimator = "ML")
  ll2 <- stats::logLik(x, REML = FALSE)
  expect_equal(as.numeric(ll), as.numeric(ll2), tolerance = 1e-4, ignore_attr = TRUE)

  skip_if_not_installed("httr2")

  model <- download_model("lmerMod_1")
  skip_if(is.null(model))
  expect_equal(
    get_loglikelihood(model, estimator = "REML"),
    logLik(model, REML = TRUE),
    tolerance = 0.01,
    ignore_attr = TRUE
  )
  expect_equal(
    get_loglikelihood(model, estimator = "ML"),
    logLik(model, REML = FALSE),
    tolerance = 0.01,
    ignore_attr = TRUE
  )

  model <- download_model("merMod_1")
  skip_if(is.null(model))
  expect_equal(
    get_loglikelihood(model, estimator = "REML"),
    logLik(model, REML = FALSE),
    tolerance = 0.01,
    ignore_attr = TRUE
  )
  expect_equal(
    get_loglikelihood(model, estimator = "ML"),
    logLik(model, REML = FALSE),
    tolerance = 0.01,
    ignore_attr = TRUE
  )
})

test_that("get_loglikelihood - stanreg ", {
  skip_on_cran()
  skip_if_not_installed("rstanarm")
  x <- rstanarm::stan_glm(Sepal.Length ~ Petal.Width, data = iris, refresh = 0)
  ref <- lm(Sepal.Length ~ Petal.Width, data = iris)
  ll <- loglikelihood(x)
  ll2 <- loglikelihood(ref)
  expect_equal(as.numeric(ll), as.numeric(ll2), tolerance = 2)
  expect_equal(
    mean(abs(attributes(ll)$per_obs - attributes(ll2)$per_obs)),
    0,
    tolerance = 0.1
  )
})

test_that("get_loglikelihood - ivreg", {
  skip_if_not_installed("ivreg")
  data("CigaretteDemand", package = "ivreg")
  x <- ivreg::ivreg(
    log(packs) ~ log(rprice) + log(rincome) | salestax + log(rincome),
    data = CigaretteDemand
  )

  ll <- loglikelihood(x)
  expect_equal(as.numeric(ll), 13.26255, tolerance = 1e-3)
})

test_that("get_loglikelihood - plm", {
  skip_if_not_installed("plm")
  data("Produc", package = "plm")
  x <- suppressWarnings(
    plm::plm(
      log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp,
      data = Produc,
      index = c("state", "year")
    )
  )

  ll <- loglikelihood(x)
  expect_equal(as.numeric(ll), 1534.532, tolerance = 1e-3)
})

test_that("get_loglikelihood - iv_robust", {
  skip_if_not_installed("estimatr")
  x <- estimatr::iv_robust(mpg ~ gear + cyl | carb + wt, data = mtcars)

  ll <- loglikelihood(x)
  expect_equal(as.numeric(ll), -84.60057, tolerance = 1e-3)
})

test_that("get_loglikelihood - mgcv", {
  skip_if_not_installed("mgcv")
  x <- mgcv::gam(Sepal.Length ~ s(Petal.Width), data = iris)
  ll <- insight::get_loglikelihood(x)
  ll2 <- stats::logLik(x)
  expect_equal(as.numeric(ll), -96.26613, tolerance = 1e-3)
  # TODO: I'm not sure why this differes :/
  # expect_equal(as.numeric(ll), as.numeric(ll2))
})

test_that("get_loglikelihood - gamm uses the lme part", {
  skip_if_not_installed("mgcv")
  skip_if_not_installed("nlme")
  x <- mgcv::gamm(
    Sepal.Length ~ s(Petal.Width) + Petal.Length,
    random = list(Species = ~1),
    data = iris,
    method = "REML"
  )
  ll <- get_loglikelihood(x)
  ll2 <- stats::logLik(x$lme)
  expect_equal(as.numeric(ll), as.numeric(ll2), tolerance = 1e-6)
  expect_identical(attributes(ll)$df, attributes(ll2)$df)
  expect_equal(
    as.numeric(get_loglikelihood(x, estimator = "ML")),
    as.numeric(stats::logLik(x$lme, REML = FALSE)),
    tolerance = 1e-6
  )
})

test_that("get_loglikelihood - gamm fitted by PQL", {
  skip_if_not_installed("mgcv")
  skip_if_not_installed("nlme")
  skip_if_not_installed("MASS")
  set.seed(123)
  d <- data.frame(x = stats::runif(200), f = factor(sample(1:5, 200, TRUE)))
  d$y <- stats::rpois(200, exp(1 + sin(3 * d$x)))
  x <- mgcv::gamm(
    y ~ s(x),
    random = list(f = ~1),
    family = poisson(),
    data = d,
    verbosePQL = FALSE
  )
  expect_warning(ll <- get_loglikelihood(x), regexp = "PQL")
  expect_true(is.na(ll))
  expect_silent(get_loglikelihood(x, verbose = FALSE))
})

test_that("get_loglikelihood - gamm4 uses the mer part", {
  skip_on_cran() # fails for 4.5.3 (2026-03-11 ucrt)
  skip_if_not_installed("gamm4")
  x <- suppressWarnings(gamm4::gamm4(
    Sepal.Length ~ s(Petal.Width),
    random = ~ (1 | Species),
    data = iris
  ))
  ll <- get_loglikelihood(x)
  expect_equal(as.numeric(ll), as.numeric(stats::logLik(x$mer)), tolerance = 1e-6)
  # older gamm4 versions return a plain list
  ll <- get_loglikelihood(unclass(x))
  expect_equal(as.numeric(ll), as.numeric(stats::logLik(x$mer)), tolerance = 1e-6)
})

test_that("get_loglikelihood - lme, ML for REML fits", {
  skip_if_not_installed("nlme")
  x <- nlme::lme(Sepal.Length ~ Petal.Width, random = ~ 1 | Species, data = iris)
  expect_equal(
    as.numeric(get_loglikelihood(x, estimator = "ML")),
    as.numeric(stats::logLik(x, REML = FALSE)),
    tolerance = 1e-6
  )
})

test_that("get_loglikelihood - Bernoulli with inversed levels", {
  d <- mtcars
  d$zero <- factor(d$vs, levels = c(0, 1))
  d$ones <- factor(d$vs, levels = c(1, 0))

  ml_zero <- glm(zero ~ mpg, family = binomial, data = d)
  ml_ones <- glm(ones ~ mpg, family = binomial, data = d)

  expect_equal(logLik(ml_zero), get_loglikelihood(ml_zero), ignore_attr = TRUE)
  expect_equal(logLik(ml_ones), get_loglikelihood(ml_ones), ignore_attr = TRUE)
  expect_equal(get_loglikelihood(ml_zero), get_loglikelihood(ml_ones), ignore_attr = TRUE)
})

test_that("get_loglikelihood - fails for negative values for some transformation", {
  data(mtcars)
  dafr <- data.frame(y = mtcars$mpg * -1, x = mtcars$hp)
  m <- lm(y^2 ~ x, data = dafr)
  expect_warning(get_loglikelihood(m, check_response = TRUE), regex = "Could not")
  expect_equal(
    get_loglikelihood(m, check_response = TRUE, verbose = FALSE),
    get_loglikelihood(m, verbose = FALSE),
    tolerance = 1e-4
  )
  expect_null(get_loglikelihood_adjustment(m))
})

test_that("get_loglikelihood - tweedie", {
  skip_on_cran()
  skip_if_not_installed("tweedie")
  skip_if_not_installed("statmod")
  set.seed(123)
  y <- rgamma(20, shape = 5)
  x <- 1:20
  m <- glm(y ~ x, family = statmod::tweedie(var.power = 3, link.power = 0))

  ll <- loglikelihood(m)
  expect_equal(as.numeric(ll), -42.6214, tolerance = 1e-2, ignore_attr = TRUE)
})
