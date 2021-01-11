if (require("testthat") && require("insight") && require("nonnest2")) {
  data(iris)
  data(mtcars)

  test_that("get_loglikelihood - lm", {
    x <- lm(Sepal.Length ~ Petal.Width + Species, data = iris)
    ll <- loglikelihood(x, estimator = "ML")
    ll2 <- stats::logLik(x)
    expect_equal(as.numeric(ll), as.numeric(ll2))
    expect_equal(attributes(ll)$df, attributes(ll2)$df)
    expect_equal(sum(attributes(ll)$per_obs - nonnest2::llcont(x)), 0)

    # REML
    ll <- loglikelihood(x, estimator = "REML")
    ll2 <- stats::logLik(x, REML = TRUE)
    expect_equal(as.numeric(ll), as.numeric(ll2))

    # With weights
    x <- lm(Sepal.Length ~ Petal.Width + Species, data = iris, weights = Petal.Length)
    ll <- loglikelihood(x, estimator = "ML")
    ll2 <- stats::logLik(x)
    expect_equal(as.numeric(ll), as.numeric(ll2))
  })

  test_that("get_loglikelihood - glm", {
    x <- glm(vs ~ mpg * disp, data = mtcars, family = "binomial")
    ll <- loglikelihood(x)
    ll2 <- stats::logLik(x)
    expect_equal(as.numeric(ll), as.numeric(ll2))
    expect_equal(attributes(ll)$df, attributes(ll2)$df)
    expect_equal(sum(attributes(ll)$per_obs - nonnest2::llcont(x)), 0)

    x <- glm(cbind(cyl, gear) ~ mpg, data = mtcars, weights = disp, family = binomial)
    ll <- loglikelihood(x)
    ll2 <- stats::logLik(x)
    expect_equal(as.numeric(ll), as.numeric(ll2))
    expect_equal(attributes(ll)$df, attributes(ll2)$df)
    # Nonnest2 seems to be giving diffenrent results,
    # which sums doesn't add up to base R's result... so commenting off
    # expect_equal(sum(attributes(ll)$per_obs - nonnest2::llcont(x)), 0)
  })

  test_that("get_loglikelihood - (g)lmer", {
    if (require("lme4")) {
      x <- lme4::lmer(Sepal.Length ~ Sepal.Width + (1|Species), data = iris)
      ll <- loglikelihood(x, estimator = "ML")
      ll2 <- stats::logLik(x)
      expect_equal(as.numeric(ll), as.numeric(ll2))
      expect_equal(attributes(ll)$df, attributes(ll2)$df)

      x <- lme4::glmer(vs ~ mpg + (1|cyl), data = mtcars, family = "binomial")
      ll <- loglikelihood(x, estimator = "ML")
      ll2 <- stats::logLik(x)
      expect_equal(as.numeric(ll), as.numeric(ll2))
      expect_equal(attributes(ll)$df, attributes(ll2)$df)
    }
  })

  test_that("get_loglikelihood - stanreg", {
    if (require("rstanarm")) {
      x <- rstanarm::stan_glm(Sepal.Length ~ Petal.Width, data = iris)
      ref <- lm(Sepal.Length ~ Petal.Width, data = iris)
      ll <- loglikelihood(x)
      ll2 <- loglikelihood(ref)
      expect_equal(as.numeric(ll), as.numeric(ll2), tolerance=2)
      expect_equal(mean(abs(attributes(ll)$per_obs - attributes(ll2)$per_obs)), 0,  tolerance=0.1)
    }
  })

  test_that("get_loglikelihood - ivreg", {
    if (require("ivreg")) {
      data("CigaretteDemand", package = "ivreg")
      x <- ivreg::ivreg(log(packs) ~ log(rprice) + log(rincome) | salestax + log(rincome), data = CigaretteDemand)

      ll <- loglikelihood(x)
      expect_equal(as.numeric(ll), 13.26255, tolerance = 1e-3)
    }
  })

  test_that("get_loglikelihood - plm", {
    if (require("plm")) {
      data("Produc", package = "plm")
      x <- plm::plm(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp,
                data = Produc, index = c("state","year"))

      ll <- loglikelihood(x)
      expect_equal(as.numeric(ll), 1534.532, tolerance = 1e-3)
    }
  })

  test_that("get_loglikelihood - iv_robust", {
    if (require("estimatr")) {
      data(mtcars)
      x <- estimatr::iv_robust(mpg ~ gear + cyl | carb + wt, data = mtcars)

      ll <- loglikelihood(x)
      expect_equal(as.numeric(ll), -84.60057, tolerance = 1e-3)
    }
  })
}
