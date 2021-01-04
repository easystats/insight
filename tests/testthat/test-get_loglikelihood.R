if (require("testthat") && require("insight") && require("nonnest2")) {
  data(iris)
  data(mtcars)

  test_that("get_loglikelihood - lm", {
    x <- lm(Sepal.Length ~ Petal.Width + Species, data=iris)
    ll <- loglikelihood(x, estimator = "ML"); ll2 <- stats::logLik(x)
    testthat::expect_equal(as.numeric(ll), as.numeric(ll2))
    testthat::expect_equal(attributes(ll)$df, attributes(ll2)$df)
    testthat::expect_equal(sum(attributes(ll)$per_obs - nonnest2::llcont(x)), 0)

    # With weights
    x <- lm(Sepal.Length ~ Petal.Width + Species, data=iris, weights=Petal.Length)
    ll <- loglikelihood(x, estimator = "ML"); ll2 <- stats::logLik(x)
    testthat::expect_equal(as.numeric(ll), as.numeric(ll2))
  })

  test_that("get_loglikelihood - glm", {
    x <- glm(vs ~ mpg * disp, data=mtcars, family="binomial")
    ll <- loglikelihood(x, estimator = "ML"); ll2 <- stats::logLik(x)
    testthat::expect_equal(as.numeric(ll), as.numeric(ll2))
    testthat::expect_equal(attributes(ll)$df, attributes(ll2)$df)
    testthat::expect_equal(sum(attributes(ll)$per_obs - nonnest2::llcont(x)), 0)
  })

  test_that("get_loglikelihood - (g)lmer", {
    if(require("lme4")){
      x <- lme4::lmer(Sepal.Length ~ Sepal.Width + (1|Species), data=iris)
      ll <- loglikelihood(x, estimator = "ML"); ll2 <- stats::logLik(x)
      testthat::expect_equal(as.numeric(ll), as.numeric(ll2))
      testthat::expect_equal(attributes(ll)$df, attributes(ll2)$df)

      x <- lme4::glmer(vs ~ mpg + (1|cyl), data=mtcars, family="binomial")
      ll <- loglikelihood(x, estimator = "ML"); ll2 <- stats::logLik(x)
      testthat::expect_equal(as.numeric(ll), as.numeric(ll2))
      testthat::expect_equal(attributes(ll)$df, attributes(ll2)$df)
    }
  })
}
