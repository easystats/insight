if (require("testthat") && require("insight") && require("lme4")) {
  data(mtcars)
  data(sleepstudy)
  data(cbpp)
  set.seed(123)
  mtcars$w <- abs(rnorm(nrow(mtcars), mean = 1, .3))
  sleepstudy$w <- abs(rnorm(nrow(sleepstudy), mean = 1, .3))
  cbpp$w <- abs(rnorm(nrow(cbpp), mean = 1, .3))

  test_that("get_residuals - lm", {
    m <- lm(am ~ cyl, weights = w, data = mtcars)
    expect_equal(
      as.vector(get_residuals(m, weighted = FALSE)),
      as.vector(residuals(m))
    )
    expect_equal(
      as.vector(get_residuals(m, weighted = FALSE, type = "deviance")),
      as.vector(residuals(m, type = "deviance"))
    )
    expect_equal(
      get_weights(m),
      weights(m)
    )
    expect_equal(
      as.vector(get_residuals(m)),
      as.vector(residuals(m))
    )
    expect_equal(
      as.vector(get_deviance(m)),
      as.vector(deviance(m))
    )
    expect_equal(
      get_residuals(m, weighted = TRUE),
      as.vector(weighted.residuals(m))
    )
  })

  test_that("get_residuals - glm", {
    m <- suppressWarnings(glm(am ~ cyl, weights = w, data = mtcars, family = binomial))
    expect_equal(
      as.vector(get_residuals(m, weighted = FALSE, type = "response")),
      as.vector(residuals(m, type = "response"))
    )
    expect_equal(
      as.vector(get_residuals(m, weighted = FALSE)),
      as.vector(residuals(m))
    )
    expect_equal(
      get_weights(m),
      weights(m)
    )
    expect_equal(
      as.vector(get_residuals(m)),
      as.vector(residuals(m))
    )
    expect_equal(
      as.vector(get_deviance(m)),
      as.vector(deviance(m))
    )
    expect_equal(
      get_residuals(m, weighted = TRUE),
      as.vector(weighted.residuals(m))
    )
  })

  test_that("get_residuals - lmer", {
    m <- lmer(Reaction ~ Days + (Days | Subject), weights = w, data = sleepstudy)
    expect_equal(
      as.vector(get_residuals(m, weighted = FALSE)),
      as.vector(residuals(m))
    )
    expect_equal(
      as.vector(get_residuals(m, weighted = FALSE, type = "deviance")),
      as.vector(residuals(m, type = "deviance"))
    )
    expect_equal(
      get_weights(m),
      weights(m)
    )
    expect_equal(
      as.vector(get_residuals(m)),
      as.vector(residuals(m))
    )
    expect_equal(
      as.vector(get_deviance(m)),
      as.vector(deviance(m, REML = FALSE))
    )
    expect_equal(
      get_residuals(m, weighted = TRUE),
      as.vector(weighted.residuals(m))
    )
  })

  test_that("get_residuals - glmer", {
    m <- glmer(cbind(incidence, size - incidence) ~ period + (1 | herd), weights = w, data = cbpp, family = binomial, nAGQ = 0)
    expect_equal(
      as.vector(get_residuals(m, weighted = FALSE, type = "response")),
      as.vector(residuals(m, type = "response"))
    )
    expect_equal(
      as.vector(get_residuals(m, weighted = FALSE)),
      as.vector(residuals(m))
    )
    expect_equal(
      get_weights(m),
      weights(m)
    )
    expect_equal(
      as.vector(get_residuals(m)),
      as.vector(residuals(m))
    )
    expect_equal(
      as.vector(get_deviance(m)),
      as.vector(deviance(m))
    )
    expect_equal(
      get_residuals(m, weighted = TRUE),
      as.vector(weighted.residuals(m))
    )
  })
}
