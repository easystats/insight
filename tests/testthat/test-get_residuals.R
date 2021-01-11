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
    get_residuals(m, weighted = FALSE, type = "response")
    get_residuals(m, weighted = FALSE, type = "deviance")
    expect_equal(
      as.vector(insight::get_residuals(m)),
      as.vector(residuals(m))
    )
    expect_equal(
      insight::get_residuals(m, weighted = TRUE),
      as.vector(weighted.residuals(m))
    )
  })

  test_that("get_residuals - glm", {
    m <- suppressWarnings(glm(am ~ cyl, weights = w, data = mtcars, family = binomial))
    get_residuals(m, weighted = FALSE, type = "response")
    get_residuals(m, weighted = FALSE, type = "deviance")
    expect_equal(
      as.vector(insight::get_residuals(m)),
      as.vector(residuals(m))
    )
    expect_equal(
      insight::get_residuals(m, weighted = TRUE),
      as.vector(weighted.residuals(m))
    )
  })

  test_that("get_residuals - lmer", {
    m <- lmer(Reaction ~ Days + (Days | Subject), weights = w, data = sleepstudy)
    get_residuals(m, weighted = FALSE, type = "response")
    get_residuals(m, weighted = FALSE, type = "deviance")
    expect_equal(
      as.vector(insight::get_residuals(m)),
      as.vector(residuals(m))
    )
    expect_equal(
      insight::get_residuals(m, weighted = TRUE),
      as.vector(weighted.residuals(m))
    )
  })

  test_that("get_residuals - glmer", {
    m <- glmer(cbind(incidence, size - incidence) ~ period + (1 | herd), weights = w, data = cbpp, family = binomial, nAGQ = 0)
    get_residuals(m, weighted = FALSE, type = "response")
    get_residuals(m, weighted = FALSE, type = "deviance")
    expect_equal(
      as.vector(insight::get_residuals(m)),
      as.vector(residuals(m))
    )
    expect_equal(
      insight::get_residuals(m, weighted = TRUE),
      as.vector(weighted.residuals(m))
    )
  })

}
