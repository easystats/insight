if (require("testthat") && require("insight") && require("lme4")  && require("rstanarm")) {
  data(mtcars)

  test_that("get_deviance - Bayesian lm", {
    m1 <- lm(mpg ~ disp, data=mtcars)
    m2 <- rstanarm::stan_glm(mpg ~ disp, data=mtcars, refresh=0)
    expect_equal(get_deviance(m1), get_deviance(m2), tolerance = 1e-1)
  })

  test_that("get_deviance - Bayesian glm", {
    m1 <- glm(vs ~ disp, data=mtcars, family="binomial")
    m2 <- rstanarm::stan_glm(vs ~ disp, data=mtcars, family="binomial", refresh=0)
    expect_equal(get_deviance(m1), get_deviance(m2), tolerance = 1e-1)
  })
}
