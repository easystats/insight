if (requiet("testthat") && requiet("insight") && requiet("lme4")) {
  data(mtcars)
  data(sleepstudy)

  m1 <- lm(mpg ~ wt + cyl + vs, data = mtcars)
  m2 <- lm(mpg ~ wt + cyl, data = mtcars)
  m3 <- lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy)
  m4 <- glm(formula = vs ~ wt, family = binomial(), data = mtcars)

  test_that("all_models_equal", {
    expect_true(all_models_equal(m1, m2))
    expect_false(all_models_equal(m1, m2, mtcars))
    expect_message(expect_false(all_models_equal(m1, m2, mtcars, verbose = TRUE)))
    expect_false(all_models_equal(m1, m2, m3))
    expect_message(expect_false(all_models_equal(m1, m4, m2, m3, verbose = TRUE)))

    expect_true(is_model_supported(m1))
    expect_false(is_model_supported(mtcars))
  })
}
