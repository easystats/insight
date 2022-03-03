if (requiet("testthat") && requiet("insight")) {
  data(iris)

  m1 <- lm(Sepal.Length ~ Petal.Width + Species, data = iris)
  m2 <- lm(Sepal.Length ~ Species, data = iris)
  m3 <- lm(Sepal.Length ~ Species, data = iris)
  m4 <- lm(Sepal.Length ~ 1, data = iris)

  test_that("ellipses_info", {
    expect_message(ellipsis_info(m1, m2, m3, m4))
  })

  info <- ellipsis_info(m1, m2, m4)
  test_that("ellipses_info", {
    expect_false(attributes(info)$same_fixef)
    expect_true(attributes(info)$is_nested)
    expect_true(attributes(info)$is_nested_decreasing)
    expect_false(attributes(info)$is_nested_increasing)
    expect_false(isTRUE(attributes(info)$all_mixed_models))
  })

  info <- ellipsis_info(m4, m3, m1)
  test_that("ellipses_info", {
    expect_true(attributes(info)$is_nested)
    expect_false(attributes(info)$is_nested_decreasing)
    expect_true(attributes(info)$is_nested_increasing)
  })

  test_that("ellipses_info - single model", {
    out <- ellipsis_info(m1)
    expect_equal(out, m1)
  })

  m1 <- lm(Sepal.Length ~ Petal.Width + Species, data = iris)
  m2 <- lm(Sepal.Width ~ Petal.Width + Species, data = iris)
  m3 <- lm(Petal.Length ~ Petal.Width + Species, data = iris)

  info <- ellipsis_info(m1, m2, m3)
  test_that("ellipses_info, same fixed effects", {
    expect_true(attributes(info)$same_fixef)
    expect_false(attributes(info)$is_nested)
  })

  if (requiet("lme4")) {
    data(sleepstudy)
    m1 <- lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy)
    m2 <- lmer(Reaction ~ Days + (1 | Subject) + (1 | Days), data = sleepstudy)

    info <- ellipsis_info(m1, m2)
    test_that("ellipses_info, random effects", {
      expect_true(attributes(info)$same_fixef)
      expect_false(attributes(info)$same_ranef)
      expect_true(attributes(info)$re_nested)
      expect_true(attributes(info)$all_mixed_models)
      expect_true(attributes(info)$re_nested_increasing)
      expect_false(attributes(info)$re_nested_decreasing)
    })

    info <- ellipsis_info(m2, m1)
    test_that("ellipses_info, random effects", {
      expect_true(attributes(info)$re_nested)
      expect_false(attributes(info)$re_nested_increasing)
      expect_true(attributes(info)$re_nested_decreasing)
    })

    m1 <- lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy)
    m2 <- lmer(Reaction ~ 1 + (1 | Subject), data = sleepstudy)

    info <- ellipsis_info(m1, m2)
    test_that("ellipses_info, random effects", {
      expect_false(attributes(info)$same_fixef)
      expect_true(attributes(info)$same_ranef)
      expect_true(attributes(info)$re_nested)
      expect_true(attributes(info)$all_mixed_models)
      expect_true(attributes(info)$re_nested_increasing)
      expect_true(attributes(info)$re_nested_decreasing)
    })
  }
}
