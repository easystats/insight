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

test_that("ellipses_info - list of models", {
  expect_message(out <- ellipsis_info(list(m1, m2, m3)))
  expect_true(attributes(out)$is_nested)
  expect_named(out, c("m1", "m2", "m3"))
  expect_equal(length(out), 3L)
})

test_that("ellipses_info - names of models for lists", {
  models <- list(m1, m2, m3)
  out <- ellipsis_info(models, verbose = FALSE)
  expect_true(attributes(out)$is_nested)
  expect_named(out, c("Model 1", "Model 2", "Model 3"))
  expect_equal(length(out), 3L)
})


m1 <- lm(Sepal.Length ~ Petal.Width + Species, data = iris)
m2 <- lm(Sepal.Width ~ Petal.Width + Species, data = iris)
m3 <- lm(Petal.Length ~ Petal.Width + Species, data = iris)

info <- ellipsis_info(m1, m2, m3)
test_that("ellipses_info, same fixed effects", {
  expect_true(attributes(info)$same_fixef)
  expect_false(attributes(info)$is_nested)
  expect_true(all(attributes(info)$is_linear))
  expect_false(any(attributes(info)$is_binomial))
})

m1 <- glm(am ~ hp, data = mtcars, family = binomial())
m2 <- glm(am ~ hp + gear, data = mtcars, family = binomial())
m3 <- glm(am ~ hp + cyl + gear, data = mtcars, family = binomial())

info <- ellipsis_info(m1, m2, m3)
test_that("ellipses_info, binomial", {
  expect_true(attributes(info)$is_nested)
  expect_true(all(attributes(info)$is_binomial))
  expect_false(any(attributes(info)$is_linear))
})

skip_if_not_installed("lme4")

data(sleepstudy, package = "lme4")
m1 <- lme4::lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy)
m2 <- suppressMessages(lme4::lmer(Reaction ~ Days + (1 | Subject) + (1 | Days), data = sleepstudy))

info <- ellipsis_info(m1, m2, verbose = FALSE)
test_that("ellipses_info, random effects", {
  expect_true(attributes(info)$same_fixef)
  expect_false(attributes(info)$same_ranef)
  expect_true(attributes(info)$re_nested)
  expect_true(attributes(info)$all_mixed_models)
  expect_true(attributes(info)$re_nested_increasing)
  expect_false(attributes(info)$re_nested_decreasing)
})

info <- ellipsis_info(m2, m1, verbose = FALSE)
test_that("ellipses_info, random effects", {
  expect_true(attributes(info)$re_nested)
  expect_false(attributes(info)$re_nested_increasing)
  expect_true(attributes(info)$re_nested_decreasing)
})

m1 <- lme4::lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy)
m2 <- lme4::lmer(Reaction ~ 1 + (1 | Subject), data = sleepstudy)

info <- ellipsis_info(m1, m2)
test_that("ellipses_info, random effects", {
  expect_false(attributes(info)$same_fixef)
  expect_true(attributes(info)$same_ranef)
  expect_true(attributes(info)$re_nested)
  expect_true(attributes(info)$all_mixed_models)
  expect_true(attributes(info)$re_nested_increasing)
  expect_true(attributes(info)$re_nested_decreasing)
})

test_that("ellipses_info, do.call", {
  data(iris)
  lm1 <- lm(Sepal.Length ~ Species, data = iris)
  lm2 <- lm(Sepal.Length ~ Species + Petal.Length, data = iris)
  lm3 <- lm(Sepal.Length ~ Species * Petal.Length, data = iris)

  out <- do.call(ellipsis_info, list(lm1, lm2, lm3, only_models = TRUE))
  expect_length(out, 3)
  expect_named(out, c("model1", "model2", "model3"))

  out <- ellipsis_info(list(lm1, lm2, lm3), only_models = TRUE)
  expect_length(out, 3)
  expect_named(out, c("lm1", "lm2", "lm3"))

  out <- ellipsis_info(lm1, lm2, lm3, only_models = TRUE)
  expect_length(out, 3)
  expect_named(out, c("lm1", "lm2", "lm3"))
})
