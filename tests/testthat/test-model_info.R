test_that("glm bernoulli", {
  data(mtcars)
  model <- glm(vs ~ disp, data = mtcars, family = binomial())
  mi <- model_info(model)
  expect_true(mi$is_binomial)
  expect_true(mi$is_bernoulli)
})

test_that("geeglm bernoulli", {
  skip_if_not_installed("geepack")
  data(mtcars)
  model <- geepack::geeglm(
    vs ~ disp,
    data = mtcars,
    id = cyl,
    family = binomial()
  )
  mi <- model_info(model)
  expect_true(mi$is_binomial)
  expect_true(mi$is_bernoulli)
})

test_that("bigglm bernoulli", {
  skip_if_not_installed("bigglm")
  data(mtcars)
  model <- biglm::bigglm(
    vs ~ disp,
    family = binomial(),
    data = mtcars
  )
  mi <- model_info(model)
  expect_true(mi$is_binomial)
  expect_true(mi$is_bernoulli)
})

test_that("glmmTMB bernoulli", {
  skip_if_not_installed("glmmTMB")
  data(mtcars)
  model <- glmmTMB::glmmTMB(vs ~ disp, data = mtcars, family = binomial())
  mi <- model_info(model)
  expect_true(mi$is_binomial)
  expect_true(mi$is_bernoulli)

  model <- glmmTMB::glmmTMB(vs ~ disp + (1 | cyl), data = mtcars, family = binomial())
  mi <- model_info(model)
  expect_true(mi$is_binomial)
  expect_true(mi$is_bernoulli)
})

test_that("glmer bernoulli", {
  skip_if_not_installed("lme4")
  data(mtcars)
  model <- lme4::glmer(vs ~ disp + (1 | cyl), data = mtcars, family = binomial())
  mi <- model_info(model)
  expect_true(mi$is_binomial)
  expect_true(mi$is_bernoulli)
})

test_that("model_info-BF-proptest", {
  skip_if_not_installed("BayesFactor")
  model <- BayesFactor::proportionBF(15, 25, p = 0.5)
  mi <- model_info(model)
  expect_true(mi$is_binomial)
  expect_false(mi$is_linear)
})

test_that("model_info-proptest", {
  model <- prop.test(15, 25, p = 0.5)
  mi <- model_info(model)
  expect_true(mi$is_binomial)
  expect_false(mi$is_linear)
  expect_false(mi$is_correlation)
})

test_that("model_info-tweedie", {
  skip_if_not_installed("tweedie")
  skip_if_not_installed("statmod")
  d <- data.frame(x = 1:20, y = rgamma(20, shape = 5))
  # Fit a poisson generalized linear model with identity link
  model <- glm(y ~ x, data = d, family = statmod::tweedie(var.power = 1, link.power = 1))
  mi <- model_info(model)
  expect_true(mi$is_tweedie)
  expect_false(mi$is_poisson)
  expect_identical(mi$family, "Tweedie")
})

test_that("model_info, glm bernoulli", {
  set.seed(1)
  tot <- rep(10, 100)
  suc <- rbinom(100, prob = 0.9, size = tot)
  dat <- data.frame(tot, suc)
  dat$prop <- suc / tot

  mod <- glm(prop ~ 1,
    family = binomial,
    data = dat,
    weights = tot
  )

  expect_true(model_info(mod)$is_binomial)
  expect_false(model_info(mod)$is_bernoulli)

  data(mtcars)
  mod <- glm(am ~ 1, family = binomial, data = mtcars)
  expect_true(model_info(mod)$is_binomial)
  expect_true(model_info(mod)$is_bernoulli)
})
