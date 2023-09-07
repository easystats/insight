skip_if_not_installed("lme4")

m1 <- lme4::lmer(mpg ~ am + (1 | cyl), data = mtcars)
m2 <- lm(mpg ~ am, data = mtcars)

test_that("get_weights", {
  expect_null(get_weights(m1))
  expect_null(get_weights(m2))
})

set.seed(123)
mtcars$w <- abs(rnorm(nrow(mtcars), sd = 0.5))

m1 <- lme4::lmer(mpg ~ am + (1 | cyl), data = mtcars, weights = w)
m2 <- lm(mpg ~ am, data = mtcars, weights = w)
m3 <- suppressWarnings(glm(am ~ mpg + as.factor(vs), data = mtcars, weights = w, family = binomial()))
m4 <- glm(am ~ mpg + as.factor(vs), data = mtcars, weights = w, family = quasibinomial())

test_that("get_weights", {
  expect_equal(
    get_weights(m1),
    mtcars$w,
    tolerance = 1e-2,
    ignore_attr = TRUE
  )
  expect_equal(
    get_weights(m2),
    mtcars$w,
    tolerance = 1e-2,
    ignore_attr = TRUE
  )
  expect_equal(
    get_weights(m3),
    mtcars$w,
    tolerance = 1e-2,
    ignore_attr = TRUE
  )
  expect_equal(
    get_weights(m4),
    mtcars$w,
    tolerance = 1e-2,
    ignore_attr = TRUE
  )
})


test_that("get_weights, with missing", { # 754
  set.seed(123)
  mtcars2 <- mtcars
  mtcars2$hp[sample(seq_len(nrow(mtcars)), 5)] <- NA
  mtcars2$w <- abs(rnorm(nrow(mtcars), sd = 0.5))
  m_w1 <- glm(am ~ hp, na.action = na.exclude, data = mtcars2, family = binomial())
  expect_null(get_weights(m_w1))
  m_w2 <- suppressWarnings(glm(am ~ hp, na.action = na.exclude, data = mtcars2, weights = w, family = binomial()))
  expect_equal(weights(m_w2), get_weights(m_w2), tolerance = 1e-4, ignore_attr = TRUE)
})


skip_if_not_installed("nlme")
data("Orthodont", package = "nlme")
m <- nlme::lme( # a model of variance only
  distance ~ 1,
  data = nlme::Orthodont, # grand mean
  weights = nlme::varConstPower(form = ~ age | Sex)
)

out <- get_weights(m)
test_that("get_weights nlme", {
  expect_identical(colnames(out), c("age", "Sex"))
})
