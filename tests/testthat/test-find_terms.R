skip_if_not_installed("lme4")

test_that("find_terms by formula", {
  data(mtcars)
  m <- lm(mpg ~ log(hp) * (am + factor(cyl)), data = mtcars)
  ## FIXME: this is currently wrong behaviour
  expect_identical(
    find_terms(m),
    list(response = "mpg", conditional = c("log(hp)", "(am", "factor(cyl))"))
  )
  expect_identical(
    find_terms(m, as_term_labels = TRUE),
    list(conditional = c(
      "log(hp)", "am", "factor(cyl)", "log(hp):am",
      "log(hp):factor(cyl)"
    ))
  )
})

test_that("find_terms", {
  m <- lm(Sepal.Length ~ -1 + Petal.Width + Species, data = iris)
  expect_identical(
    find_terms(m),
    list(response = "Sepal.Length", conditional = c("Petal.Width", "Species", "-1"))
  )
  expect_false(has_intercept(m))
})

test_that("find_terms", {
  m <- lm(Sepal.Length ~ 0 + Petal.Width + Species, data = iris)
  expect_identical(
    find_terms(m),
    list(response = "Sepal.Length", conditional = c("0", "Petal.Width", "Species"))
  )
  expect_false(has_intercept(m))
})

test_that("find_terms", {
  m <- lm(Sepal.Length ~ Petal.Width + Species - 1, data = iris)
  expect_identical(
    find_terms(m),
    list(response = "Sepal.Length", conditional = c("Petal.Width", "Species", "-1"))
  )
  expect_false(has_intercept(m))
})

set.seed(1984)
dat <- data.frame(
  y = rnorm(100 * 5, sd = 1 - 0.20),
  time = rep(1:10, 10 * 5),
  g1 = sort(rep(1:100, 5)),
  g2 = sort(rep(1:10, 10 * 5))
)
dat$g0 <- paste(dat$time, dat$g1)
dat$time1 <- dat$time - 8
dat$post <- 0
dat$post[dat$time >= 8] <- 1
m <- suppressMessages(lme4::lmer(y ~ post + time1 + (post + time1 - 1 | g2), data = dat))

test_that("find_terms", {
  expect_identical(
    find_terms(m),
    list(response = "y", conditional = c("post", "time1"), random = c("post", "time1", "g2"))
  )
  expect_true(has_intercept(m))
})

test_that("find_terms, - in response", {
  m <- lm(Sepal.Length - Sepal.Width ~ Species, data = iris)
  expect_identical(find_terms(m)$response, c("Sepal.Length", "Sepal.Width"))
  m <- lm(cbind(Sepal.Length - Sepal.Width) ~ Species, data = iris)
  expect_identical(find_terms(m)$response, "cbind(Sepal.Length - Sepal.Width)")
  m <- lm(Sepal.Length^-0.5 ~ Species, data = iris)
  expect_identical(find_terms(m)$response, "Sepal.Length^-0.5")
  m <- lm(I(Sepal.Length^-0.5) ~ Species, data = iris)
  expect_identical(find_terms(m)$response, "I(Sepal.Length^-0.5)")
})

test_that("find_terms, - response as is", {
  data(iris)
  m <- lm(1 / Sepal.Length ~ Species, data = iris)
  expect_identical(find_terms(m)$response, "1/Sepal.Length")
})

test_that("find_terms, - box cox", {
  data(mtcars)
  model <- lm(mpg / 0.7 ~ hp, data = mtcars)
  expect_identical(
    find_terms(model),
    list(response = c("mpg", "0.7"), conditional = "hp")
  )
  model <- lm(I(mpg / 0.7) ~ hp, data = mtcars)
  expect_identical(
    find_terms(model),
    list(response = "I(mpg/0.7)", conditional = "hp")
  )
  model <- lm((mpg^0.7 - 1) / 0.7 ~ hp, data = mtcars)
  expect_identical(
    find_terms(model),
    list(response = c("(mpg^0.7 - 1)", "0.7"), conditional = "hp")
  )
  model <- lm(I((mpg^0.7 - 1) / 0.7) ~ hp, data = mtcars)
  expect_identical(
    find_terms(model),
    list(response = "I((mpg^0.7 - 1)/0.7)", conditional = "hp")
  )
  model <- lm((mpg^-1.3 - 1) / -1.3 ~ hp, data = mtcars)
  expect_identical(
    find_terms(model),
    list(response = c("(mpg^-1.3 - 1)", "-1.3"), conditional = "hp")
  )
})
