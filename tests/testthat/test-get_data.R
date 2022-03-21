
test_that("lme4", {
  requiet("lme4")
  data("cbpp", package = "lme4")
  set.seed(123)
  cbpp$cont <- rnorm(nrow(cbpp))
  m <- glmer(cbind(incidence, size - incidence) ~ poly(cont, 2) + (1 | herd),
    data = cbpp, family = binomial)
  expect_s3_class(get_data(m), "data.frame")
})

test_that("lm", {
  set.seed(1023)
  x <- rnorm(1000, sd = 4)
  y <- cos(x) + rnorm(1000)
  # fails if we assign this locally
  dat <<- data.frame(x, y)
  mod1 <- lm(y ~ x, data = dat)
  mod2 <- lm(y ~ cos(x), data = dat)
  expect_equal(get_data(mod1), get_data(mod2), ignore_attr = TRUE)
  expect_equal(get_data(mod1)$x, dat$x, ignore_attr = TRUE)
  expect_equal(get_data(mod2)$x, dat$x, ignore_attr = TRUE)
})

test_that("lm with transformations", {
  d <- data.frame(
    time = as.factor(c(1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5)),
    group = c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2),
    sum = c(0, 5, 10, 15, 20, 0, 20, 25, 45, 50, 0, 5, 10, 15, 20, 0, 20, 25, 45, 50, 0, 5, 10, 15, 20, 0, 20, 25, 45, 50)
  )
  m <- lm(log(sum + 1) ~ as.numeric(time) * group, data = d)
  expect_equal(colnames(get_data(m)), c("sum", "time", "group"))
})

test_that("mgcv", {
  skip("works interactively")
  requiet("mgcv")
  d <- iris
  d$NewFac <- rep(c(1, 2), length.out = 150)
  model <- gam(Sepal.Length ~ s(Petal.Length, by = interaction(Species, NewFac)), data = d)
  expect_equal(
    head(insight::get_data(model)),
    head(d[c("Sepal.Length", "Petal.Length", "Species", "NewFac")]),
    ignore_attr = TRUE
  )
})



.runThisTest <- Sys.getenv("RunAllinsightTests") == "yes"

if (.runThisTest) {
  data(iris)
  m <- lm(Sepal.Length ~ Sepal.Width, data = iris)
  out <- get_data(m)
  expect_false(attributes(out)$is_subset)
  expect_equal(colnames(out), c("Sepal.Length", "Sepal.Width"))
  expect_equal(nrow(out), 150)

  m <- lm(Sepal.Length ~ Sepal.Width, data = iris, subset = Species == "versicolor")
  out <- get_data(m)
  expect_true(attributes(out)$is_subset)
  expect_equal(colnames(out), c("Sepal.Length", "Sepal.Width", "Species"))
  expect_equal(nrow(out), 50)

  d <- iris
  m <- lm(Petal.Length ~ poly(Sepal.Length), data = d)
  d <- mtcars
  expect_warning(expect_warning(out <- get_data(m)))
  expect_equal(colnames(out), c("Petal.Length", "Sepal.Length"))

  data(iris)
  m <- lm(log(Sepal.Length) ~ sqrt(Sepal.Width), data = iris)
  out <- get_data(m)
  expect_equal(out, iris[c("Sepal.Length", "Sepal.Width")], ignore_attr = TRUE)

  m <- lm(log(Sepal.Length) ~ scale(Sepal.Width), data = iris)
  out <- get_data(m)
  expect_equal(out, iris[c("Sepal.Length", "Sepal.Width")], ignore_attr = TRUE)

  requiet("brms")
  m <- brms::brm(mpg ~ hp + mo(cyl), data = mtcars, refresh = 0, iter = 200, chains = 1)
  out <- get_data(m)
  expect_equal(attributes(out)$factors, "cyl")
  expect_type(out$cyl, "double")
  expect_equal(colnames(out), c("mpg", "hp", "cyl"))

  out <- get_datagrid(m)
  expect_equal(dim(out), c(10, 2))
  expect_equal(out$cyl, c(4, 4, 6, 6, 8, 8, 8, 8, 8, 8))
}
