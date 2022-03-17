if (requiet("testthat") && requiet("insight") && requiet("lme4")) {
  data("cbpp")
  set.seed(123)
  cbpp$cont <- rnorm(nrow(cbpp))
  m <- glmer(cbind(incidence, size - incidence) ~ poly(cont, 2) + (1 | herd),
    data = cbpp, family = binomial
  )

  test_that("get_data", {
    expect_s3_class(get_data(m), "data.frame")
  })

  d <- data.frame(
    time = as.factor(c(1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5)),
    group = c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2),
    sum = c(0, 5, 10, 15, 20, 0, 20, 25, 45, 50, 0, 5, 10, 15, 20, 0, 20, 25, 45, 50, 0, 5, 10, 15, 20, 0, 20, 25, 45, 50)
  )
  m <- lm(log(sum + 1) ~ as.numeric(time) * group, data = d)

  test_that("get_data", {
    expect_equal(colnames(get_data(m)), c("sum", "time", "group"))
  })

  set.seed(1023)
  x <- rnorm(1000, sd = 4)
  y <- cos(x) + rnorm(1000)
  dat <<- data.frame(x, y)
  mod1 <- lm(y ~ x, data = dat)
  mod2 <- lm(y ~ cos(x), data = dat)

  test_that("get_data", {
    expect_equal(get_data(mod1), get_data(mod2), ignore_attr = TRUE)
    expect_equal(get_data(mod1)$x, dat$x, ignore_attr = TRUE)
    expect_equal(get_data(mod2)$x, dat$x, ignore_attr = TRUE)
  })

  if (requiet("mgcv")) {
    d <- iris
    d$NewFac <- rep(c(1, 2), length.out = 150)
    model <- mgcv::gam(Sepal.Length ~ s(Petal.Length, by = interaction(Species, NewFac)), data = d)

    test_that("get_data", {
      expect_equal(
        head(insight::get_data(model)),
        head(d[c("Sepal.Length", "Petal.Length", "Species", "NewFac")]),
        ignore_attr = TRUE
      )
    })
  }
}
