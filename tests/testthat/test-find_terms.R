if (require("testthat") && require("insight") && require("lme4")) {

  test_that("find_terms", {
    m <- lm(Sepal.Length ~ -1 + Petal.Width + Species, data = iris)
    expect_equal(
      find_terms(m),
      list(response = "Sepal.Length", conditional = c("Petal.Width", "Species", "-1"))
    )
    expect_false(has_intercept(m))
  })

  test_that("find_terms", {
    m <- lm(Sepal.Length ~ 0 + Petal.Width + Species, data = iris)
    expect_equal(
      find_terms(m),
      list(response = "Sepal.Length", conditional = c("0", "Petal.Width", "Species"))
    )
    expect_false(has_intercept(m))
  })

  test_that("find_terms", {
    m <- lm(Sepal.Length ~ Petal.Width + Species - 1, data = iris)
    expect_equal(
      find_terms(m),
      list(response = "Sepal.Length", conditional = c("Petal.Width", "Species", "-1"))
    )
    expect_false(has_intercept(m))
  })

  set.seed(1984)
  dat <- data.frame(
    y = rnorm(100 * 5, sd = 1 - .20),
    time = rep(1:10, 10 * 5),
    g1 = sort(rep(1:100, 5)),
    g2 = sort(rep(1:10, 10 * 5))
  )
  dat$g0 <- paste(dat$time, dat$g1)
  dat$time1 <- dat$time - 8
  dat$post <- 0
  dat$post[dat$time >= 8] <- 1
  m <- lmer(y ~ post + time1 + (post + time1 - 1 | g2), data = dat)

  test_that("find_terms", {
    expect_equal(
      find_terms(m),
      list(response = "y", conditional = c("post", "time1"), random = c("post", "time1", "g2"))
    )
    expect_true(has_intercept(m))
  })
}
