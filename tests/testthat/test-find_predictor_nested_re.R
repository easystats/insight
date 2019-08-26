if (require("testthat") && require("insight") && require("lme4")) {
  context("insight, find_predictors")

  set.seed(1984)
  dat <- data.frame(y = rnorm(1000 * 5, sd = 1 - .20),
                    time = rep(1:10, 100 * 5),
                    g1 = sort(rep(1:100, 10 * 5)),
                    g2 = sort(rep(1:10, 100 * 5)))
  dat$g0 <- paste(dat$time, dat$g1)
  dat$time1 <- dat$time - 8
  dat$post <- 0
  dat$post[dat$time >= 8] <- 1
  m <- lmer(y ~ post + time1 + (1 | g2 / g1 / g0) + (post + time1 - 1 | g2), data = dat)

  test_that("clean_names", {
    expect_equal(
      find_predictors(m, effects = "all"),
      list(conditional = c("post", "time1"), random = c("g0", "g1", "g2"))
    )
  })
}
