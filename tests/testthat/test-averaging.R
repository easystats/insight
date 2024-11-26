skip_if_not_installed("MuMIn")
skip_if_not_installed("withr")
skip_on_cran()

withr::with_options(
  list(na.action = "na.fail"),
  test_that("MuMIn link functions", {
    library(MuMIn) # nolint
    set.seed(1234)
    dat <- data.frame(
      outcome = rbinom(n = 100, size = 1, prob = 0.35),
      var_binom = as.factor(rbinom(n = 100, size = 1, prob = 0.2)),
      var_cont = rnorm(n = 100, mean = 10, sd = 7),
      group = sample(letters[1:4], size = 100, replace = TRUE),
      stringsAsFactors = FALSE
    )
    dat$var_cont <- as.vector(scale(dat$var_cont))
    m1 <- glm(
      outcome ~ var_binom + var_cont,
      data = dat,
      family = binomial(link = "logit")
    )
    out <- MuMIn::model.avg(MuMIn::dredge(m1), fit = TRUE)
    expect_equal(link_function(out), link_function(m1), ignore_attr = TRUE)
    expect_equal(link_inverse(out), link_inverse(m1), ignore_attr = TRUE)
  })
)

unloadNamespace("MuMIn")
