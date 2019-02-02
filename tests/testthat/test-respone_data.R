.runThisTest <- Sys.getenv("RunAllinsightTests") == "yes"

if (.runThisTest) {

  if (suppressWarnings(
    require("testthat") &&
    require("insight") &&
    require("brms")
  )) {
    context("insight, brms-find_response")

    data("epilepsy")
    data("iris")

    bprior1 <- prior(student_t(5,0,10), class = b) + prior(cauchy(0,2), class = sd)

    m1 <- brm(
      count ~ log_Age_c + log_Base4_c * Trt + (1|patient),
      data = epilepsy,
      family = poisson(),
      prior = bprior1,
      chains = 1,
      iter = 500
    )

    f1 <- bf(Sepal.Length ~ Petal.Length + Sepal.Width + Species)
    f2 <- bf(Sepal.Width ~ Species)
    m2 <- brm(f1 + f2 + set_rescor(FALSE), data = iris, chains = 1, iter = 500)

    dat <- read.table(header = TRUE, text = "
      n r r/n group treat c2 c1 w
      62 3 0.048387097 1 0 0.1438 1.941115288 1.941115288
      96 1 0.010416667 1 0 0.237 1.186583128 1.186583128
      17 0 0 0 0 0.2774 1.159882668 3.159882668
      41 2 0.048780488 1 0 0.2774 1.159882668 3.159882668
      212 170 0.801886792 0 0 0.2093 1.133397521 1.133397521
      143 21 0.146853147 1 1 0.1206 1.128993008 1.128993008
      143 0 0 1 1 0.1707 1.128993008 2.128993008
      143 33 0.230769231 0 1 0.0699 1.128993008 1.128993008
      73 62 1.260273973 0 1 0.1351 1.121927228 1.121927228
      73 17 0.232876712 0 1 0.1206 1.121927228 1.121927228"
    )
    dat$treat <- as.factor(dat$treat)

    m3 <- brm(r | trials(n) ~ treat * c2, data = dat, family = binomial(link = logit))

    test_that("find_response", {
      expect_equal(find_response(m1, combine = TRUE), "count")
      expect_equal(find_response(m2, combine = TRUE), c(SepalLength = "Sepal.Length", SepalWidth = "Sepal.Width"))
      expect_equal(find_response(m3, combine = TRUE), c("r", "n"))
      expect_equal(find_response(m1, combine = FALSE), "count")
      expect_equal(find_response(m2, combine = FALSE), c(SepalLength = "Sepal.Length", SepalWidth = "Sepal.Width"))
      expect_equal(find_response(m3, combine = FALSE), c("r", "n"))
    })

    test_that("get_response", {
      expect_length(get_response(m1), 236)
      expect_equal(ncol(get_response(m2)), 2)
      expect_equal(colnames(get_response(m2)), c("Sepal.Length", "Sepal.Width"))
      expect_equal(ncol(get_response(m3)), 2)
      expect_equal(colnames(get_response(m3)), c("r", "n"))
    })
  }
}
