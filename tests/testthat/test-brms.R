.runThisTest <- Sys.getenv("RunAllinsightTests") == "yes"

if (.runThisTest && Sys.getenv("USER") != "travis") {
  if (suppressWarnings(
    require("testthat") &&
      require("insight") &&
      require("brms")
  )) {
    context("insight, brms-find_response")




# Model fitting -----------------------------------------------------------

    data("epilepsy")
    data("iris")
    zinb <- read.csv("http://stats.idre.ucla.edu/stat/data/fish.csv")

    bprior1 <- prior(student_t(5, 0, 10), class = b) + prior(cauchy(0, 2), class = sd)

    m1 <- brm(
      count ~ log_Age_c + log_Base4_c * Trt + (1 | patient),
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
      73 17 0.232876712 0 1 0.1206 1.121927228 1.121927228")
    dat$treat <- as.factor(dat$treat)

    m3 <- brm(r | trials(n) ~ treat * c2, data = dat, family = binomial(link = logit), chains = 1, iter = 500)

    m4 <- brm(
      bf(count ~ child + camper + (1 | persons),
         zi ~ child + camper + (1 | persons)),
      data = zinb,
      family = zero_inflated_poisson(),
      chains = 1,
      iter = 500
    )

    zinb$count2 <- rpois(250, 5)
    zinb$count2[sample(1:250, 100, replace = FALSE)] <- 0

    bf1 <- bf(count ~ child + camper + (1 | persons), zi ~ camper + (1 | persons))
    bf2 <- bf(count2 ~ child + livebait + (1 | persons), zi ~ child + (1 | persons))

    m5 <- brm(bf1 + bf2, data = zinb,
      family = zero_inflated_poisson(),
      chains = 1,
      iter = 500
    )


# Tests -------------------------------------------------------------------


    test_that("clean_names", {
      expect_identical(clean_names(m1), c("count", "log_Age_c", "log_Base4_c", "Trt", "patient"))
      expect_identical(clean_names(m2), c("Sepal.Length", "Sepal.Width", "Petal.Length", "Species"))
      expect_identical(clean_names(m3), c("r", "n", "treat", "c2"))
      expect_identical(clean_names(m4), c("count", "child", "camper", "persons"))
    })


    test_that("find_predictors", {
      expect_identical(find_predictors(m1), list(conditional = c("log_Age_c", "log_Base4_c", "Trt")))
      expect_identical(find_predictors(m1, flatten = TRUE), c("log_Age_c", "log_Base4_c", "Trt"))
      expect_identical(find_predictors(m1, effects = "all", component = "all"), list(conditional = c("log_Age_c", "log_Base4_c", "Trt"), random = "patient"))
      expect_identical(find_predictors(m1, effects = "all", component = "all", flatten = TRUE), c("log_Age_c", "log_Base4_c", "Trt", "patient"))

      expect_identical(
        find_predictors(m2),
        list(
          SepalLength = list(conditional = c("Petal.Length", "Sepal.Width", "Species")),
          SepalWidth = list(conditional = "Species")
      ))

      expect_identical(find_predictors(m2, flatten = TRUE), c("Petal.Length", "Sepal.Width", "Species"))
      expect_identical(find_predictors(m3), list(conditional = c("treat", "c2")))
      expect_identical(find_predictors(m4), list(conditional = c("child", "camper"), zero_inflated = c("child", "camper")))
      expect_identical(find_predictors(m4, effects = "random"), list(random = "persons", zero_inflated_random = "persons"))
      expect_identical(find_predictors(m4, flatten = TRUE), c("child", "camper"))
    })

    test_that("find_response", {
      expect_equal(find_response(m1, combine = TRUE), "count")
      expect_equal(find_response(m2, combine = TRUE), c(SepalLength = "Sepal.Length", SepalWidth = "Sepal.Width"))
      expect_equal(find_response(m3, combine = TRUE), c("r", "n"))
      expect_equal(find_response(m1, combine = FALSE), "count")
      expect_equal(find_response(m2, combine = FALSE), c(SepalLength = "Sepal.Length", SepalWidth = "Sepal.Width"))
      expect_equal(find_response(m3, combine = FALSE), c("r", "n"))
      expect_equal(find_response(m4, combine = FALSE), "count")
    })

    test_that("get_response", {
      expect_length(get_response(m1), 236)
      expect_equal(ncol(get_response(m2)), 2)
      expect_equal(colnames(get_response(m2)), c("Sepal.Length", "Sepal.Width"))
      expect_equal(ncol(get_response(m3)), 2)
      expect_equal(colnames(get_response(m3)), c("r", "n"))
      expect_length(get_response(m4), 250)
    })

    test_that("find_terms", {
      expect_identical(find_terms(m1), list(response = "count", conditional = c("log_Age_c", "log_Base4_c", "Trt"), random = "patient"))
      expect_identical(find_terms(m1, effects = "fixed"), list(response = "count", conditional = c("log_Age_c", "log_Base4_c", "Trt")))
      expect_null(find_terms(m1, component = "zi"))

      expect_identical(
        find_terms(m2),
        list(
          response = c(SepalLength = "Sepal.Length", SepalWidth = "Sepal.Width"),
          SepalLength = list(conditional = c("Petal.Length", "Sepal.Width", "Species")),
          SepalWidth = list(conditional = "Species")
        ))

      expect_identical(find_terms(m2, flatten = TRUE), c("Sepal.Length", "Sepal.Width", "Petal.Length", "Species"))
      expect_identical(find_terms(m3), list(response = c("r", "n"), conditional = c("treat", "c2")))

      expect_identical(
        find_terms(m4),
        list(
          response = "count",
          conditional = c("child", "camper"),
          random = "persons",
          zero_inflated = c("child", "camper"),
          zero_inflated_random = "persons"
        ))

      expect_identical(find_terms(m4, flatten = TRUE), c("count", "child", "camper", "persons"))
    })

    test_that("n_obs", {
      expect_equal(n_obs(m1), 236)
      expect_equal(n_obs(m2), 150)
      expect_equal(n_obs(m3), 10)
      expect_equal(n_obs(m4), 250)
      expect_equal(n_obs(m5), 250)
    })

  }
}
