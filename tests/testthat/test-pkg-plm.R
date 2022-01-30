skip_if_not(isTRUE(Sys.getenv("RunAllinsightTests") == "yes"))
skip_if_not(getRversion() > "3.5")
requiet("plm")

test_that("plm()", {
  set.seed(123)
  data(Crime)
  data("Produc", package = "plm")
  m1 <- plm(lcrmrte ~ lprbarr + factor(year) | . - lprbarr + lmix, data = Crime, model = "random")
  m2 <- plm(
    formula = log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp,
    data = Produc,
    index = c("state", "year"))

  # model_info
  expect_true(model_info(m1)$is_linear)

  # find_predictors
  expect_identical(
    find_predictors(m1),
    list(conditional = c("lprbarr", "year"),
         instruments = c("lprbarr", "lmix")))
  expect_identical(find_predictors(m1, flatten = TRUE), c("lprbarr", "year", "lmix"))
  expect_null(find_predictors(m1, effects = "random"))

  # find_random
  expect_null(find_random(m1))

  # get_random
  expect_warning(get_random(m1))

  # find_response
  expect_identical(find_response(m1), "lcrmrte")

  # get_response
  expect_equal(get_response(m1), Crime$lcrmrte)

  # get_predictors
  expect_equal(colnames(get_predictors(m1)), c("lprbarr", "year", "lmix"))

  # link_inverse
  expect_equal(link_inverse(m1)(.2), .2, tolerance = 1e-5)

  # get_data
  expect_equal(nrow(get_data(m1)), 630)
  expect_equal(colnames(get_data(m1)), c("lcrmrte", "lprbarr", "year", "lmix"))

  expect_equal(nrow(get_data(m2)), 816)
  expect_equal(colnames(get_data(m2)), c("gsp", "pcap", "pc", "emp", "unemp", "state", "year"))

  # find_formula
  expect_length(find_formula(m1), 2)
  expect_equal(
    find_formula(m1),
    list(conditional = as.formula("lcrmrte ~ lprbarr + factor(year)"),
         instruments = as.formula("~-lprbarr + lmix")),
    ignore_attr = TRUE)

  # find_variables
  expect_equal(find_variables(m1),
               list(response = "lcrmrte",
                    conditional = c("lprbarr", "year"),
                    instruments = c("lprbarr", "lmix")))
  expect_equal(find_variables(m1, flatten = TRUE),
               c("lcrmrte", "lprbarr", "year", "lmix"))

  # n_obs
  expect_equal(n_obs(m1), 630)

  # linkfun
  expect_false(is.null(link_function(m1)))

  # find_parameters
  expect_equal(
    find_parameters(m1),
    list(conditional = c("(Intercept)",
                         "lprbarr",
                         "factor(year)82",
                         "factor(year)83",
                         "factor(year)84",
                         "factor(year)85",
                         "factor(year)86",
                         "factor(year)87")))
  expect_equal(nrow(get_parameters(m1)), 8)

  # is_multivariate
  expect_false(is_multivariate(m1))

  # find_statistic
  expect_identical(find_statistic(m1), "z-statistic")
  expect_identical(find_statistic(m2), "t-statistic")
})

