# if (require("testthat") && require("insight") && require("brms")) {
#   data(mtcars)
#   set.seed(123)
#
#   model <- brms::brm(mpg ~ wt, data = mtcars, seed = 1, refresh = 0)
#   priors <- insight::get_priors(model)
#
#   test_that("get_dispersion", {
#     expect_true(is.na(priors[priors$Parameter == "b_wt", "Location"]))
#   })
# }