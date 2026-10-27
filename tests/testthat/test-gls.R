skip_if_not_installed("nlme")

data(Ovary, package = "nlme")
m1 <- nlme::gls(
  follicles ~ sin(2 * pi * Time) + cos(2 * pi * Time),
  Ovary,
  correlation = nlme::corAR1(form = ~ 1 | Mare)
)

cr <<- nlme::corAR1(form = ~ 1 | Mare)
m2 <- nlme::gls(
  follicles ~ sin(2 * pi * Time) + cos(2 * pi * Time),
  Ovary,
  correlation = cr
)

set.seed(123)
d <- Ovary
d$x1 <- runif(nrow(d))
d$x2 <- sample(1:10, size = nrow(d), replace = TRUE)
m3 <- nlme::gls(
  follicles ~ Time + x1 + x2,
  d,
  correlation = nlme::corAR1(form = ~ 1 | Mare)
)

test_that("model_info", {
  expect_true(model_info(m1)$is_linear)
})

test_that("find_predictors", {
  # `Mare` groups the correlation structure, it is no fixed predictor
  expect_identical(find_predictors(m1), list(conditional = "Time"))
  expect_identical(find_predictors(m1, flatten = TRUE), "Time")
  expect_identical(
    find_predictors(m1, effects = "all"),
    list(conditional = "Time", correlation = "Mare")
  )
  expect_null(find_predictors(m1, effects = "random"))
})

test_that("find_response", {
  expect_identical(find_response(m1), "follicles")
})

test_that("link_inverse", {
  expect_equal(link_inverse(m1)(0.2), 0.2, tolerance = 1e-5)
})

test_that("get_data", {
  expect_equal(nrow(get_data(m1)), 308)
  expect_equal(colnames(get_data(m1)), c("follicles", "Time", "Mare"))
})

test_that("get_df", {
  expect_equal(get_df(m1, type = "residual"), 305, ignore_attr = TRUE)
  expect_equal(get_df(m1, type = "normal"), Inf, ignore_attr = TRUE)
  expect_equal(get_df(m1, type = "wald"), 305, ignore_attr = TRUE)
  expect_equal(get_df(m3, type = "residual"), 304, ignore_attr = TRUE)
  expect_equal(get_df(m3, type = "normal"), Inf, ignore_attr = TRUE)
  expect_equal(get_df(m3, type = "wald"), 304, ignore_attr = TRUE)
})

test_that("find_formula", {
  expect_length(find_formula(m1), 2)
  expect_equal(
    find_formula(m1),
    list(
      conditional = as.formula("follicles ~ sin(2 * pi * Time) + cos(2 * pi * Time)"),
      correlation = as.formula("~1 | Mare")
    ),
    ignore_attr = TRUE
  )
  expect_equal(
    find_formula(m2),
    list(
      conditional = as.formula("follicles ~ sin(2 * pi * Time) + cos(2 * pi * Time)"),
      correlation = as.formula("~1 | Mare")
    ),
    ignore_attr = TRUE
  )
})

test_that("find_terms", {
  expect_identical(
    find_terms(m1),
    list(
      response = "follicles",
      conditional = c("sin(2 * pi * Time)", "cos(2 * pi * Time)"),
      correlation = "Mare"
    )
  )
  expect_identical(
    find_terms(m1, flatten = TRUE),
    c(
      "follicles",
      "sin(2 * pi * Time)",
      "cos(2 * pi * Time)",
      "Mare"
    )
  )
})

test_that("find_variables", {
  expect_equal(
    find_variables(m1),
    list(
      response = "follicles",
      conditional = "Time",
      correlation = "Mare"
    )
  )
  expect_equal(
    find_variables(m1, flatten = TRUE),
    c("follicles", "Time", "Mare")
  )
})

test_that("n_obs", {
  expect_equal(n_obs(m1), 308)
})

test_that("linkfun", {
  expect_false(is.null(link_function(m1)))
})

test_that("find_parameters", {
  expect_equal(
    find_parameters(m1),
    list(
      conditional = c("(Intercept)", "sin(2 * pi * Time)", "cos(2 * pi * Time)")
    )
  )
  expect_equal(nrow(get_parameters(m1)), 3)
  expect_equal(
    get_parameters(m1)$Parameter,
    c("(Intercept)", "sin(2 * pi * Time)", "cos(2 * pi * Time)")
  )
})

test_that("is_multivariate", {
  expect_false(is_multivariate(m1))
})

test_that("find_statistic", {
  expect_identical(find_statistic(m1), "t-statistic")
})


# correlation part, read from the fitted correlation structure ----------------

test_that("find_formula, gls, correlation structure without `form`", {
  # without `form`, the fitted formula is `~1`, which has no variable
  m <- nlme::gls(distance ~ age, data = nlme::Orthodont, correlation = nlme::corAR1())
  expect_named(find_formula(m), "conditional")
})

test_that("find_formula, gls, phylogenetic structure object, easystats/insight#189", {
  skip_if_not_installed("ape")
  set.seed(123)
  gls_tree <- ape::rtree(10)
  gls_phylo_d <- data.frame(
    y = rnorm(10),
    x = rnorm(10),
    row.names = gls_tree$tip.label
  )
  gls_corr_brownian <<- ape::corBrownian(phy = gls_tree)
  withr::defer(rm(gls_corr_brownian, envir = globalenv()))
  m <- nlme::gls(y ~ x, data = gls_phylo_d, correlation = gls_corr_brownian)
  # the structure has no `form`, so its fitted formula `~1` has no variable
  expect_no_error(find_formula(m))
  expect_named(find_formula(m), "conditional")
})


test_that("get_datagrid, gls, correlation grouping", {
  expect_named(get_datagrid(m1), "Time")
  expect_named(get_datagrid(m1, by = "Mare"), c("Mare", "Time"))
})
