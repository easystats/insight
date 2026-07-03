skip_on_cran()
skip_if_not_installed("marginaleffects", minimum_version = "0.29.0")
skip_if_not_installed("emmeans")

test_that("marginaleffects", {
  m <- lm(Sepal.Width ~ Species * Petal.Length, data = iris)

  x <- marginaleffects::slopes(
    m,
    variables = "Petal.Length",
    newdata = insight::get_datagrid(m, by = "Species")
  )
  # Equivalent in emmeans
  x2 <- emmeans::emtrends(m, var = "Petal.Length", specs = ~ Species + Petal.Length)

  # Get parameters
  p1 <- insight::get_parameters(x)
  p2 <- insight::get_parameters(x2)

  expect_true("Estimate" %in% colnames(p1))
  expect_true("Species" %in% colnames(p1))
  expect_true("Petal.Length" %in% colnames(p1))
  expect_equal(p1$Estimate, p2$Estimate, tolerance = 0.001)

  # Find parameters
  expect_identical(insight::find_parameters(x)$marginaleffects, "Species")

  # Find statistic
  expect_identical(insight::find_statistic(x), "z-statistic")

  # standardize names - "s.value" becomes "S"
  skip_if_not_installed("parameters", minimum_version = "0.28.0.13")
  skip_if_not_installed("marginaleffects", minimum_version = "0.29.0")

  expect_named(
    parameters::model_parameters(x),
    c(
      "rowid",
      "Parameter",
      "Comparison",
      "Coefficient",
      "SE",
      "Statistic",
      "p",
      "S",
      "CI",
      "CI_low",
      "CI_high",
      "Species",
      "Petal.Length",
      "Predicted"
    )
  )

  expect_equal(n_obs(x), 150) # nrow(iris)
})

test_that("marginaleffects, find_response", {
  data(mtcars)
  tmp <- mtcars
  tmp$am <- as.logical(tmp$am)
  mod <- lm(mpg ~ am + factor(cyl), tmp)

  mod_comp <- suppressWarnings(marginaleffects::avg_comparisons(
    mod,
    variables = list(cyl = "reference")
  ))
  expect_identical(find_response(mod_comp), "mpg")
  mod_comp <- marginaleffects::avg_predictions(mod, variables = "cyl")
  expect_identical(find_response(mod_comp), "mpg")
  mod_comp <- marginaleffects::avg_slopes(mod, variables = "am")
  expect_identical(find_response(mod_comp), "mpg")

  skip_if_not_installed("lme4")
  data("cbpp", package = "lme4")
  m <- glm(cbind(incidence, size - incidence) ~ herd, data = cbpp, family = binomial)
  mod_comp <- marginaleffects::avg_predictions(m, variables = "herd")
  expect_identical(find_response(mod_comp, combine = FALSE), c("incidence", "size"))
  expect_identical(
    find_response(mod_comp, combine = TRUE),
    "cbind(incidence, size - incidence)"
  )
})

test_that("marginaleffects, find_predictors", {
  data(mtcars)
  tmp <- mtcars
  tmp$am <- as.logical(tmp$am)
  mod <- lm(mpg ~ am + factor(cyl), tmp)

  mod_comp <- suppressWarnings(marginaleffects::avg_comparisons(
    mod,
    variables = list(cyl = "reference")
  ))
  expect_identical(find_predictors(mod_comp), c("am", "cyl"))
  mod_comp <- marginaleffects::avg_predictions(mod, variables = "cyl")
  expect_identical(find_predictors(mod_comp), c("am", "cyl"))
  mod_comp <- marginaleffects::avg_slopes(mod, variables = "am")
  expect_identical(find_predictors(mod_comp), c("am", "cyl"))
})
