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

test_that("marginaleffects, get_data", {
  data(mtcars)
  tmp <- mtcars
  tmp$cyl <- as.factor(tmp$cyl)
  attr(tmp$mpg, "label") <- "Miles per gallon"
  attr(tmp$cyl, "label") <- "Number of cylinders"
  attr(tmp$disp, "label") <- "Displacement"

  mod <- lm(mpg ~ cyl + disp, tmp)
  mod_ame <- marginaleffects::avg_comparisons(mod, variables = list(cyl = "reference"))
  out <- get_data(mod)
  expect_named(out, c("mpg", "cyl", "disp"))
  expect_identical(attributes(out$cyl)$label, "Number of cylinders")

  mod <- marginaleffects::avg_predictions(mod, variables = "cyl")
  out <- get_data(mod)
  expect_named(out, c("mpg", "cyl", "disp"))
  expect_identical(attributes(out$cyl)$label, "Number of cylinders")
})

test_that("marginaleffects, comparisons, find_parameters", {
  data(mtcars)
  tmp <- mtcars
  tmp$cyl <- as.factor(tmp$cyl)
  tmp$am <- as.logical(tmp$am)

  attr(tmp$mpg, "label") <- "Miles per gallon"
  attr(tmp$cyl, "label") <- "Number of cylinders"
  attr(tmp$disp, "label") <- "Displacement"
  attr(tmp$am, "label") <- "Transmission (manual)"

  mod <- lm(mpg ~ cyl * am + disp, tmp)
  mod_ame <- marginaleffects::avg_comparisons(mod, variables = c("cyl", "am"))
  expect_identical(find_parameters(mod_ame), c("am", "cyl"))

  mod_ame <- marginaleffects::avg_comparisons(mod, variables = "cyl", by = "am")
  expect_identical(find_parameters(mod_ame), "cyl")

  mod_ame <- marginaleffects::avg_comparisons(mod, variables = "am", by = "cyl")
  expect_identical(find_parameters(mod_ame), "am")

  mod_ame <- marginaleffects::avg_comparisons(mod, by = c("cyl", "am"))
  expect_identical(find_parameters(mod_ame), c("am", "cyl", "disp"))
})

test_that("marginaleffects, predictions, find_parameters", {
  data(mtcars)
  tmp <- mtcars
  tmp$cyl <- as.factor(tmp$cyl)
  tmp$am <- as.logical(tmp$am)

  attr(tmp$mpg, "label") <- "Miles per gallon"
  attr(tmp$cyl, "label") <- "Number of cylinders"
  attr(tmp$disp, "label") <- "Displacement"
  attr(tmp$am, "label") <- "Transmission (manual)"

  mod <- lm(mpg ~ cyl * am + disp, tmp)
  mod_ame <- marginaleffects::avg_predictions(mod, variables = c("cyl", "am"))
  expect_identical(find_parameters(mod_ame), c("am", "cyl"))

  mod_ame <- marginaleffects::avg_predictions(mod, variables = "cyl", by = "am")
  expect_identical(find_parameters(mod_ame), c("cyl", "am"))

  mod_ame <- marginaleffects::avg_predictions(mod, variables = "am", by = "cyl")
  expect_identical(find_parameters(mod_ame), c("am", "cyl"))

  mod_ame <- marginaleffects::avg_predictions(mod, by = c("cyl", "am"))
  expect_identical(find_parameters(mod_ame), c("cyl", "am"))
})
