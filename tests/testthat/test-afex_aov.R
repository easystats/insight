skip_if_not_installed("afex")

data(obk.long, package = "afex")

obk.long$treatment <- as.character(obk.long$treatment)
obk.long$phase <- as.character(obk.long$phase)

Mc <- suppressWarnings(suppressMessages(
  afex::aov_car(
    value ~ treatment * gender + age + Error(id / (phase * hour)),
    factorize = FALSE,
    data = obk.long, include_aov = FALSE
  )
))

Mc2 <- suppressWarnings(suppressMessages(
  afex::aov_car(
    value ~ treatment * gender + exp(age) + Error(id / (phase * hour)),
    factorize = FALSE,
    data = obk.long, include_aov = FALSE
  )
))

M <- suppressWarnings(suppressMessages(
  afex::aov_car(
    value ~ treatment * gender + Error(id / (phase * hour)),
    data = obk.long, include_aov = FALSE
  )
))

B <- suppressWarnings(suppressMessages(
  afex::aov_car(
    value ~ treatment * gender + Error(id),
    data = obk.long, include_aov = FALSE
  )
))

W <- suppressWarnings(suppressMessages(
  afex::aov_car(
    value ~ Error(id / (phase * hour)),
    data = obk.long, include_aov = FALSE
  )
))

mods <- list(Mc, Mc2, M, B, W)


test_that("afex_aov: afex", {
  # see https://github.com/georgheinze/logistf/pull/54
  skip_if(
    "as.character.formula" %in% methods(as.character),
    "Some package uses `formula.tools::as.character.formula()` which breaks `find_formula()`."
  )

  expect_identical(unique(unlist(sapply(mods, model_name))), "afex_aov")
  expect_identical(unique(unlist(sapply(mods, find_algorithm))), "OLS")
  expect_identical(unique(unlist(sapply(mods, find_statistic))), "F-statistic")

  expect_null(unique(unlist(sapply(mods, find_offset))))
  expect_null(unique(unlist(sapply(mods, find_random_slopes))))
  expect_null(unique(unlist(sapply(mods, find_smooth))))
  expect_null(unique(unlist(sapply(mods, find_weights))))
  expect_null(unique(unlist(sapply(mods, get_call))))
  expect_null(unique(unlist(sapply(mods, get_weights))))
  expect_null(unique(unlist(suppressWarnings(sapply(mods, get_variance)))))

  expect_true(unique(sapply(mods, all_models_equal)))
  expect_true(unique(sapply(mods, has_intercept)))
  expect_true(unique(sapply(mods, is_model)))
  expect_true(unique(sapply(mods, is_model_supported)))
  expect_false(unique(sapply(mods, is_gam_model)))
  expect_false(unique(sapply(mods, is_nullmodel)))

  expect_equal(link_function(Mc2), gaussian()$linkfun, ignore_attr = TRUE)
  expect_equal(link_inverse(Mc2), gaussian()$linkinv, ignore_attr = TRUE)
})

test_that("afex_aov: model values", {
  expect_equal(
    suppressWarnings(sapply(mods, get_auxiliary)),
    c(1.75262, 1.77497, 1.77038, 1.29973, 2.08001),
    tolerance = 0.01
  )
  expect_equal(
    suppressWarnings(sapply(mods, get_df)),
    c(134, 134, 149, 9, 224),
    tolerance = 0.01
  )
  expect_equal(
    sapply(mods, get_loglikelihood),
    c(-405.34037, -408.37849, -420.51277, -22.29462, -508.05184),
    tolerance = 0.01
  )
  expect_equal(
    suppressWarnings(sapply(mods, get_sigma)),
    c(1.75262, 1.77497, 1.77038, 1.29973, 2.08001),
    tolerance = 0.01
  )
  expect_equal(
    sapply(mods, n_obs),
    c(240, 240, 240, 16, 240),
    tolerance = 0.01
  )
  expect_equal(
    sapply(mods, n_parameters),
    c(105, 105, 90, 6, 15),
    tolerance = 0.01
  )
  expect_equal(
    sapply(mods, is_mixed_model),
    c(TRUE, TRUE, TRUE, FALSE, TRUE),
    tolerance = 0.01
  )
  expect_equal(
    sapply(mods, get_deviance),
    c(411.603, 422.17, 467, 15.204, 969.125),
    tolerance = 0.01
  )
})

test_that("afex_aov: formula and parameters", {
  # see https://github.com/georgheinze/logistf/pull/54
  skip_if(
    "as.character.formula" %in% methods(as.character),
    "Some package uses `formula.tools::as.character.formula()` which breaks `find_formula()`."
  )

  # find_formula
  expect_identical(
    find_terms(Mc2),
    list(
      response = "value",
      conditional = c("phase", "hour", "treatment", "gender", "exp(age)"),
      error = "Error(id/(phase * hour))"
    )
  )
  expect_length(find_interactions(Mc2)$conditional, 14)
  expect_identical(
    find_variables(Mc2),
    list(
      response = "value",
      fixed = c("treatment", "gender", "age", "phase", "hour"),
      random = "id"
    )
  )
  expect_identical(
    find_predictors(Mc2, effects = "all"),
    list(
      fixed = c("treatment", "gender", "age", "phase", "hour"),
      random = "id"
    )
  )
  expect_identical(
    find_random(Mc2),
    list(random = "id")
  )
  expect_identical(find_response(Mc2), "value")
})


test_that("afex_aov: formula and parameters", {
  expect_identical(dim(get_data(Mc2)), c(240L, 7L))
  expect_identical(dim(get_statistic(Mc2)), c(19L, 2L))

  expect_identical(dim(get_modelmatrix(Mc2)), c(16L, 7L))
  expect_length(find_parameters(Mc2), 15)
  expect_length(get_intercept(Mc2), 15)
  expect_identical(dim(get_parameters(Mc2)), as.integer(c(15 * 7, 3)))
  expect_identical(dim(get_varcov(Mc2)), as.integer(c(15 * 7, 15 * 7)))

  expect_length(get_predicted(Mc2), n_obs(Mc2))
  expect_length(get_residuals(Mc2), n_obs(Mc2))
})
