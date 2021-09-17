if (requiet("testthat") && requiet("insight") && requiet("afex")) {
  data(obk.long, package = "afex")

  obk.long$treatment <- as.character(obk.long$treatment)
  obk.long$phase <- as.character(obk.long$phase)

  Mc <- afex::aov_car(value ~ treatment * gender + age + Error(id / (phase * hour)),
    factorize = FALSE,
    data = obk.long, include_aov = FALSE
  )

  Mc2 <- afex::aov_car(value ~ treatment * gender + exp(age) + Error(id / (phase * hour)),
    factorize = FALSE,
    data = obk.long, include_aov = FALSE
  )

  M <- afex::aov_car(value ~ treatment * gender + Error(id / (phase * hour)),
    data = obk.long, include_aov = FALSE
  )

  B <- afex::aov_car(value ~ treatment * gender + Error(id),
    data = obk.long, include_aov = FALSE
  )

  W <- afex::aov_car(value ~ Error(id / (phase * hour)),
    data = obk.long, include_aov = FALSE
  )

  mods <- list(Mc, Mc2, M, B, W)


  test_that("afex_aov: afex", {
    expect_equal(unique(unlist(sapply(mods, model_name))), "afex_aov")
    expect_equal(unique(unlist(sapply(mods, find_algorithm))), "OLS")
    expect_equal(unique(unlist(sapply(mods, find_statistic))), "F-statistic")

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
    # expect_false(unique(sapply(mods, is_multivariate)))
    expect_false(unique(sapply(mods, is_nullmodel)))

    # expect_equal(get_family(Mc2), gaussian())
    expect_equal(link_function(Mc2), gaussian()$linkfun)
    expect_equal(link_inverse(Mc2), gaussian()$linkinv)
  })

  test_that("afex_aov: model values", {
    expect_equal(suppressWarnings(sapply(mods, get_auxiliary)),
      c(1.746, 1.768, 1.764, 1.233, 2.075),
      tolerance = 0.01
    )
    expect_equal(suppressWarnings(sapply(mods, get_df)),
      c(135, 135, 150, 10, 225),
      tolerance = 0.01
    )
    expect_equal(sapply(mods, get_loglikelihood),
      c(-411.04, -414.088, -431.688, -22.295, -517.397),
      tolerance = 0.01
    )
    expect_equal(suppressWarnings(sapply(mods, get_sigma)),
      c(1.746, 1.768, 1.764, 1.233, 2.075),
      tolerance = 0.01
    )
    expect_equal(sapply(mods, n_obs),
      c(240, 240, 240, 16, 240),
      tolerance = 0.01
    )
    expect_equal(sapply(mods, n_parameters),
      c(105, 105, 90, 6, 15),
      tolerance = 0.01
    )
    expect_equal(sapply(mods, is_mixed_model),
      c(TRUE, TRUE, TRUE, FALSE, TRUE),
      tolerance = 0.01
    )
    expect_equal(sapply(mods, get_deviance),
      c(411.603, 422.17, 467, 15.204, 969.125),
      tolerance = 0.01
    )
  })

  test_that("afex_aov: formula and parameters", {
    # find_formula
    expect_equal(
      find_terms(Mc2),
      list(
        response = "value",
        conditional = c("phase", "hour", "treatment", "gender", "exp(age)"),
        error = "Error(id/(phase * hour))"
      )
    )
    expect_equal(length(find_interactions(Mc2)$conditional), 14L)
    expect_equal(
      find_variables(Mc2),
      list(
        response = "value",
        fixed = c("treatment", "gender", "age", "phase", "hour"),
        random = "id"
      )
    )
    expect_equal(
      find_predictors(Mc2, effects = "all"),
      list(
        fixed = c("treatment", "gender", "age", "phase", "hour"),
        random = "id"
      )
    )
    expect_equal(
      find_random(Mc2),
      list(random = "id")
    )
    expect_equal(find_response(Mc2), "value")
  })


  test_that("afex_aov: formula and parameters", {
    expect_equal(dim(get_data(Mc2)), c(240, 7))
    expect_equal(dim(get_statistic(Mc2)), c(19, 2))

    expect_equal(dim(get_modelmatrix(Mc2)), c(16, 7))
    expect_equal(length(find_parameters(Mc2)), 15L)
    expect_equal(length(get_intercept(Mc2)), 15L)
    expect_equal(dim(get_parameters(Mc2)), c(15 * 7, 3))
    expect_equal(dim(get_varcov(Mc2)), c(15 * 7, 15 * 7))

    expect_equal(length(get_predicted(Mc2)), n_obs(Mc2))
    expect_equal(length(get_residuals(Mc2)), n_obs(Mc2))
  })
}
