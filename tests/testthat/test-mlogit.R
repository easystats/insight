if (

  requiet("mlogit") &&
    requiet("mclogit")) {
  data("Fishing")
  Fish <-
    mlogit.data(Fishing,
      varying = c(2:9),
      shape = "wide",
      choice = "mode"
    )

  m1 <- mlogit(mode ~ price + catch, data = Fish)
  m2 <- mlogit(mode ~ price + catch | income, data = Fish)

  test_that("model_info", {
    expect_false(model_info(m1)$is_ordinal)
    expect_false(model_info(m2)$is_ordinal)
    expect_true(model_info(m1)$is_multinomial)
    expect_true(model_info(m2)$is_multinomial)
    expect_false(model_info(m1)$is_linear)
  })

  test_that("find_predictors", {
    expect_identical(find_predictors(m1), list(conditional = c("price", "catch")))
    expect_identical(find_predictors(m1, flatten = TRUE), c("price", "catch"))
    expect_null(find_predictors(m1, effects = "random"))
    expect_identical(find_predictors(m2), list(conditional = c("price", "catch", "income")))
    expect_identical(
      find_predictors(m2, flatten = TRUE),
      c("price", "catch", "income")
    )
    expect_null(find_predictors(m2, effects = "random"))
  })

  test_that("find_response", {
    expect_identical(find_response(m1), "mode")
    expect_identical(find_response(m2), "mode")
  })

  if (getRversion() >= "3.6.0") {
    test_that("get_response", {
      expect_equal(get_response(m1), as.vector(Fish$mode))
    })

    test_that("get_data", {
      expect_equal(nrow(get_data(m1, verbose = FALSE)), 4728)
      expect_equal(nrow(get_data(m2, verbose = FALSE)), 4728)

      if (packageVersion("mlogit") <= "1.0-3.1") {
        expect_equal(
          colnames(get_data(m1, verbose = FALSE)),
          c("mode", "price", "catch", "probabilities", "linpred")
        )
        expect_equal(
          colnames(get_data(m2, verbose = FALSE)),
          c(
            "mode",
            "price",
            "catch",
            "income",
            "probabilities",
            "linpred"
          )
        )
      } else {
        expect_equal(
          colnames(get_data(m1, verbose = FALSE)),
          c("mode", "price", "catch", "idx", "probabilities", "linpred")
        )
        expect_equal(
          colnames(get_data(m2, verbose = FALSE)),
          c(
            "mode",
            "price",
            "catch",
            "income",
            "idx",
            "probabilities",
            "linpred"
          )
        )
      }
    })
  }

  test_that("link_inverse", {
    expect_equal(link_inverse(m1)(0.2), plogis(0.2), tolerance = 1e-5)
    expect_equal(link_inverse(m2)(0.2), plogis(0.2), tolerance = 1e-5)
  })


  test_that("find_formula", {
    expect_length(find_formula(m1), 1)
    expect_length(find_formula(m2), 1)
  })

  test_that("find_terms", {
    expect_equal(find_terms(m1), list(
      response = "mode",
      conditional = c("price", "catch")
    ))
    expect_equal(find_terms(m1, flatten = TRUE), c("mode", "price", "catch"))
    expect_equal(find_terms(m2), list(
      response = "mode",
      conditional = c("price", "catch", "income")
    ))
    expect_equal(
      find_terms(m2, flatten = TRUE),
      c("mode", "price", "catch", "income")
    )
  })

  test_that("n_obs", {
    expect_equal(n_obs(m1), 4728)
    expect_equal(n_obs(m2), 4728)
  })

  test_that("linkfun", {
    expect_false(is.null(link_function(m1)))
    expect_false(is.null(link_function(m2)))
  })

  test_that("find_statistic", {
    expect_identical(find_statistic(m1), "z-statistic")
    expect_identical(find_statistic(m2), "z-statistic")
  })
}


test_that("mblogit and mclogit is not linear", {
  requiet("mclogit")

  if (packageVersion("mclogit") >= "0.9.1") {
    data(Transport)
    mod <- mblogit(factor(gear) ~ mpg + hp, data = mtcars, trace = FALSE)
    expect_false(model_info(mod)$is_linear)
    expect_true(model_info(mod)$is_logit)
    expect_true(is_model(mod))
    expect_true(is_model_supported(mod))

    mod <- mclogit(resp | suburb ~ distance + cost, data = Transport, trace = FALSE)
    expect_false(model_info(mod)$is_linear)
    expect_true(model_info(mod)$is_logit)
    expect_true(is_model(mod))
    expect_true(is_model_supported(mod))
  }
})
