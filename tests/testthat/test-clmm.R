osx <- tryCatch(
  {
    si <- Sys.info()
    if (!is.null(si["sysname"])) {
      si["sysname"] == "Darwin" || grepl("^darwin", R.version$os)
    } else {
      FALSE
    }
  },
  error = function(e) {
    FALSE
  }
)

if (requiet("testthat") &&
  requiet("insight") &&
  requiet("ordinal")) {
  data(wine, package = "ordinal")
  data(soup)

  m1 <- clmm(rating ~ temp + contact + (1 | judge), data = wine)
  m2 <- clmm(SURENESS ~ PROD + (1 | RESP) + (1 | RESP:PROD),
    data = soup,
    link = "probit",
    threshold = "equidistant"
  )

  test_that("model_info", {
    expect_true(model_info(m1)$is_ordinal)
    expect_true(model_info(m2)$is_ordinal)
    expect_true(model_info(m1)$is_logit)
    expect_true(model_info(m2)$is_probit)
    expect_false(model_info(m1)$is_multinomial)
    expect_false(model_info(m1)$is_linear)
  })

  test_that("find_predictors", {
    expect_identical(find_predictors(m1), list(conditional = c("temp", "contact")))
    expect_identical(
      find_predictors(m1, effects = "all"),
      list(
        conditional = c("temp", "contact"),
        random = "judge"
      )
    )
    expect_identical(
      find_predictors(m1, effects = "all", flatten = TRUE),
      c("temp", "contact", "judge")
    )
    expect_identical(find_predictors(m2), list(conditional = "PROD"))
    expect_identical(
      find_predictors(m2, effects = "all"),
      list(
        conditional = "PROD",
        random = c("RESP", "PROD")
      )
    )
    expect_identical(
      find_predictors(m2, effects = "all", flatten = TRUE),
      c("PROD", "RESP")
    )
  })

  test_that("find_random", {
    expect_equal(find_random(m1), list(random = "judge"))
    expect_equal(find_random(m2), list(random = c("RESP", "RESP:PROD")))
    expect_equal(find_random(m2, split_nested = TRUE), list(random = c("RESP", "PROD")))
  })

  test_that("get_random", {
    expect_equal(get_random(m1), wine[, "judge", drop = FALSE])
    expect_equal(get_random(m2), soup[, c("RESP", "PROD"), drop = FALSE])
  })

  test_that("find_response", {
    expect_identical(find_response(m1), "rating")
    expect_identical(find_response(m2), "SURENESS")
  })

  test_that("get_response", {
    expect_equal(get_response(m1), wine$rating)
    expect_equal(get_response(m2), soup$SURENESS)
  })

  test_that("get_predictors", {
    expect_equal(colnames(get_predictors(m1)), c("temp", "contact"))
    expect_equal(colnames(get_predictors(m2)), "PROD")
  })

  test_that("link_inverse", {
    expect_equal(link_inverse(m1)(.2), plogis(.2), tolerance = 1e-5)
    expect_equal(link_inverse(m2)(.2), pnorm(.2), tolerance = 1e-5)
  })

  test_that("get_data", {
    expect_equal(nrow(get_data(m1)), 72)
    expect_equal(
      colnames(get_data(m1)),
      c("rating", "temp", "contact", "judge")
    )
    expect_equal(nrow(get_data(m2)), 1847)
    expect_equal(colnames(get_data(m2)), c("SURENESS", "PROD", "RESP"))
  })

  test_that("find_formula", {
    expect_length(find_formula(m1), 2)
    expect_equal(
      find_formula(m1),
      list(
        conditional = as.formula("rating ~ temp + contact"),
        random = as.formula("~1 | judge")
      ),
      ignore_attr = TRUE
    )
    expect_length(find_formula(m2), 2)
    expect_equal(
      find_formula(m2),
      list(
        conditional = as.formula("SURENESS ~ PROD"),
        random = list(as.formula("~1 | RESP"), as.formula("~1 | RESP:PROD"))
      ),
      ignore_attr = TRUE
    )
  })

  test_that("find_terms", {
    expect_equal(
      find_terms(m1),
      list(
        response = "rating",
        conditional = c("temp", "contact"),
        random = "judge"
      )
    )
    expect_equal(
      find_terms(m1, flatten = TRUE),
      c("rating", "temp", "contact", "judge")
    )
    expect_equal(
      find_terms(m2),
      list(
        response = "SURENESS",
        conditional = "PROD",
        random = c("RESP", "PROD")
      )
    )
    expect_equal(
      find_terms(m2, flatten = TRUE),
      c("SURENESS", "PROD", "RESP")
    )
  })

  test_that("n_obs", {
    expect_equal(n_obs(m1), 72)
    expect_equal(n_obs(m2), 1847)
  })

  test_that("linkfun", {
    expect_false(is.null(link_function(m1)))
    expect_false(is.null(link_function(m2)))
  })

  test_that("find_parameters", {
    expect_equal(
      find_parameters(m1),
      list(
        conditional = c("1|2", "2|3", "3|4", "4|5", "tempwarm", "contactyes")
      )
    )
    expect_equal(
      find_parameters(m2),
      list(conditional = c("threshold.1", "spacing", "PRODTest"))
    )
  })

  test_that("is_multivariate", {
    expect_false(is_multivariate(m1))
    expect_false(is_multivariate(m2))
  })

  if (getRversion() > "3.6.3" && !isTRUE(osx)) {
    test_that("get_variance", {
      expect_equal(
        get_variance(m1),
        list(
          var.fixed = 3.23207765938872,
          var.random = 1.27946088209319,
          var.residual = 3.28986813369645,
          var.distribution = 3.28986813369645,
          var.dispersion = 0,
          var.intercept = c(judge = 1.27946088209319)
        ),
        tolerance = 1e-4
      )
      expect_equal(
        get_variance(m2),
        list(
          var.fixed = 0.132313576370902,
          var.random = 0.193186321588604,
          var.residual = 1,
          var.distribution = 1,
          var.dispersion = 0,
          var.intercept = c(`RESP:PROD` = 0.148265480396059, RESP = 0.0449208411925493)
        ),
        tolerance = 1e-4
      )
    })
  }

  test_that("find_statistic", {
    expect_identical(find_statistic(m1), "z-statistic")
    expect_identical(find_statistic(m2), "z-statistic")
  })
}
