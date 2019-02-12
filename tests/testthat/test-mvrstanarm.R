.runThisTest <- Sys.getenv("RunAllinsightTests") == "yes"

if (.runThisTest && Sys.getenv("USER") != "travis") {
  if (suppressWarnings(
    require("testthat") &&
      require("insight") &&
      require("rstanarm")
  )) {
    context("insight, mv-rstanarm")

    data("pbcLong")
    m1 <- stan_mvmer(
      formula = list(
        logBili ~ year + (1 | id),
        albumin ~ sex + year + (year | id)),
      data = pbcLong,
      chains = 1, cores = 1, seed = 12345, iter = 1000
    )

    test_that("clean_names", {
      expect_identical(clean_names(m1), c("logBili", "albumin", "year", "id", "sex"))
    })

    test_that("find_predictors", {
      expect_identical(
        find_predictors(m1),
        list(
          y1 = list(conditional = "year"),
          y2 = list(conditional = c("sex", "year"))
      ))
      expect_identical(find_predictors(m1, flatten = TRUE), c("year", "sex"))
      expect_identical(
        find_predictors(m1, effects = "all", component = "all"),
        list(
          y1 = list(conditional = "year", random = "id"),
          y2 = list(conditional = c("sex", "year"), random = "id")
        ))
      expect_identical(
        find_predictors(m1, effects = "all", component = "all", flatten = TRUE),
        c("year", "id", "sex")
      )
    })

    test_that("find_response", {
      expect_equal(find_response(m1, combine = TRUE), c(y1 = "logBili", y2 = "albumin"))
      expect_equal(find_response(m1, combine = FALSE), c(y1 = "logBili", y2 = "albumin"))
    })

    test_that("get_response", {
      expect_equal(nrow(get_response(m1)), 304)
      expect_equal(colnames(get_response(m1)), c("logBili", "albumin"))
    })

    test_that("find_terms", {
      expect_identical(
        find_terms(m1),
        list(
          response = c(y1 = "logBili", y2 = "albumin"),
          y1 = list(conditional = "year", random = "id"),
          y2 = list(conditional = c("sex", "year"), random = "id")
      ))
      expect_identical(
        find_terms(m1, flatten = TRUE),
        c("logBili", "albumin", "year", "id", "sex")
      )
      expect_identical(
        find_terms(m1, effects = "random"),
        list(
          response = c(y1 = "logBili", y2 = "albumin"),
          y1 = list(random = "id"),
          y2 = list(random = "id")
        ))
    })

    test_that("n_obs", {
      expect_equal(n_obs(m1), 304)
    })

    test_that("find_paramaters", {
      expect_equal(
        find_parameters(m1),
        list(
          y1 = list(
            conditional = c("(Intercept)", "year", "sigma"),
            random = sprintf("b[(Intercept) id:%i]", 1:40)
          ),
          y2 = list(
            conditional = c("(Intercept)", "sexf", "year", "sigma"),
            random = sprintf(c("b[(Intercept) id:%i]", "b[year id:%i]"), rep(1:40, each = 2))
          )
        )
      )
    })

    test_that("find_paramaters", {
      expect_equal(
        colnames(get_parameters(m1)),
        c("y1|(Intercept)", "y1|year", "y1|sigma", "y2|(Intercept)", "y2|sexf", "y2|year", "y2|sigma")
      )
      expect_equal(
        colnames(get_parameters(m1, effects = "all")),
        c("y1|(Intercept)", "y1|year", "y1|sigma", sprintf("b[y1|(Intercept) id:%i]", 1:40),
          "y2|(Intercept)", "y2|sexf", "y2|year", "y2|sigma",
          sprintf(c("b[y2|(Intercept) id:%i]", "b[y2|year id:%i]"), rep(1:40, each = 2)))
      )
    })

    test_that("linkfun", {
      expect_false(is.null(link_function(m1)))
      expect_length(link_function(m1), 2)
    })

    test_that("linkinv", {
      expect_false(is.null(link_inverse(m1)))
      expect_length(link_inverse(m1), 2)
    })


    test_that("is_multivariate", {
      expect_true(is_multivariate(m1))
    })

  }
}
