skip_on_cran()
skip_on_os(c("mac", "linux", "solaris"))

skip_if_not_installed("svylme")
skip_if_not_installed("lme4")
skip_if_not_installed("survey")
skip_if_not_installed("withr")

withr::with_environment(
  new.env(),
  {
    data(api, package = "survey")
    # two-stage cluster sample
    dclus2 <- survey::svydesign(
      id = ~ dnum + snum,
      fpc = ~ fpc1 + fpc2,
      data = apiclus2
    )
    m1 <- svylme::svy2lme(
      api00 ~ ell + mobility + api99 + (1 + api99 | dnum),
      design = dclus2,
      method = "nested"
    )

    test_that("model_info", {
      expect_true(model_info(m1)$is_linear)
      expect_true(model_info(m1)$is_linear)
    })

    test_that("find_predictors", {
      expect_identical(find_predictors(m1), list(conditional = c("ell", "mobility", "api99")))
      expect_identical(
        find_predictors(m1, effects = "random"),
        list(random = "dnum")
      )
    })

    test_that("find_response", {
      expect_identical(find_response(m1), "api00")
    })

    test_that("link_inverse", {
      expect_equal(link_inverse(m1)(0.2), 0.2, tolerance = 1e-5)
    })

    test_that("get_data", {
      expect_warning(expect_warning(expect_null(get_data(m1))))
    })

    test_that("find_formula", {
      expect_length(find_formula(m1), 2)
      expect_equal(
        find_formula(m1),
        list(
          conditional = api00 ~ ell + mobility + api99,
          random = ~ 1 + api99 | dnum
        ),
        ignore_attr = TRUE
      )
    })

    test_that("find_terms", {
      expect_identical(
        find_terms(m1),
        list(
          response = "api00",
          conditional = c("ell", "mobility", "api99"),
          random = c("api99", "dnum")
        )
      )
      expect_identical(
        find_terms(m1, flatten = TRUE),
        c("api00", "ell", "mobility", "api99", "dnum")
      )
    })

    test_that("find_variables", {
      expect_identical(
        find_variables(m1),
        list(
          response = "api00",
          conditional = c("ell", "mobility", "api99"),
          random = "dnum"
        )
      )
      expect_identical(
        find_variables(m1, flatten = TRUE),
        c("api00", "ell", "mobility", "api99", "dnum")
      )
    })

    test_that("get_response", {
      expect_null(get_response(m1))
    })

    test_that("linkfun", {
      expect_false(is.null(link_function(m1)))
    })

    test_that("is_multivariate", {
      expect_false(is_multivariate(m1))
    })

    test_that("find_parameters", {
      expect_identical(
        find_parameters(m1),
        list(
          conditional = c("(Intercept)", "ell", "mobility", "api99"),
          random = list(dnum1 = "(Intercept)", dnum2 = "api99")
        )
      )
      expect_identical(nrow(get_parameters(m1)), 4L)
      expect_identical(
        get_parameters(m1)$Parameter,
        c("(Intercept)", "ell", "mobility", "api99")
      )
      expect_equal(
        get_parameters(m1)$Estimate,
        c(-60.97707, 0.91716, -0.38037, 1.09788),
        tolerance = 1e-4
      )
    })

    test_that("find_statistic", {
      expect_identical(find_statistic(m1), "t-statistic")
    })
  }
)
