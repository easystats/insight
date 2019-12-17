if (require("testthat") &&
  require("insight") &&
  require("glmmTMB")) {
  context("insight, glmmTMB")

  # fish <- read.csv("https://stats.idre.ucla.edu/stat/data/fish.csv")
  # fish$nofish <- as.factor(fish$nofish)
  # fish$livebait <- as.factor(fish$livebait)
  # fish$camper <- as.factor(fish$camper)

  m1 <- download_model("glmmTMB_zi_1")
  m2 <- download_model("glmmTMB_1")
  m3 <- download_model("glmmTMB_zi_2")
  m4 <- download_model("glmmTMB_zi_5")
  m7 <- download_model("glmmTMB_zi_6")

  fish <- get_data(m7)

  data(Salamanders)
  m5 <- glmmTMB(
    count ~ mined + (1 | site),
    ziformula = ~mined,
    family = poisson,
    data = Salamanders
  )

  m6 <-
    glmmTMB(count ~ 1,
      ziformula = ~1,
      family = poisson(),
      data = Salamanders
    )

  test_that("find_weights", {
    expect_null(find_weights(m2))
  })

  test_that("get_weights", {
    expect_null(get_weights(m2))
  })

  test_that("model_info", {
    expect_true(model_info(m1)$is_zero_inflated)
    expect_false(model_info(m2)$is_zero_inflated)
    expect_true(model_info(m3)$is_count)
    expect_true(model_info(m3)$is_pois)
    expect_false(model_info(m3)$is_negbin)
    expect_true(model_info(m6)$is_count)
  })

  test_that("clean_names", {
    expect_identical(clean_names(m1), c("count", "child", "camper", "persons"))
    expect_identical(clean_names(m2), c("count", "child", "camper", "persons"))
    expect_identical(
      clean_names(m3),
      c("count", "child", "camper", "persons", "livebait")
    )
    expect_identical(
      clean_names(m4),
      c(
        "count",
        "child",
        "camper",
        "persons",
        "livebait",
        "ID",
        "xb"
      )
    )
    expect_identical(clean_names(m6), c("count"))
  })

  test_that("find_predictors", {
    expect_identical(
      find_predictors(m1, effects = "all"),
      list(
        conditional = c("child", "camper"),
        random = "persons",
        zero_inflated = c("child", "camper"),
        zero_inflated_random = "persons"
      )
    )
    expect_identical(
      find_predictors(m1, effects = "all", flatten = TRUE),
      c("child", "camper", "persons")
    )
    expect_identical(
      find_predictors(m1, effects = "random"),
      list(random = "persons", zero_inflated_random = "persons")
    )
    expect_identical(
      find_predictors(m1, effects = "random", flatten = TRUE),
      "persons"
    )
    expect_identical(
      find_predictors(m1, effects = "random", component = "conditional"),
      list(random = "persons")
    )
    expect_identical(
      find_predictors(
        m1,
        effects = "random",
        component = "conditional",
        flatten = TRUE
      ),
      "persons"
    )
    expect_identical(
      find_predictors(m1),
      list(
        conditional = c("child", "camper"),
        zero_inflated = c("child", "camper")
      )
    )
    expect_identical(find_predictors(m1, flatten = TRUE), c("child", "camper"))

    expect_identical(
      find_predictors(m2, effects = "all"),
      list(
        conditional = c("child", "camper"),
        random = "persons"
      )
    )
    expect_identical(
      find_predictors(m2, effects = "all", flatten = TRUE),
      c("child", "camper", "persons")
    )
    expect_identical(
      find_predictors(m2, effects = "random"),
      list(random = "persons")
    )
    expect_identical(
      find_predictors(m2, effects = "random", flatten = TRUE),
      "persons"
    )
    expect_identical(find_predictors(m2), list(conditional = c("child", "camper")))

    expect_null(find_predictors(m6))
  })

  test_that("find_response", {
    expect_identical(find_response(m1), "count")
    expect_identical(find_response(m2), "count")
    expect_identical(find_response(m6), "count")
  })

  test_that("link_inverse", {
    expect_identical(link_inverse(m1)(.2), exp(.2))
    expect_identical(link_inverse(m2)(.2), exp(.2))
  })

  test_that("get_data", {
    expect_equal(
      colnames(get_data(m1)),
      c("count", "child", "camper", "persons")
    )
    expect_equal(
      colnames(get_data(m1, effects = "all")),
      c("count", "child", "camper", "persons")
    )
    expect_equal(colnames(get_data(m1, effects = "random")), "persons")
    expect_equal(
      colnames(get_data(m2)),
      c("count", "child", "camper", "persons")
    )
    expect_equal(
      colnames(get_data(m2, effects = "all")),
      c("count", "child", "camper", "persons")
    )
    expect_equal(colnames(get_data(m2, effects = "random")), "persons")
    get_data(m3)
    expect_equal(colnames(get_data(m6)), "count")
    expect_null(get_data(m6, effects = "random"))
  })

  test_that("find_predictors", {
    expect_identical(
      find_predictors(m3, effects = "fixed", component = "conditional"),
      list(conditional = c("child", "camper"))
    )
    expect_identical(
      find_predictors(
        m3,
        effects = "fixed",
        component = "conditional",
        flatten = TRUE
      ),
      c("child", "camper")
    )
    expect_identical(
      find_predictors(m3, effects = "fixed", component = "zero_inflated"),
      list(zero_inflated = c("child", "livebait"))
    )
    expect_identical(
      find_predictors(
        m3,
        effects = "fixed",
        component = "zero_inflated",
        flatten = TRUE
      ),
      c("child", "livebait")
    )
    expect_identical(
      find_predictors(m3, effects = "all", component = "conditional"),
      list(
        conditional = c("child", "camper"),
        random = "persons"
      )
    )
    expect_identical(
      find_predictors(
        m3,
        effects = "all",
        component = "conditional",
        flatten = TRUE
      ),
      c("child", "camper", "persons")
    )
    expect_identical(
      find_predictors(m3, effects = "all", component = "zero_inflated"),
      list(
        zero_inflated = c("child", "livebait"),
        zero_inflated_random = "persons"
      )
    )
    expect_identical(
      find_predictors(
        m3,
        effects = "all",
        component = "zero_inflated",
        flatten = TRUE
      ),
      c("child", "livebait", "persons")
    )
    expect_identical(
      find_predictors(m3, effects = "random", component = "conditional"),
      list(random = "persons")
    )
    expect_identical(
      find_predictors(
        m3,
        effects = "random",
        component = "conditional",
        flatten = TRUE
      ),
      "persons"
    )
    expect_identical(
      find_predictors(m3, effects = "random", component = "zero_inflated"),
      list(zero_inflated_random = "persons")
    )
    expect_identical(
      find_predictors(
        m3,
        effects = "random",
        component = "zero_inflated",
        flatten = TRUE
      ),
      "persons"
    )

    expect_identical(
      find_predictors(m3, effects = "fixed", component = "all"),
      list(
        conditional = c("child", "camper"),
        zero_inflated = c("child", "livebait")
      )
    )
    expect_identical(
      find_predictors(
        m3,
        effects = "fixed",
        component = "all",
        flatten = TRUE
      ),
      c("child", "camper", "livebait")
    )
    expect_identical(
      find_predictors(m3, effects = "all", component = "all"),
      list(
        conditional = c("child", "camper"),
        random = "persons",
        zero_inflated = c("child", "livebait"),
        zero_inflated_random = "persons"
      )
    )
    expect_identical(
      find_predictors(
        m3,
        effects = "all",
        component = "all",
        flatten = TRUE
      ),
      c("child", "camper", "persons", "livebait")
    )
    expect_identical(
      find_predictors(m3, effects = "random", component = "all"),
      list(random = "persons", zero_inflated_random = "persons")
    )
    expect_identical(
      find_predictors(
        m3,
        effects = "random",
        component = "all",
        flatten = TRUE
      ),
      "persons"
    )

    expect_null(find_predictors(
      m6,
      effects = "random",
      component = "all",
      flatten = TRUE
    ))
  })

  test_that("find_formula", {
    expect_length(find_formula(m4), 5)
    expect_equal(
      find_formula(m4),
      list(
        conditional = as.formula("count ~ child + camper"),
        random = as.formula("~1 | persons"),
        zero_inflated = as.formula("~child + livebait"),
        zero_inflated_random = as.formula("~1 | ID"),
        dispersion = as.formula("~xb")
      )
    )
    expect_equal(find_formula(m6), list(conditional = as.formula("count ~ 1")))
  })

  test_that("find_predictors", {
    expect_identical(
      find_predictors(m4),
      list(
        conditional = c("child", "camper"),
        zero_inflated = c("child", "livebait"),
        dispersion = "xb"
      )
    )
    expect_identical(
      find_predictors(m4, flatten = TRUE),
      c("child", "camper", "livebait", "xb")
    )
    expect_identical(
      find_predictors(m4, effects = "random"),
      list(random = "persons", zero_inflated_random = "ID")
    )
    expect_identical(
      find_predictors(m4, effects = "all", flatten = TRUE),
      c("child", "camper", "persons", "livebait", "ID", "xb")
    )
    expect_identical(
      find_predictors(m4, effects = "all"),
      list(
        conditional = c("child", "camper"),
        random = "persons",
        zero_inflated = c("child", "livebait"),
        zero_inflated_random = "ID",
        dispersion = "xb"
      )
    )
    expect_identical(
      find_predictors(m4, component = "conditional", flatten = TRUE),
      c("child", "camper")
    )
    expect_identical(
      find_predictors(m4, component = "conditional", flatten = FALSE),
      list(conditional = c("child", "camper"))
    )
    expect_identical(
      find_predictors(m4, effects = "random", component = "conditional"),
      list(random = "persons")
    )
    expect_identical(
      find_predictors(m4, effects = "all", component = "conditional"),
      list(
        conditional = c("child", "camper"),
        random = "persons"
      )
    )
    expect_identical(
      find_predictors(m4, component = "zero_inflated"),
      list(zero_inflated = c("child", "livebait"))
    )
    expect_identical(
      find_predictors(m4, effects = "random", component = "zero_inflated"),
      list(zero_inflated_random = "ID")
    )
    expect_identical(
      find_predictors(
        m4,
        effects = "all",
        component = "zero_inflated",
        flatten = TRUE
      ),
      c("child", "livebait", "ID")
    )
    expect_identical(
      find_predictors(m4, component = "dispersion"),
      list(dispersion = "xb")
    )
    expect_identical(
      find_predictors(m4, component = "dispersion", flatten = TRUE),
      "xb"
    )
    expect_null(find_predictors(m4, effects = "random", component = "dispersion"))
    expect_identical(
      find_predictors(m4, effects = "all", component = "dispersion"),
      list(dispersion = "xb")
    )
    expect_identical(
      find_predictors(
        m4,
        effects = "all",
        component = "dispersion",
        flatten = TRUE
      ),
      "xb"
    )
  })

  test_that("find_random", {
    expect_identical(
      find_random(m4),
      list(random = "persons", zero_inflated_random = "ID")
    )
    expect_identical(find_random(m4, flatten = TRUE), c("persons", "ID"))
    expect_null(find_random(m6, flatten = TRUE))
  })

  test_that("find_respone", {
    expect_identical(find_response(m4), "count")
    expect_identical(find_response(m6), "count")
  })

  test_that("find_terms", {
    expect_identical(
      find_terms(m4),
      list(
        response = "count",
        conditional = c("child", "camper"),
        random = "persons",
        zero_inflated = c("child", "livebait"),
        zero_inflated_random = "ID",
        dispersion = "xb"
      )
    )
    expect_identical(
      find_terms(m4, flatten = TRUE),
      c(
        "count",
        "child",
        "camper",
        "persons",
        "livebait",
        "ID",
        "xb"
      )
    )
    expect_identical(find_terms(m6), list(response = "count", conditional = "1"))
    expect_identical(find_terms(m6, flatten = TRUE), c("count", "1"))
  })

  test_that("find_variables", {
    expect_identical(
      find_variables(m4),
      list(
        response = "count",
        conditional = c("child", "camper"),
        random = "persons",
        zero_inflated = c("child", "livebait"),
        zero_inflated_random = "ID",
        dispersion = "xb"
      )
    )
    expect_identical(
      find_variables(m4, flatten = TRUE),
      c(
        "count",
        "child",
        "camper",
        "persons",
        "livebait",
        "ID",
        "xb"
      )
    )
    expect_identical(find_variables(m6), list(response = "count"))
    expect_identical(find_variables(m6, flatten = TRUE), "count")
  })

  test_that("get_response", {
    expect_identical(get_response(m4), fish$count)
    expect_identical(get_response(m6), Salamanders$count)
  })

  test_that("get_predictors", {
    expect_identical(
      colnames(get_predictors(m4)),
      c("child", "camper", "livebait", "xb")
    )
    expect_null(get_predictors(m6))
  })

  test_that("get_random", {
    expect_identical(colnames(get_random(m4)), c("persons", "ID"))
    expect_warning(expect_null(get_random(m6)))
  })

  test_that("get_data", {
    expect_identical(
      colnames(get_data(m4)),
      c(
        "count",
        "child",
        "camper",
        "livebait",
        "xb",
        "persons",
        "ID"
      )
    )
    expect_identical(
      colnames(get_data(m4, effects = "fixed")),
      c("count", "child", "camper", "livebait", "xb")
    )
    expect_identical(colnames(get_data(m4, effects = "random")), c("persons", "ID"))
    expect_identical(colnames(get_data(m4, component = "zi")), c("count", "child", "livebait", "ID"))
    expect_identical(colnames(get_data(
      m4,
      component = "zi", effects = "fixed"
    )), c("count", "child", "livebait"))
    expect_identical(colnames(get_data(
      m4,
      component = "zi", effects = "random"
    )), "ID")
    expect_identical(
      colnames(get_data(m4, component = "cond")),
      c("count", "child", "camper", "persons")
    )
    expect_identical(colnames(get_data(
      m4,
      component = "cond", effects = "fixed"
    )), c("count", "child", "camper"))
    expect_identical(colnames(get_data(
      m4,
      component = "cond", effects = "random"
    )), "persons")
    expect_identical(colnames(get_data(m4, component = "disp")), c("count", "xb"))
    expect_identical(colnames(get_data(
      m4,
      component = "disp", effects = "fixed"
    )), c("count", "xb"))
    expect_null(get_data(m4, component = "disp", effects = "random"))
  })

  test_that("find_paramaters", {
    expect_equal(
      find_parameters(m4),
      list(
        conditional = c("(Intercept)", "child", "camper1"),
        random = list(persons = "(Intercept)"),
        zero_inflated = c("(Intercept)", "child", "livebait1"),
        zero_inflated_random = list(ID = "(Intercept)")
      )
    )

    expect_equal(
      find_parameters(m4, flatten = TRUE),
      c("(Intercept)", "child", "camper1", "livebait1")
    )
    expect_equal(
      find_parameters(m6),
      list(
        conditional = "(Intercept)",
        zero_inflated = "(Intercept)"
      )
    )

    expect_equal(
      find_parameters(m3),
      list(
        conditional = c("(Intercept)", "child", "camper1"),
        random = list(persons = "(Intercept)"),
        zero_inflated = c("(Intercept)", "child", "livebait1"),
        zero_inflated_random = list(persons = "(Intercept)")
      )
    )

    expect_equal(
      find_parameters(m3),
      list(
        conditional = c("(Intercept)", "child", "camper1"),
        random = list(persons = "(Intercept)"),
        zero_inflated = c("(Intercept)", "child", "livebait1"),
        zero_inflated_random = list(persons = "(Intercept)")
      )
    )

    expect_equal(
      find_parameters(m3, effects = "fixed"),
      list(
        conditional = c("(Intercept)", "child", "camper1"),
        zero_inflated = c("(Intercept)", "child", "livebait1")
      )
    )

    expect_equal(
      find_parameters(m3, effects = "random", component = "zi"),
      list(zero_inflated_random = list(persons = "(Intercept)"))
    )

    expect_equal(
      find_parameters(
        m3,
        effects = "fixed",
        component = "zi",
        flatten = TRUE
      ),
      c("(Intercept)", "child", "livebait1")
    )
  })


  test_that("get_paramaters", {
    expect_equal(nrow(get_parameters(m4)), 6)
    expect_equal(
      colnames(get_parameters(m4)),
      c("Parameter", "Estimate", "Component")
    )
    expect_equal(
      get_parameters(m4)$Parameter,
      c(
        "(Intercept)",
        "child",
        "camper1",
        "(Intercept)",
        "child",
        "livebait1"
      )
    )
    expect_equal(
      get_parameters(m4)$Component,
      c(
        "conditional",
        "conditional",
        "conditional",
        "zero_inflated",
        "zero_inflated",
        "zero_inflated"
      )
    )
    expect_equal(
      get_parameters(m6)$Parameter,
      c("(Intercept)", "(Intercept)")
    )

    expect_equal(
      get_parameters(m2)$Parameter,
      c("(Intercept)", "child", "camper1")
    )

    expect_equal(
      get_parameters(m2, component = "all")$Parameter,
      c("(Intercept)", "child", "camper1")
    )

    expect_null(get_parameters(m2, component = "zi"))
  })

  test_that("linkfun", {
    expect_false(is.null(link_function(m1)))
    expect_false(is.null(link_function(m2)))
    expect_false(is.null(link_function(m3)))
    expect_false(is.null(link_function(m4)))
  })

  test_that("is_multivariate", {
    expect_false(is_multivariate(m1))
    expect_false(is_multivariate(m2))
    expect_false(is_multivariate(m3))
    expect_false(is_multivariate(m4))
  })

  # test_that("get_variance", {
  #
  #   expect_warning(expect_equal(get_variance(m5), list(
  #     var.fixed = 0.32588694431268194762,
  #     var.random = 0.07842738279575413307,
  #     var.residual = 0.41218000030914692111,
  #     var.distribution = 0.41218000030914692111,
  #     var.dispersion = 0,
  #     var.intercept = c(site = 0.07842738279575474369)
  #   ),
  #   tolerance = 1e-3))
  #
  #   expect_warning(expect_equal(get_variance_fixed(m1), c(var.fixed = 1.09712435712435052437), tolerance = 1e-3))
  #   expect_warning(expect_equal(get_variance_random(m1), c(var.random = 0.86712737445492238386), tolerance = 1e-3))
  #   expect_warning(expect_equal(get_variance_residual(m1), c(var.residual = 0.02634500773355940087 ), tolerance = 1e-3))
  #   expect_warning(expect_equal(get_variance_distribution(m1), c(var.distribution = 0.02634500773355940087 ), tolerance = 1e-3))
  #   expect_warning(expect_equal(get_variance_dispersion(m1), c(var.dispersion = 0), tolerance = 1e-3))
  # })

  test_that("find_algorithm", {
    expect_equal(
      find_algorithm(m1),
      list(algorithm = "ML", optimizer = "nlminb")
    )
  })

  test_that("find_random_slopes", {
    skip_on_cran()
    skip_on_travis()

    expect_null(find_random_slopes(m6))

    expect_equal(
      find_random_slopes(m7),
      list(
        random = "xb",
        zero_inflated_random = c("zg", "nofish")
      )
    )
  })

  test_that("clean_parameters", {
    expect_equal(
      clean_parameters(m1),
      structure(
        list(
          Parameter = c(
            "(Intercept)",
            "child",
            "camper1",
            "(Intercept)",
            "(Intercept)",
            "child",
            "camper1",
            "(Intercept)"
          ),
          Effects = c(
            "fixed",
            "fixed",
            "fixed",
            "random",
            "fixed",
            "fixed",
            "fixed",
            "random"
          ),
          Component = c(
            "conditional",
            "conditional",
            "conditional",
            "conditional",
            "zero_inflated",
            "zero_inflated",
            "zero_inflated",
            "zero_inflated"
          ),
          Group = c("", "", "", "persons", "", "", "", "persons"),
          Cleaned_Parameter = c(
            "(Intercept)",
            "child",
            "camper1",
            "(Intercept)",
            "(Intercept)",
            "child",
            "camper1",
            "(Intercept)"
          )
        ),
        class = c("clean_parameters", "data.frame"),
        row.names = c(NA, -8L)
      )
    )
  })

  test_that("find_statistic", {
    expect_identical(find_statistic(m1), "z-statistic")
    expect_identical(find_statistic(m2), "z-statistic")
    expect_identical(find_statistic(m3), "z-statistic")
    expect_identical(find_statistic(m4), "z-statistic")
    expect_identical(find_statistic(m5), "z-statistic")
    expect_identical(find_statistic(m6), "z-statistic")
    expect_identical(find_statistic(m7), "z-statistic")
  })
}
