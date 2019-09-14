.runThisTest <- Sys.getenv("RunAllinsightTests") == "yes"

if (.runThisTest || Sys.getenv("USER") == "travis") {
  if (suppressWarnings(require("testthat") &&
    require("insight") &&
    require("brms"))) {
    context("insight, brms-find_response")

    # Model fitting -----------------------------------------------------------

    m1 <- insight::download_model("brms_mixed_6")
    m2 <- insight::download_model("brms_mv_4")
    m3 <- insight::download_model("brms_2")
    m4 <- insight::download_model("brms_zi_3")
    m5 <- insight::download_model("brms_mv_5")

    # Tests -------------------------------------------------------------------

    test_that("find_statistic", {
      expect_null(find_statistic(m1))
      expect_null(find_statistic(m2))
      expect_null(find_statistic(m3))
      expect_null(find_statistic(m4))
      expect_null(find_statistic(m5))
    })

    test_that("model_info", {
      expect_true(model_info(m3)$is_trial)
      expect_true(model_info(m5)[[1]]$is_zero_inflated)
      expect_true(model_info(m5)[[1]]$is_bayesian)
    })

    test_that("clean_names", {
      expect_identical(
        clean_names(m1),
        c("count", "Age", "Base", "Trt", "patient")
      )
      expect_identical(
        clean_names(m2),
        c(
          "Sepal.Length",
          "Sepal.Width",
          "Petal.Length",
          "Species"
        )
      )
      expect_identical(clean_names(m3), c("r", "n", "treat", "c2"))
      expect_identical(
        clean_names(m4),
        c("count", "child", "camper", "persons")
      )
      expect_identical(
        clean_names(m5),
        c(
          "count",
          "count2",
          "child",
          "camper",
          "persons",
          "livebait"
        )
      )
    })


    test_that("find_predictors", {
      expect_identical(find_predictors(m1), list(conditional = c("Age", "Base", "Trt")))
      expect_identical(
        find_predictors(m1, flatten = TRUE),
        c("Age", "Base", "Trt")
      )
      expect_identical(
        find_predictors(m1, effects = "all", component = "all"),
        list(
          conditional = c("Age", "Base", "Trt"),
          random = "patient"
        )
      )
      expect_identical(
        find_predictors(
          m1,
          effects = "all",
          component = "all",
          flatten = TRUE
        ),
        c("Age", "Base", "Trt", "patient")
      )

      expect_identical(
        find_predictors(m2),
        list(
          SepalLength = list(conditional = c(
            "Petal.Length", "Sepal.Width", "Species"
          )),
          SepalWidth = list(conditional = "Species")
        )
      )

      expect_identical(
        find_predictors(m2, flatten = TRUE),
        c("Petal.Length", "Sepal.Width", "Species")
      )
      expect_identical(find_predictors(m3), list(conditional = c("treat", "c2")))
      expect_identical(
        find_predictors(m4),
        list(
          conditional = c("child", "camper"),
          zero_inflated = c("child", "camper")
        )
      )
      expect_identical(
        find_predictors(m4, effects = "random"),
        list(random = "persons", zero_inflated_random = "persons")
      )
      expect_identical(find_predictors(m4, flatten = TRUE), c("child", "camper"))

      expect_identical(
        find_predictors(m5),
        list(
          count = list(
            conditional = c("child", "camper"),
            zero_inflated = "camper"
          ),
          count2 = list(
            conditional = c("child", "livebait"),
            zero_inflated = "child"
          )
        )
      )
    })

    test_that("find_response", {
      expect_equal(find_response(m1, combine = TRUE), "count")
      expect_equal(
        find_response(m2, combine = TRUE),
        c(SepalLength = "Sepal.Length", SepalWidth = "Sepal.Width")
      )
      expect_equal(find_response(m3, combine = TRUE), c("r", "n"))
      expect_equal(find_response(m1, combine = FALSE), "count")
      expect_equal(
        find_response(m2, combine = FALSE),
        c(SepalLength = "Sepal.Length", SepalWidth = "Sepal.Width")
      )
      expect_equal(find_response(m3, combine = FALSE), c("r", "n"))
      expect_equal(find_response(m4, combine = FALSE), "count")
      expect_equal(
        find_response(m5, combine = TRUE),
        c(count = "count", count2 = "count2")
      )
    })

    test_that("get_response", {
      expect_length(get_response(m1), 236)
      expect_equal(ncol(get_response(m2)), 2)
      expect_equal(
        colnames(get_response(m2)),
        c("Sepal.Length", "Sepal.Width")
      )
      expect_equal(ncol(get_response(m3)), 2)
      expect_equal(colnames(get_response(m3)), c("r", "n"))
      expect_length(get_response(m4), 250)
      expect_equal(colnames(get_response(m5)), c("count", "count2"))
    })

    test_that("find_variables", {
      expect_identical(
        find_variables(m1),
        list(
          response = "count",
          conditional = c("Age", "Base", "Trt"),
          random = "patient"
        )
      )
      expect_identical(
        find_variables(m1, effects = "fixed"),
        list(
          response = "count",
          conditional = c("Age", "Base", "Trt")
        )
      )
      expect_null(find_variables(m1, component = "zi"))

      expect_identical(
        find_variables(m2),
        list(
          response = c(SepalLength = "Sepal.Length", SepalWidth = "Sepal.Width"),
          SepalLength = list(conditional = c(
            "Petal.Length", "Sepal.Width", "Species"
          )),
          SepalWidth = list(conditional = "Species")
        )
      )

      expect_identical(
        find_variables(m2, flatten = TRUE),
        c(
          "Sepal.Length",
          "Sepal.Width",
          "Petal.Length",
          "Species"
        )
      )
      expect_identical(find_variables(m3), list(
        response = c("r", "n"),
        conditional = c("treat", "c2")
      ))

      expect_identical(
        find_variables(m4),
        list(
          response = "count",
          conditional = c("child", "camper"),
          random = "persons",
          zero_inflated = c("child", "camper"),
          zero_inflated_random = "persons"
        )
      )

      expect_identical(
        find_variables(m4, flatten = TRUE),
        c("count", "child", "camper", "persons")
      )
    })

    test_that("n_obs", {
      expect_equal(n_obs(m1), 236)
      expect_equal(n_obs(m2), 150)
      expect_equal(n_obs(m3), 10)
      expect_equal(n_obs(m4), 250)
      expect_equal(n_obs(m5), 250)
    })


    test_that("find_random", {
      expect_equal(find_random(m5), list(
        count = list(
          random = "persons",
          zero_inflated_random = "persons"
        ),
        count2 = list(
          random = "persons",
          zero_inflated_random = "persons"
        )
      ))
      expect_equal(find_random(m5, flatten = TRUE), "persons")
    })


    test_that("get_random", {
      zinb <- get_data(m4)
      expect_equal(get_random(m4), zinb[, "persons", drop = FALSE])
    })


    test_that("find_paramaters", {
      expect_equal(
        find_parameters(m1),
        list(
          conditional = c(
            "b_Intercept",
            "b_Age",
            "b_Base",
            "b_Trt1",
            "b_Base.Trt1"
          ),
          random = sprintf("r_patient.%i.Intercept.", 1:59)
        )
      )

      expect_equal(
        find_parameters(m2),
        structure(list(
          SepalLength = list(
            conditional = c(
              "b_SepalLength_Intercept",
              "b_SepalLength_Petal.Length",
              "b_SepalLength_Sepal.Width",
              "b_SepalLength_Speciesversicolor",
              "b_SepalLength_Speciesvirginica"
            ),
            sigma = "sigma_SepalLength"
          ),
          SepalWidth = list(
            conditional = c(
              "b_SepalWidth_Intercept",
              "b_SepalWidth_Speciesversicolor",
              "b_SepalWidth_Speciesvirginica"
            ),
            sigma = "sigma_SepalWidth"
          )
        ),
        "is_mv" = "1"
        )
      )

      expect_equal(
        find_parameters(m4),
        list(
          conditional = c("b_Intercept", "b_child", "b_camper"),
          random = sprintf("r_persons.%i.Intercept.", 1:4),
          zero_inflated = c("b_zi_Intercept", "b_zi_child", "b_zi_camper"),
          zero_inflated_random = sprintf("r_persons__zi.%i.Intercept.", 1:4)
        )
      )

      expect_equal(
        find_parameters(m5),
        structure(list(
          count = list(
            conditional = c("b_count_Intercept", "b_count_child", "b_count_camper"),
            random = sprintf("r_persons__count.%i.Intercept.", 1:4),
            zero_inflated = c("b_zi_count_Intercept", "b_zi_count_camper"),
            zero_inflated_random = sprintf("r_persons__zi_count.%i.Intercept.", 1:4)
          ),
          count2 = list(
            conditional = c(
              "b_count2_Intercept",
              "b_count2_child",
              "b_count2_livebait"
            ),
            random = sprintf("r_persons__count2.%i.Intercept.", 1:4),
            zero_inflated = c("b_zi_count2_Intercept", "b_zi_count2_child"),
            zero_inflated_random = sprintf("r_persons__zi_count2.%i.Intercept.", 1:4)
          )
        ),
        "is_mv" = "1"
        )
      )
    })

    test_that("find_paramaters", {
      expect_equal(
        colnames(get_parameters(m4)),
        c(
          "b_Intercept",
          "b_child",
          "b_camper",
          "b_zi_Intercept",
          "b_zi_child",
          "b_zi_camper"
        )
      )
      expect_equal(
        colnames(get_parameters(m4, component = "zi")),
        c("b_zi_Intercept", "b_zi_child", "b_zi_camper")
      )
      expect_equal(
        colnames(get_parameters(m4, effects = "all")),
        c(
          "b_Intercept",
          "b_child",
          "b_camper",
          "r_persons.1.Intercept.",
          "r_persons.2.Intercept.",
          "r_persons.3.Intercept.",
          "r_persons.4.Intercept.",
          "b_zi_Intercept",
          "b_zi_child",
          "b_zi_camper",
          "r_persons__zi.1.Intercept.",
          "r_persons__zi.2.Intercept.",
          "r_persons__zi.3.Intercept.",
          "r_persons__zi.4.Intercept."
        )
      )
      expect_equal(
        colnames(get_parameters(
          m4,
          effects = "random", component = "cond"
        )),
        c(
          "r_persons.1.Intercept.",
          "r_persons.2.Intercept.",
          "r_persons.3.Intercept.",
          "r_persons.4.Intercept."
        )
      )

      expect_equal(
        colnames(get_parameters(
          m5,
          effects = "random", component = "cond"
        )),
        c(
          "r_persons__count.1.Intercept.",
          "r_persons__count.2.Intercept.",
          "r_persons__count.3.Intercept.",
          "r_persons__count.4.Intercept.",
          "r_persons__count2.1.Intercept.",
          "r_persons__count2.2.Intercept.",
          "r_persons__count2.3.Intercept.",
          "r_persons__count2.4.Intercept."
        )
      )

      expect_equal(
        colnames(get_parameters(
          m5,
          effects = "all", component = "all"
        )),
        c(
          "b_count_Intercept",
          "b_count_child",
          "b_count_camper",
          "r_persons__count.1.Intercept.",
          "r_persons__count.2.Intercept.",
          "r_persons__count.3.Intercept.",
          "r_persons__count.4.Intercept.",
          "b_zi_count_Intercept",
          "b_zi_count_camper",
          "r_persons__zi_count.1.Intercept.",
          "r_persons__zi_count.2.Intercept.",
          "r_persons__zi_count.3.Intercept.",
          "r_persons__zi_count.4.Intercept.",
          "b_count2_Intercept",
          "b_count2_child",
          "b_count2_livebait",
          "r_persons__count2.1.Intercept.",
          "r_persons__count2.2.Intercept.",
          "r_persons__count2.3.Intercept.",
          "r_persons__count2.4.Intercept.",
          "b_zi_count2_Intercept",
          "b_zi_count2_child",
          "r_persons__zi_count2.1.Intercept.",
          "r_persons__zi_count2.2.Intercept.",
          "r_persons__zi_count2.3.Intercept.",
          "r_persons__zi_count2.4.Intercept."
        )
      )
    })

    test_that("linkfun", {
      expect_false(is.null(link_function(m1)))
      expect_length(link_function(m2), 2)
      expect_false(is.null(link_function(m3)))
      expect_false(is.null(link_function(m4)))
      expect_length(link_function(m5), 2)
    })

    test_that("linkinv", {
      expect_false(is.null(link_inverse(m1)))
      expect_length(link_inverse(m2), 2)
      expect_false(is.null(link_inverse(m3)))
      expect_false(is.null(link_inverse(m4)))
      expect_length(link_inverse(m2), 2)
    })

    test_that("is_multivariate", {
      expect_false(is_multivariate(m1))
      expect_true(is_multivariate(m2))
      expect_false(is_multivariate(m3))
      expect_false(is_multivariate(m4))
      expect_true(is_multivariate(m5))
    })

    test_that("find_terms", {
      expect_equal(
        find_terms(m2),
        list(
          SepalLength = list(
            response = "Sepal.Length",
            conditional = c("Petal.Length", "Sepal.Width", "Species")
          ),
          SepalWidth = list(
            response = "Sepal.Width",
            conditional = "Species"
          )
        )
      )
    })

    test_that("find_algorithm", {
      expect_equal(
        find_algorithm(m1),
        list(
          algorithm = "sampling",
          chains = 1,
          iterations = 500,
          warmup = 250
        )
      )
    })


    test_that("get_priors", {
      expect_equal(
        get_priors(m1),
        data.frame(
          parameter = c("Age", "Base", "Base:Trt1", "Trt1"),
          distribution = c("student_t", "student_t", "student_t", "student_t"),
          location = c("5, 0", "5, 0", "5, 0", "5, 0"),
          scale = c(10, 10, 10, 10),
          stringsAsFactors = FALSE
        )
      )

      expect_equal(
        get_priors(m3),
        data.frame(
          parameter = c("c2", "treat1", "treat1:c2"),
          distribution = c("uniform", "uniform", "uniform"),
          location = c(0, 0, 0),
          scale = c(NA, NA, NA),
          stringsAsFactors = FALSE
        )
      )
    })


    test_that("clean_parameters", {
      expect_equal(
        clean_parameters(m4),
        structure(
          list(
            Parameter = c(
              "b_Intercept",
              "b_child",
              "b_camper",
              "r_persons.1.Intercept.",
              "r_persons.2.Intercept.",
              "r_persons.3.Intercept.",
              "r_persons.4.Intercept.",
              "b_zi_Intercept",
              "b_zi_child",
              "b_zi_camper",
              "r_persons__zi.1.Intercept.",
              "r_persons__zi.2.Intercept.",
              "r_persons__zi.3.Intercept.",
              "r_persons__zi.4.Intercept."
            ),
            Effects = c(
              "fixed",
              "fixed",
              "fixed",
              "random",
              "random",
              "random",
              "random",
              "fixed",
              "fixed",
              "fixed",
              "random",
              "random",
              "random",
              "random"
            ),
            Component = c(
              "conditional",
              "conditional",
              "conditional",
              "conditional",
              "conditional",
              "conditional",
              "conditional",
              "zero_inflated",
              "zero_inflated",
              "zero_inflated",
              "zero_inflated",
              "zero_inflated",
              "zero_inflated",
              "zero_inflated"
            ),
            Group = c(
              "",
              "",
              "",
              "Intercept: persons",
              "Intercept: persons",
              "Intercept: persons",
              "Intercept: persons",
              "",
              "",
              "",
              "Intercept: persons",
              "Intercept: persons",
              "Intercept: persons",
              "Intercept: persons"
            ),
            Cleaned_Parameter = c(
              "(Intercept)",
              "child",
              "camper",
              "persons.1",
              "persons.2",
              "persons.3",
              "persons.4",
              "(Intercept)",
              "child",
              "camper",
              "persons.1",
              "persons.2",
              "persons.3",
              "persons.4"
            )
          ),
          class = c("clean_parameters", "data.frame"),
          row.names = c(NA, -14L)
        )
      )

      expect_equal(
        clean_parameters(m5),
        structure(
          list(
            Parameter = c(
              "b_count_Intercept",
              "b_count_child",
              "b_count_camper",
              "b_count2_Intercept",
              "b_count2_child",
              "b_count2_livebait",
              "r_persons__count.1.Intercept.",
              "r_persons__count.2.Intercept.",
              "r_persons__count.3.Intercept.",
              "r_persons__count.4.Intercept.",
              "r_persons__count2.1.Intercept.",
              "r_persons__count2.2.Intercept.",
              "r_persons__count2.3.Intercept.",
              "r_persons__count2.4.Intercept.",
              "b_zi_count_Intercept",
              "b_zi_count_camper",
              "b_zi_count2_Intercept",
              "b_zi_count2_child",
              "r_persons__zi_count.1.Intercept.",
              "r_persons__zi_count.2.Intercept.",
              "r_persons__zi_count.3.Intercept.",
              "r_persons__zi_count.4.Intercept.",
              "r_persons__zi_count2.1.Intercept.",
              "r_persons__zi_count2.2.Intercept.",
              "r_persons__zi_count2.3.Intercept.",
              "r_persons__zi_count2.4.Intercept."
            ),
            Effects = c(
              "fixed",
              "fixed",
              "fixed",
              "fixed",
              "fixed",
              "fixed",
              "random",
              "random",
              "random",
              "random",
              "random",
              "random",
              "random",
              "random",
              "fixed",
              "fixed",
              "fixed",
              "fixed",
              "random",
              "random",
              "random",
              "random",
              "random",
              "random",
              "random",
              "random"
            ),
            Component = c(
              "conditional",
              "conditional",
              "conditional",
              "conditional",
              "conditional",
              "conditional",
              "conditional",
              "conditional",
              "conditional",
              "conditional",
              "conditional",
              "conditional",
              "conditional",
              "conditional",
              "zero_inflated",
              "zero_inflated",
              "zero_inflated",
              "zero_inflated",
              "zero_inflated",
              "zero_inflated",
              "zero_inflated",
              "zero_inflated",
              "zero_inflated",
              "zero_inflated",
              "zero_inflated",
              "zero_inflated"
            ),
            Group = c(
              "",
              "",
              "",
              "",
              "",
              "",
              "Intercept: persons",
              "Intercept: persons",
              "Intercept: persons",
              "Intercept: persons",
              "Intercept: persons2",
              "Intercept: persons2",
              "Intercept: persons2",
              "Intercept: persons2",
              "",
              "",
              "",
              "",
              "Intercept: persons",
              "Intercept: persons",
              "Intercept: persons",
              "Intercept: persons",
              "Intercept: persons2",
              "Intercept: persons2",
              "Intercept: persons2",
              "Intercept: persons2"
            ),
            Response = c(
              "count",
              "count",
              "count",
              "count2",
              "count2",
              "count2",
              "count",
              "count",
              "count",
              "count",
              "count2",
              "count2",
              "count2",
              "count2",
              "count",
              "count",
              "count2",
              "count2",
              "count",
              "count",
              "count",
              "count",
              "count2",
              "count2",
              "count2",
              "count2"
            ),
            Cleaned_Parameter = c(
              "(Intercept)",
              "child",
              "camper",
              "(Intercept)",
              "child",
              "livebait",
              "persons.1",
              "persons.2",
              "persons.3",
              "persons.4",
              "persons2.1",
              "persons2.2",
              "persons2.3",
              "persons2.4",
              "(Intercept)",
              "camper",
              "(Intercept)",
              "child",
              "persons.1",
              "persons.2",
              "persons.3",
              "persons.4",
              "persons2.1",
              "persons2.2",
              "persons2.3",
              "persons2.4"
            )
          ),
          class = c("clean_parameters", "data.frame"),
          row.names = c(NA, -26L)
        )
      )
    })
  }
}
