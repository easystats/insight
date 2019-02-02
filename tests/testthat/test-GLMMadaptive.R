if (require("testthat") && require("insight") && require("GLMMadaptive")) {
  context("insight, model_info")

  # data(fish)
  fish <- read.csv("https://stats.idre.ucla.edu/stat/data/fish.csv")
  fish$nofish <- as.factor(fish$nofish)
  fish$livebait <- as.factor(fish$livebait)
  fish$camper <- as.factor(fish$camper)

  m <- mixed_model(
    count ~ child + camper,
    random = ~ 1 | persons,
    zi_fixed = ~ child + livebait,
    zi_random = ~ 1 | persons,
    data = fish,
    family = zi.poisson()
  )

  test_that("model_info", {
    expect_true(model_info(m)$is_zeroinf)
    expect_true(model_info(m)$is_count)
    expect_true(model_info(m)$is_pois)
    expect_false(model_info(m)$is_negbin)
  })

  test_that("find_predictors", {
    expect_identical(find_predictors(m, effects = "fixed"), c("child", "camper", "livebait"))
    expect_identical(find_predictors(m, effects = "all"), c("child", "camper", "livebait", "persons"))
    expect_identical(find_predictors(m, effects = "random"), "persons")
    expect_identical(find_predictors(m, effects = "fixed", component = "cond"), c("child", "camper"))
    expect_identical(find_predictors(m, effects = "all", component = "cond"), c("child", "camper", "persons"))
    expect_identical(find_predictors(m, effects = "random", component = "cond"), "persons")
    expect_identical(find_predictors(m, effects = "fixed", component = "zi"), c("child", "livebait"))
    expect_identical(find_predictors(m, effects = "all", component = "zi"), c("child", "livebait", "persons"))
    expect_identical(find_predictors(m, effects = "random", component = "zi"), "persons")
    expect_null(find_predictors(m, effects = "fixed", component = "disp"))
    expect_null(find_predictors(m, effects = "all", component = "disp"))
    expect_warning(find_predictors(m, effects = "random", component = "disp"))
  })

  test_that("find_response", {
    expect_identical(find_response(m), "count")
  })

  test_that("link_inverse", {
    expect_identical(link_inverse(m)(.2), exp(.2))
  })

  test_that("clean_names", {
    expect_identical(clean_names(m), c("count", "child", "camper", "persons", "livebait"))
  })

  test_that("find_formula", {
    expect_length(find_formula(m), 4)
    expect_identical(names(find_formula(m)), c("conditional", "zero_inflated", "random", "zero_inflated_random"))
    expect_length(find_formula(m, component = "cond"), 2)
    expect_identical(names(find_formula(m, component = "cond")), c("conditional", "random"))
    expect_length(find_formula(m, component = "zi"), 2)
    expect_identical(names(find_formula(m, component = "zi")), c("zero_inflated", "zero_inflated_random"))
    expect_null(find_formula(m, component = "disp"))

    expect_length(find_formula(m, effects = "fixed"), 2)
    expect_identical(names(find_formula(m, effects = "fixed")), c("conditional", "zero_inflated"))
    expect_length(find_formula(m, effects = "fixed", component = "cond"), 1)
    expect_identical(names(find_formula(m, effects = "fixed", component = "cond")), "conditional")
    expect_length(find_formula(m, effects = "fixed", component = "zi"), 1)
    expect_identical(names(find_formula(m, effects = "fixed", component = "zi")), "zero_inflated")
    expect_null(find_formula(m, effects = "fixed", component = "disp"))

    expect_length(find_formula(m, effects = "random"), 2)
    expect_identical(names(find_formula(m, effects = "random")), c("random", "zero_inflated_random"))
    expect_length(find_formula(m, effects = "random", component = "cond"), 1)
    expect_identical(names(find_formula(m, effects = "random", component = "cond")), "random")
    expect_length(find_formula(m, effects = "random", component = "zi"), 1)
    expect_identical(names(find_formula(m, effects = "random", component = "zi")), "zero_inflated_random")
    expect_null(find_formula(m, effects = "random", component = "disp"))
  })

  test_that("find_random", {
    expect_identical(find_random(m), "persons")
    expect_identical(find_random(m, component = "cond"), "persons")
    expect_identical(find_random(m, component = "zi"), "persons")
    expect_identical(find_random(m, flatten = TRUE), "persons")
    expect_identical(find_random(m, component = "cond", flatten = TRUE), "persons")
    expect_identical(find_random(m, component = "zi", flatten = TRUE), "persons")
  })

  test_that("find_respone", {
    expect_identical(find_response(m), "count")
  })

  test_that("find_terms", {
    expect_identical(find_terms(m), list(
      response = "count",
      conditional = c("child", "camper"),
      random = "persons",
      zero_inflated = c("child", "livebait"),
      zero_inflated_random = "persons"
    ))
    expect_identical(find_terms(m, flatten = TRUE), c("count", "child", "camper", "persons", "livebait"))
  })

  test_that("get_response", {
    expect_identical(get_response(m), fish$count)
  })

  test_that("get_predictors", {
    expect_identical(colnames(get_predictors(m)), c("child", "camper", "livebait"))
  })

  test_that("get_random", {
    expect_identical(colnames(get_random(m)), "persons")
  })

  test_that("get_data", {
    expect_identical(colnames(get_data(m)), c("count", "child", "camper", "livebait", "persons"))
    expect_identical(colnames(get_data(m, effects = "fixed")), c("count", "child", "camper", "livebait"))
    expect_identical(colnames(get_data(m, effects = "random")), "persons")
    expect_identical(colnames(get_data(m, component = "zi")), c("count", "child", "livebait", "persons"))
    expect_identical(colnames(get_data(m, component = "zi", effects = "fixed")), c("count", "child", "livebait"))
    expect_identical(colnames(get_data(m, component = "zi", effects = "random")), "persons")
    expect_identical(colnames(get_data(m, component = "cond")), c("count", "child", "camper", "persons"))
    expect_identical(colnames(get_data(m, component = "cond", effects = "fixed")), c("count", "child", "camper"))
    expect_identical(colnames(get_data(m, component = "cond", effects = "random")), "persons")
    expect_identical(colnames(get_data(m, component = "disp")), "count")
    expect_warning(colnames(get_data(m, component = "disp", effects = "random")))
  })

}
