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

  # test_that("find_response", {
  #   expect_identical(find_response(m), "count")
  # })
  #
  # test_that("link_inverse", {
  #   expect_identical(link_inverse(m)(.2), exp(.2))
  # })
  #
  # test_that("get_data", {
  #   expect_equal(colnames(get_data(m)), c("count", "child", "camper", "persons"))
  #   expect_equal(colnames(get_data(m, effects = "all")), c("count", "child", "camper", "livebait", "persons"))
  #   expect_equal(colnames(get_data(m, effects = "random")), c("count", "child", "camper", "livebait", "persons"))
  # })
  #
  # test_that("find_formula", {
  #   expect_length(find_formula(m), 3)
  #   expect_identical(find_formula(m, component = "cond"), )
  #   expect_identical(find_formula(m4, component = "zi"), stats::formula(m4, component = "zi"))
  #   expect_identical(find_formula(m4, component = "disp"), stats::formula(m4, component = "disp"))
  # })
  #
  # test_that("find_random", {
  #   expect_identical(find_random(m4), list(cond = "person", zi = "ID"))
  #   expect_identical(find_random(m4, component = "cond"), c(cond = "person"))
  #   expect_identical(find_random(m4, component = "zi"), c(zi = "ID"))
  #   expect_identical(find_random(m4, flatten = TRUE), c(cond = "person", zi = "ID"))
  #   expect_identical(find_random(m4, component = "cond", flatten = TRUE), c(cond = "person"))
  #   expect_identical(find_random(m4, component = "zi", flatten = TRUE), c(zi = "ID"))
  # })
  #
  # test_that("find_respone", {
  #   expect_identical(find_response(m4), "count")
  # })
  #
  # test_that("find_terms", {
  #   expect_identical(find_terms(m4), list(
  #     response = "count",
  #     predictors = c("child", "camper"),
  #     random = "persons",
  #     zi = c("child", "livebait"),
  #     zi_random = "ID",
  #     disp = "xb"
  #   ))
  #   expect_identical(find_terms(m, flatten = TRUE), c("count", "child", "camper", "persons", "livebait", "ID", "xb"))
  # })

}
