if (require("testthat") && require("insight") && require("glmmTMB")) {
  context("insight, model_info")

  data(fish)

  m1 <- glmmTMB(
    count ~ child + camper + (1 | persons),
    ziformula = ~ child + camper + (1 | persons),
    data = fish,
    family = truncated_poisson()
  )

  m2 <- glmmTMB(
    count ~ child + camper + (1 | persons),
    data = fish,
    family = poisson()
  )

  m3 <- glmmTMB(
    count ~ child + camper + (1 | persons),
    ziformula = ~ child + livebait + (1 | persons),
    data = fish,
    family = truncated_poisson()
  )

  fish$ID = sample(1:4, nrow(fish), replace = TRUE)

  m4 <- glmmTMB(
    count ~ child + camper + (1 | persons),
    ziformula = ~ child + livebait + (1 | ID),
    dispformula = ~ xb,
    data = fish,
    family = truncated_poisson()
  )

  test_that("model_info", {
    expect_true(model_info(m1)$is_zeroinf)
    expect_false(model_info(m2)$is_zeroinf)
    expect_true(model_info(m3)$is_count)
    expect_true(model_info(m3)$is_pois)
    expect_false(model_info(m3)$is_negbin)
  })

  test_that("find_predictors", {
    expect_identical(find_predictors(m1, effects = "all"), c("child", "camper", "persons"))
    expect_identical(find_predictors(m1, effects = "random"), list(cond = "persons", zi = "persons"))
    expect_identical(find_predictors(m1, effects = "random", component = "cond"), c(cond = "persons"))
    expect_identical(find_predictors(m1), c("child", "camper"))
    expect_identical(find_predictors(m2, effects = "all"), c("child", "camper", "persons"))
    expect_identical(find_predictors(m2, effects = "random"), c(cond = "persons"))
    expect_identical(find_predictors(m2), c("child", "camper"))
  })

  test_that("find_response", {
    expect_identical(find_response(m1), "count")
    expect_identical(find_response(m2), "count")
  })

  test_that("link_inverse", {
    expect_identical(link_inverse(m1)(.2), exp(.2))
    expect_identical(link_inverse(m2)(.2), exp(.2))
  })

  test_that("get_data", {
    expect_equal(colnames(get_data(m1)), c("count", "child", "camper", "persons"))
    expect_equal(colnames(get_data(m1, effects = "all")), c("count", "child", "camper", "persons"))
    expect_equal(colnames(get_data(m1, effects = "random")), c("count", "child", "camper", "persons"))
    expect_equal(colnames(get_data(m2)), c("count", "child", "camper", "persons"))
    expect_equal(colnames(get_data(m2, effects = "all")), c("count", "child", "camper", "persons"))
    expect_equal(colnames(get_data(m2, effects = "random")), c("count", "child", "camper", "persons"))
    get_data(m3)
  })

  test_that("find_predictors", {
    expect_identical(find_predictors(m3, effects = "fixed", component = "cond"), c("child", "camper"))
    expect_identical(find_predictors(m3, effects = "fixed", component = "zi"), c("child", "livebait"))
    expect_identical(find_predictors(m3, effects = "all", component = "cond"), c("child", "camper", "persons"))
    expect_identical(find_predictors(m3, effects = "all", component = "zi"), c("child", "livebait", "persons"))
    expect_identical(find_predictors(m3, effects = "random", component = "cond"), c(cond = "persons"))
    expect_identical(find_predictors(m3, effects = "random", component = "zi"), c(zi = "persons"))

    expect_identical(find_predictors(m3, effects = "fixed", component = "all"), c("child", "camper", "livebait"))
    expect_identical(find_predictors(m3, effects = "all", component = "all"), c("child", "camper", "persons", "livebait"))
    expect_identical(find_predictors(m3, effects = "random", component = "all"), list(cond = "persons", zi = "persons"))
  })

  test_that("find_formula", {
    expect_length(find_formula(m4), 3)
    expect_identical(find_formula(m4, component = "cond"), stats::formula(m4, component = "cond"))
    expect_identical(find_formula(m4, component = "zi"), stats::formula(m4, component = "zi"))
    expect_identical(find_formula(m4, component = "disp"), stats::formula(m4, component = "disp"))
  })

  test_that("find_predictors", {
    expect_identical(find_predictors(m4), c("child", "camper", "livebait", "xb"))
    expect_identical(find_predictors(m4, effects = "random"), list(cond = "persons", zi = "ID"))
    expect_identical(find_predictors(m4, effects = "all"), c("child", "camper", "persons", "livebait", "ID", "xb"))
    expect_identical(find_predictors(m4, component = "cond"), c("child", "camper"))
    expect_identical(find_predictors(m4, effects = "random", component = "cond"), "persons")
    expect_identical(find_predictors(m4, effects = "all", component = "cond"), c("child", "camper", "persons"))
    expect_identical(find_predictors(m4, component = "zi"), c("child", "livebait"))
    expect_identical(find_predictors(m4, effects = "random", component = "zi"), "ID")
    expect_identical(find_predictors(m4, effects = "all", component = "zi"), c("child", "livebait", "ID"))
    expect_identical(find_predictors(m4, component = "disp"), "xb")
    expect_warning(find_predictors(m, effects = "random", component = "disp"))
    expect_identical(find_predictors(m4, effects = "all", component = "disp"), "xb")
  })

  test_that("find_random", {
    expect_identical(find_random(m4), list(cond = "person", zi = "ID"))
    expect_identical(find_random(m4, component = "cond"), c(cond = "person"))
    expect_identical(find_random(m4, component = "zi"), c(zi = "ID"))
    expect_identical(find_random(m4, flatten = TRUE), c(cond = "person", zi = "ID"))
    expect_identical(find_random(m4, component = "cond", flatten = TRUE), c(cond = "person"))
    expect_identical(find_random(m4, component = "zi", flatten = TRUE), c(zi = "ID"))
  })

  test_that("find_respone", {
    expect_identical(find_response(m4), "count")
  })

  test_that("find_terms", {
    expect_identical(find_terms(m4), list(
      response = "count",
      predictors = c("child", "camper"),
      random = "persons",
      zi = c("child", "livebait"),
      zi_random = "ID",
      disp = "xb"
    ))
    expect_identical(find_terms(m, flatten = TRUE), c("count", "child", "camper", "persons", "livebait", "ID", "xb"))
  })

}
