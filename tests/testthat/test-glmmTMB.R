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


}
