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
    expect_identical(find_predictors(m1, effects = "random"), list(conditional = "persons", zero_inflated = "persons"))
    expect_identical(find_predictors(m1, effects = "random", component = "conditional"), c(conditional = "persons"))
    expect_identical(find_predictors(m1), c("child", "camper"))
    expect_identical(find_predictors(m2, effects = "all"), c("child", "camper", "persons"))
    expect_identical(find_predictors(m2, effects = "random"), c(conditional = "persons"))
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
    expect_equal(colnames(get_data(m1, effects = "random")), "persons")
    expect_equal(colnames(get_data(m2)), c("count", "child", "camper", "persons"))
    expect_equal(colnames(get_data(m2, effects = "all")), c("count", "child", "camper", "persons"))
    expect_equal(colnames(get_data(m2, effects = "random")), "persons")
    get_data(m3)
  })

  test_that("find_predictors", {
    expect_identical(find_predictors(m3, effects = "fixed", component = "conditional"), c("child", "camper"))
    expect_identical(find_predictors(m3, effects = "fixed", component = "zero_inflated"), c("child", "livebait"))
    expect_identical(find_predictors(m3, effects = "all", component = "conditional"), c("child", "camper", "persons"))
    expect_identical(find_predictors(m3, effects = "all", component = "zero_inflated"), c("child", "livebait", "persons"))
    expect_identical(find_predictors(m3, effects = "random", component = "conditional"), c(conditional = "persons"))
    expect_identical(find_predictors(m3, effects = "random", component = "zero_inflated"), c(zero_inflated = "persons"))

    expect_identical(find_predictors(m3, effects = "fixed", component = "all"), c("child", "camper", "livebait"))
    expect_identical(find_predictors(m3, effects = "all", component = "all"), c("child", "camper", "persons", "livebait"))
    expect_identical(find_predictors(m3, effects = "random", component = "all"), list(conditional = "persons", zero_inflated = "persons"))
  })

  test_that("find_formula", {
    expect_length(find_formula(m4), 3)
    expect_identical(find_formula(m4, component = "conditional"), stats::formula(m4, component = "cond"))
    expect_identical(find_formula(m4, component = "zero_inflated"), stats::formula(m4, component = "zi"))
    expect_identical(find_formula(m4, component = "dispersion"), stats::formula(m4, component = "disp"))
  })

  test_that("find_predictors", {
    expect_identical(find_predictors(m4), c("child", "camper", "livebait", "xb"))
    expect_identical(find_predictors(m4, effects = "random"), list(conditional = "persons", zero_inflated = "ID"))
    expect_identical(find_predictors(m4, effects = "all"), c("child", "camper", "persons", "livebait", "ID", "xb"))
    expect_identical(find_predictors(m4, component = "conditional"), c("child", "camper"))
    expect_identical(find_predictors(m4, effects = "random", component = "conditional"), c(conditional = "persons"))
    expect_identical(find_predictors(m4, effects = "all", component = "conditional"), c("child", "camper", "persons"))
    expect_identical(find_predictors(m4, component = "zero_inflated"), c("child", "livebait"))
    expect_identical(find_predictors(m4, effects = "random", component = "zero_inflated"), c(zero_inflated = "ID"))
    expect_identical(find_predictors(m4, effects = "all", component = "zero_inflated"), c("child", "livebait", "ID"))
    expect_identical(find_predictors(m4, component = "dispersion"), "xb")
    expect_warning(find_predictors(m4, effects = "random", component = "dispersion"))
    expect_identical(find_predictors(m4, effects = "all", component = "dispersion"), "xb")
  })

  test_that("find_random", {
    expect_identical(find_random(m4), list(conditional = "persons", zero_inflated = "ID"))
    expect_identical(find_random(m4, component = "conditional"), c(conditional = "persons"))
    expect_identical(find_random(m4, component = "zero_inflated"), c(zero_inflated = "ID"))
    expect_identical(find_random(m4, flatten = TRUE), c(conditional = "persons", zero_inflated = "ID"))
    expect_identical(find_random(m4, component = "conditional", flatten = TRUE), c(conditional = "persons"))
    expect_identical(find_random(m4, component = "zero_inflated", flatten = TRUE), c(zero_inflated = "ID"))
  })

  test_that("find_respone", {
    expect_identical(find_response(m4), "count")
  })

  test_that("find_terms", {
    expect_identical(find_terms(m4), list(
      response = "count",
      conditional = c("child", "camper"),
      random = "persons",
      zero_inflated = c("child", "livebait"),
      zero_inflated_random = "ID",
      dispersion = "xb"
    ))
    expect_identical(find_terms(m4, flatten = TRUE), c("count", "child", "camper", "persons", "livebait", "ID", "xb"))
  })

  test_that("get_response", {
    expect_identical(get_response(m4), fish$count)
  })

  test_that("get_predictors", {
    expect_identical(colnames(get_predictors(m4)), c("child", "camper", "livebait", "xb"))
  })

  test_that("get_random", {
    expect_identical(colnames(get_random(m4)), c("persons", "ID"))
  })

  test_that("get_data", {
    expect_identical(colnames(get_data(m4)), c("count", "child", "camper", "livebait", "xb", "persons", "ID"))
    expect_identical(colnames(get_data(m4, effects = "fixed")), c("count", "child", "camper", "livebait", "xb"))
    expect_identical(colnames(get_data(m4, effects = "random")), c("persons", "ID"))
    expect_identical(colnames(get_data(m4, component = "zi")), c("count", "child", "livebait", "ID"))
    expect_identical(colnames(get_data(m4, component = "zi", effects = "fixed")), c("count", "child", "livebait"))
    expect_identical(colnames(get_data(m4, component = "zi", effects = "random")), "ID")
    expect_identical(colnames(get_data(m4, component = "cond")), c("count", "child", "camper", "persons"))
    expect_identical(colnames(get_data(m4, component = "cond", effects = "fixed")), c("count", "child", "camper"))
    expect_identical(colnames(get_data(m4, component = "cond", effects = "random")), "persons")
    expect_identical(colnames(get_data(m4, component = "disp")), c("count", "xb"))
    expect_identical(colnames(get_data(m4, component = "disp", effects = "fixed")), c("count", "xb"))
    expect_identical(colnames(get_data(m4, component = "disp", effects = "random")), vector(mode = "character"))
  })
}
