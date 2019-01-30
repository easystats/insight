if (require("testthat") && require("insight") && require("glmmTMB")) {
  context("insight, model_info")

  data(fish)
  data(efc)

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

  m3 <- lm(neg_c_7 ~ e42dep + c161sex, data = efc)


  test_that("model_info", {
    expect_true(model_info(m1)$is_zeroinf)
    expect_false(model_info(m2)$is_zeroinf)
    expect_true(model_info(m3)$is_linear)
  })

  test_that("model_predictors", {
    expect_identical(model_predictors(m1, effects = "all"), c("child", "camper", "persons"))
    expect_identical(model_predictors(m1, effects = "random"), "persons")
    expect_identical(model_predictors(m1), c("child", "camper"))
    expect_identical(model_predictors(m2, effects = "all"), c("child", "camper", "persons"))
    expect_identical(model_predictors(m2, effects = "random"), "persons")
    expect_identical(model_predictors(m2), c("child", "camper"))
    expect_identical(model_predictors(m3), c("e42dep", "c161sex"))
    expect_identical(model_predictors(m3, effects = "random"), vector(mode = "character"))
  })

  test_that("model_response", {
    expect_identical(model_response(m1), "count")
    expect_identical(model_response(m2), "count")
    expect_identical(model_response(m3), "neg_c_7")
  })

  test_that("link_inverse", {
    expect_identical(link_inverse(m1)(.2), exp(.2))
    expect_identical(link_inverse(m2)(.2), exp(.2))
    expect_identical(link_inverse(m3)(.2), .2)
  })

  test_that("model_data", {
    expect_equal(colnames(model_data(m1)), c("count", "child", "camper", "persons"))
    expect_equal(colnames(model_data(m1, effects = "all")), c("count", "child", "camper", "persons"))
    expect_equal(colnames(model_data(m1, effects = "random")), c("count", "child", "camper", "persons"))
    expect_equal(colnames(model_data(m2)), c("count", "child", "camper", "persons"))
    expect_equal(colnames(model_data(m2, effects = "all")), c("count", "child", "camper", "persons"))
    expect_equal(colnames(model_data(m2, effects = "random")), c("count", "child", "camper", "persons"))
    model_data(m3)
  })
}
