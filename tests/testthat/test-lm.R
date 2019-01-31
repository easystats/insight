if (require("testthat") && require("insight")) {
  context("insight, model_info")

  data(efc)

  m1 <- lm(neg_c_7 ~ e42dep + c161sex, data = efc)

  test_that("model_info", {
    expect_true(model_info(m1)$is_linear)
  })

  test_that("find_predictors", {
    expect_identical(find_predictors(m1), c("e42dep", "c161sex"))
    expect_identical(find_predictors(m1, effects = "random"), vector(mode = "character"))
  })

  test_that("find_response", {
    expect_identical(find_response(m1), "neg_c_7")
  })

  test_that("link_inverse", {
    expect_identical(link_inverse(m1)(.2), .2)
  })

  test_that("get_data", {
    get_data(m1)
  })
}
