if (require("testthat") && require("insight") && require("glmmTMB")) {
  context("insight, model_family")

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


  test_that("model_family", {
    expect_true(model_family(m1)$is_zeroinf)
    expect_false(model_family(m2)$is_zeroinf)
    expect_true(model_family(m3)$is_linear)
  })

  test_that("pred_vars", {
    expect_identical(pred_vars(m1), c("child", "camper", "persons"))
    expect_identical(pred_vars(m2), c("child", "camper", "persons"))
    expect_identical(pred_vars(m3), c("e42dep", "c161sex"))
  })

  test_that("resp_var", {
    expect_identical(resp_var(m1), "count")
    expect_identical(resp_var(m2), "count")
    expect_identical(resp_var(m3), "neg_c_7")
  })

  test_that("link_inverse", {
    expect_identical(link_inverse(m1)(.2), exp(.2))
    expect_identical(link_inverse(m2)(.2), exp(.2))
    expect_identical(link_inverse(m3)(.2), .2)
  })

  test_that("model_frame", {
    model_frame(m1)
    model_frame(m2)
    model_frame(m3)
  })
}
