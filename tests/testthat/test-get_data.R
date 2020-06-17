if (require("testthat") && require("insight") && require("lme4")) {
  data("cbpp")
  set.seed(123)
  cbpp$cont <- rnorm(nrow(cbpp))
  m <- glmer(cbind(incidence, size - incidence) ~ poly(cont,2) + (1 | herd),
             data = cbpp, family = binomial)

  test_that("get_data", {
    expect_s3_class(get_data(m), "data.frame")
  })
}
