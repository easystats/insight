skip_if_not_installed("mhurdle")

test_that("model_info", {
  data("Interview", package = "mhurdle")
  m <- mhurdle::mhurdle(
    shows ~ 0 | linc + smsa + age + educ + size,
    data = Interview,
    h2 = TRUE,
    dist = "n",
    method = "bhhh"
  )
  out <- find_formula(m)
  expect_equal(
    out,
    list(conditional = shows ~ linc + smsa + age + educ + size),
    ignore_attr = TRUE
  )
})
