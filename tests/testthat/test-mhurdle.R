requiet("mhurdle")
data("Interview", package = "mhurdle")
m1 <- mhurdle(shows ~ 0 | linc + smsa + age + educ + size, data = Interview, h2 = TRUE, dist = "n", method = "bhhh")
m2 <- mhurdle(shows ~ educ + size | linc | smsa + age, data = Interview,
    h2 = FALSE, method = "bhhh", corr = TRUE, finalHessian = TRUE)

test_that("get_data", {
  d1 <- get_data(m1)
  d2 <- get_data(m2)
  expect_s3_class(d1, "data.frame")
  expect_s3_class(d2, "data.frame")
  expect_equal(dim(d1), c(1000, 6))
  expect_equal(dim(d2), c(1000, 6))
})
