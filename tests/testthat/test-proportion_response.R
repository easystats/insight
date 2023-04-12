skip_if_not_installed("lme4")

m1 <- suppressMessages(suppressWarnings(lme4::glmer(
  vs / cyl ~ disp + (1 | cyl),
  data = mtcars,
  family = binomial(link = "logit")
)))

m2 <- suppressMessages(suppressWarnings(lme4::glmer(
  I(vs / cyl) ~ disp + (1 | cyl),
  data = mtcars,
  family = binomial(link = "logit")
)))

test_that("get_response", {
  expect_equal(head(get_response(m1, as_proportion = TRUE)), c(0, 0, 0.25, 0.16667, 0, 0.16667), tolerance = 1e-2)
  expect_equal(head(get_response(m1, as_proportion = FALSE)), head(mtcars[, c("vs", "cyl")]), tolerance = 1e-2)
  expect_equal(get_response(m2), mtcars[, c("vs", "cyl")])
})

test_that("find_response", {
  expect_equal(find_response(m1), "vs/cyl")
  expect_equal(find_response(m2), "I(vs/cyl)")
  expect_equal(find_response(m1, combine = FALSE), c("vs", "cyl"))
  expect_equal(find_response(m2, combine = FALSE), c("vs", "cyl"))
})
