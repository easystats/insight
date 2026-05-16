skip_on_cran()
skip_on_os("mac")
skip_if_not_installed("mice")
skip_if_not_installed("lme4")
skip_if_not_installed("cobalt")

test_that("get_data, mira", {
  data("lalonde_mis", package = "cobalt")
  imp <- mice::mice(lalonde_mis, m = 3, print = FALSE, seed = 1124)

  fits <- with(imp, lm(re78 ~ poly(re74, 2) + treat))
  out <- get_data(fits)
  expect_length(out, 3L)
  expect_named(out[[1]], c("re78", "re74", "treat"))
  expect_identical(vapply(out, nrow, integer(1)), c(614L, 614L, 614L))

  fits <- with(imp, lme4::lmer(re78 ~ poly(re74, 2) + treat + (1 | educ)))
  out <- get_data(fits)
  expect_length(out, 3L)
  expect_named(out[[1]], c("re78", "re74", "treat", "educ"))
  expect_identical(vapply(out, nrow, integer(1)), c(614L, 614L, 614L))
})
