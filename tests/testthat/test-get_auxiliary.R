if (require("testthat") && require("insight") && require("MASS")) {
  data(quine)
  clotting <- data.frame(
    u = c(5, 10, 15, 20, 30, 40, 60, 80, 100),
    lot1 = c(118, 58, 42, 35, 27, 25, 21, 19, 18),
    lot2 = c(69, 35, 26, 21, 18, 16, 13, 12, 12)
  )
  set.seed(123)
  m1 <- glm(lot1 ~ log(u), data = clotting, family = Gamma())

  d <- data.frame(
    counts = c(18, 17, 15, 20, 10, 20, 25, 13, 12),
    outcome = gl(3, 1, 9),
    treatment = gl(3, 3)
  )
  set.seed(123)
  m2 <- glm(counts ~ outcome + treatment, data = d, family = poisson())
  m3 <- glm.nb(Days ~ Sex / (Age + Eth * Lrn), data = quine)

  test_that("get_dispersion", {
    expect_equal(get_auxiliary(m1, type = "dispersion"), summary(m1)$dispersion, tolerance = 1e-3, ignore_attr = TRUE)
    expect_equal(get_auxiliary(m2, type = "dispersion"), 1)
    expect_equal(get_auxiliary(m3, type = "dispersion"), 1)
  })
}
