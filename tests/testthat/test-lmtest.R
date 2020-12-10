if (require("testthat") &&
  require("insight") &&
  require("lmtest")) {
  data("Mandible", package = "lmtest")
  m <- lm(length ~ age, data = Mandible, subset = (age <= 28))
  ct1 <- coeftest(m)
  ct2 <- coeftest(m, df = Inf)

  test_that("find_statistic", {
    expect_equal(find_statistic(ct1), "t-statistic")
    expect_equal(find_statistic(ct2), "z-statistic")
  })
  test_that("get_statistic", {
    expect_equal(get_statistic(ct1)$Statistic, c(-12.24446, 37.16067), tolerance = 1e-3)
    expect_equal(get_statistic(ct2)$Statistic, c(-12.24446, 37.16067), tolerance = 1e-3)
  })
}
