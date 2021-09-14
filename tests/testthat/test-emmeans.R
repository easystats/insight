if (require(emmeans) && require(insight) && require(testthat)) {
  test_that("emmeans", {
    m <- glm(am ~ factor(cyl),
             family = binomial(), data = mtcars)

    EList <- emmeans::emmeans(m, pairwise ~ cyl, type = "resp")

    E <- emmeans::emmeans(m, ~ cyl, type = "resp")

    C <- emmeans::contrast(E, method = "pairwise")

    expect_equal(find_statistic(EList), "z-statistic")
    expect_equal(get_statistic(EList)$Statistic, c(1.449, -0.377, -2.346, 1.243, 2.717, 1.393), tolerance = 0.001)
    expect_equal(get_statistic(EList)$Statistic[1:3], get_statistic(E)$Statistic, tolerance = 0.001)
    expect_equal(get_statistic(EList)$Statistic[4:6], get_statistic(C)$Statistic, tolerance = 0.001)

    expect_equal(get_parameters(EList)$Estimate, c(0.727, 0.429, 0.143, 3.556, 16, 4.5), tolerance = 0.001)
  })
}
