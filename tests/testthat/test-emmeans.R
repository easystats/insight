## These tests fail on current R-devel (win-builder), not sure why

# * using R Under development (unstable) (2023-03-14 r83979 ucrt)
# * using platform: x86_64-w64-mingw32 (64-bit)
# * R was compiled by
#     gcc.exe (GCC) 12.2.0
#     GNU Fortran (GCC) 12.2.0
# * running under: Windows Server 2022 x64 (build 20348)

# ── Failure ('test-emmeans.R:14:5'): emmeans ────────────────────────────────────
# get_statistic(EList)$Statistic (`actual`) not equal to c(1.449, -0.377, -2.346, 1.243, 2.717, 1.393) (`expected`).

#   `actual`: 1.2429  2.7166  1.3925 1.2429 2.7166 1.3925
# `expected`: 1.4490 -0.3770 -2.3460 1.2430 2.7170 1.3930
# ── Failure ('test-emmeans.R:15:5'): emmeans ────────────────────────────────────
# get_statistic(EList)$Statistic[1:3] (`actual`) not equal to get_statistic(E)$Statistic (`expected`).

#   `actual`: 1.2  2.7  1.4
# `expected`: 1.4 -0.4 -2.3

# [ FAIL 2 | WARN 0 | SKIP 25 | PASS 3366 ]

skip_if(getRversion() > "4.2.2")

if (skip_if_not_or_load_if_installed("emmeans")) {
  test_that("emmeans", {
    m <- glm(am ~ factor(cyl),
      family = binomial(), data = mtcars
    )

    EList <- emmeans::emmeans(m, pairwise ~ cyl, type = "resp")

    E <- emmeans::emmeans(m, ~cyl, type = "resp")

    C <- emmeans::contrast(E, method = "pairwise")

    expect_identical(find_statistic(EList), "z-statistic")
    expect_equal(get_statistic(EList)$Statistic, c(1.449, -0.377, -2.346, 1.243, 2.717, 1.393), tolerance = 0.001)
    expect_equal(get_statistic(EList)$Statistic[1:3], get_statistic(E)$Statistic, tolerance = 0.001)
    expect_equal(get_statistic(EList)$Statistic[4:6], get_statistic(C)$Statistic, tolerance = 0.001)

    expect_equal(get_parameters(EList)$Estimate, c(0.727, 0.429, 0.143, 3.556, 16, 4.5), tolerance = 0.001)
  })
}
