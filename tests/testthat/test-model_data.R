if (require("testthat") && require("insight") && require("splines") && require("glmmTMB")) {
  context("insight, find_data")

  data(efc)

  m1 <- lm(neg_c_7 ~ e42dep + ns(c160age), data = efc)
  m2 <- lm(neg_c_7 ~ e42dep + ns(c160age, knots = 2), data = efc)
  m3 <- lm(neg_c_7 ~ e42dep + bs(c160age, degree = 3), data = efc)
  m4 <- lm(neg_c_7 ~ e42dep + bs(c160age, degree = 1), data = efc)

  m5 <- lm(neg_c_7 ~ e42dep + c160age, data = efc)


  test_that("find_data", {
    mf1 <- find_data(m1)
    mf2 <- find_data(m2)
    mf3 <- find_data(m3)
    mf4 <- find_data(m4)
    mf5 <- model.frame(m5)

    expect_equal(as.vector(mf1$c160age), as.vector(mf5$c160age))
    expect_equal(as.vector(mf2$c160age), as.vector(mf5$c160age))
    expect_equal(as.vector(mf3$c160age), as.vector(mf5$c160age))
    expect_equal(as.vector(mf4$c160age), as.vector(mf5$c160age))
  })

  data("Salamanders")
  m <- glmmTMB(
    count ~ spp + cover + mined + poly(DOP, 3) + (1 | site),
    ziformula = ~spp + mined,
    dispformula = ~DOY,
    data = Salamanders,
    family = nbinom2
  )

  test_that("find_data", {
    mf <- find_data(m)
    expect_equal(ncol(mf), 7)
    expect_equal(colnames(mf), c("count", "spp", "cover", "mined", "site", "DOY", "DOP"))
  })
}

