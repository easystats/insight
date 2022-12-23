if (requiet("testthat") && requiet("insight")) {
  data(mtcars)
  d <- mtcars
  m1 <- lm(mtcars$mpg ~ mtcars$hp * mtcars$cyl + poly(mtcars$drat, 2) / mtcars$disp)
  m2 <- lm(mtcars$mpg ~ d$hp * mtcars$cyl + poly(mtcars$drat, 2) / mtcars$disp)
  m3 <- lm(mpg ~ hp * cyl + poly(drat, 2) / disp, data = mtcars)

  test_that("find_formula-data1", {
    expect_warning(find_formula(m1))
  })

  test_that("find_formula-data2", {
    expect_error(find_formula(m2))
  })

  test_that("find_formula-data3", {
    expect_equal(
      find_formula(m3),
      structure(list(conditional = mpg ~ hp * cyl + poly(drat, 2) / disp),
        class = c("insight_formula", "list")
      ),
      ignore_attr = TRUE
    )
  })
}
