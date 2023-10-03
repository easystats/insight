# see https://github.com/georgheinze/logistf/pull/54
skip_if(
  "as.character.formula" %in% methods(as.character),
  "Some package uses `formula.tools::as.character.formula()` which breaks `find_formula()`."
)

d <- mtcars
m1 <- lm(mtcars$mpg ~ mtcars$hp * mtcars$cyl + poly(mtcars$drat, 2) / mtcars$disp)
m2 <- lm(mtcars$mpg ~ d$hp * mtcars$cyl + poly(mtcars$drat, 2) / mtcars$disp)
m3 <- lm(mpg ~ hp * cyl + poly(drat, 2) / disp, data = mtcars)
m4 <- lm(mpg ~ hp * cyl + poly(drat, 2, raw = T) / disp, data = mtcars)

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

test_that("find_formula-raw = T", {
  expect_warning(find_formula(m4), regex = "Looks")
  expect_silent(find_formula(m4, verbose = FALSE))
})
