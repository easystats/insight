if (require("testthat") && require("insight")) {
  data(iris)
  x1 <- color_if(iris[1:4, ], columns = "Sepal.Length", predicate = `>`, value = 5)
  x2 <- colour_if(iris[1:4, ], columns = "Sepal.Length", predicate = `>`, value = 5)

  test_that("color_if", {
    expect_equal(
      x1$Sepal.Length,
      c("\033[32m        5.10\033[39m", "\033[31m        4.90\033[39m",  "\033[31m        4.70\033[39m", "\033[31m        4.60\033[39m"),
    )
    expect_equal(
      x2$Sepal.Length,
      c("\033[32m        5.10\033[39m", "\033[31m        4.90\033[39m",  "\033[31m        4.70\033[39m", "\033[31m        4.60\033[39m"),
    )
  })
}
