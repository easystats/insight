if (requiet("testthat") && requiet("insight")) {
  test_that("format_capitalize", {
    expect_equal(format_capitalize("hello"), "Hello")
    expect_equal(format_capitalize(c("hello", "world")), c("Hello", "World"))
    expect_equal(
      unique(format_capitalize(iris$Species)),
      c("Setosa", "Versicolor", "Virginica")
    )
  })
}
