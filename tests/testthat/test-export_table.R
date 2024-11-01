d <- data.frame(a = c(1.3, 2, 543), b = c("ab", "cd", "abcde"), stringsAsFactors = FALSE)

test_that("export_table", {
  expect_snapshot(export_table(d))
})

test_that("export_table", {
  expect_snapshot(export_table(d, sep = " ", header = "*", digits = 1))
})


# snapshots have a very messy output for format = "md"

test_that("export_table", {
  out <- export_table(d, format = "md")
  expect_equal(out, structure(
    c(
      "|      a|     b|", "|------:|-----:|",
      "|   1.30|    ab|", "|   2.00|    cd|",
      "| 543.00| abcde|"
    ),
    format = "pipe",
    class = c("knitr_kable", "character")
  ), ignore_attr = TRUE)
})

d <- data.frame(a = c(1.3, 2, 543), b = c("ab", "cd", "abcde"), stringsAsFactors = FALSE)
attr(d, "table_caption") <- "Table Title"

test_that("export_table", {
  out <- export_table(d, format = "md")
  expect_equal(
    out,
    structure(
      c(
        "Table: Table Title", "", "|      a|     b|", "|------:|-----:|",
        "|   1.30|    ab|", "|   2.00|    cd|", "| 543.00| abcde|"
      ),
      format = "pipe",
      class = c("knitr_kable", "character")
    ),
    ignore_attr = TRUE
  )
})

d <- data.frame(a = c(1.3, 2, 543), b = c("ab", "cd", "abcde"), stringsAsFactors = FALSE)
attr(d, "table_title") <- "Table Title"

test_that("export_table", {
  out <- export_table(d, format = "md")
  expect_equal(
    out,
    structure(
      c(
        "Table: Table Title", "", "|      a|     b|", "|------:|-----:|",
        "|   1.30|    ab|", "|   2.00|    cd|", "| 543.00| abcde|"
      ),
      format = "pipe",
      class = c("knitr_kable", "character")
    ),
    ignore_attr = TRUE
  )
})

d <- data.frame(a = c(1.3, 2, 543), b = c("ab", "cd", "abcde"), stringsAsFactors = FALSE)

test_that("export_table", {
  out <- export_table(d, format = "md", title = "Table Title")
  expect_equal(
    out,
    structure(
      c(
        "Table: Table Title", "", "|      a|     b|", "|------:|-----:|",
        "|   1.30|    ab|", "|   2.00|    cd|", "| 543.00| abcde|"
      ),
      format = "pipe",
      class = c("knitr_kable", "character")
    ),
    ignore_attr = TRUE
  )
})

d <- data.frame(a = c(1.3, 2, 543), b = c("ab", "cd", "abcde"), stringsAsFactors = FALSE)
attr(d, "table_caption") <- "Table Title"
attr(d, "table_footer") <- list("first", "second", "third")

test_that("export_table", {
  out <- export_table(d, format = "md")
  expect_equal(
    out,
    structure(
      c(
        "Table: Table Title", "", "|      a|     b|", "|------:|-----:|",
        "|   1.30|    ab|", "|   2.00|    cd|", "| 543.00| abcde|",
        "first", "second", "third"
      ),
      format = "pipe",
      class = c("knitr_kable", "character")
    ),
    ignore_attr = TRUE
  )
})


test_that("export_table, table_width", {
  skip_on_cran()
  skip_if_not_installed("lavaan")
  skip_if_not_installed("performance")
  skip_if_not_installed("parameters")

  data(HolzingerSwineford1939, package = "lavaan")
  structure <- " visual  =~ x1 + x2 + x3
                 textual =~ x4 + x5 + x6
                 speed   =~ x7 + x8 + x9 "
  model1 <- lavaan::cfa(structure, data = HolzingerSwineford1939)
  model2 <- lavaan::cfa(structure, data = HolzingerSwineford1939)

  out <- performance::compare_performance(model1, model2)
  expect_snapshot(print(out, table_width = 50), variant = "windows")

  data(iris)
  lm1 <- lm(Sepal.Length ~ Species, data = iris)
  lm2 <- lm(Sepal.Length ~ Species + Petal.Length, data = iris)
  lm3 <- lm(Sepal.Length ~ Species * Petal.Length, data = iris)
  lm6 <- lm5 <- lm4 <- lm(Sepal.Length ~ Species * Petal.Length + Petal.Width, data = iris)

  tab <- parameters::compare_parameters(lm1, lm2, lm3, lm4, lm5, lm6)
  expect_snapshot(print(tab, table_width = 80), variant = "windows")
})
