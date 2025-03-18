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
  expect_snapshot(print(out, ci_digits = 2, table_width = 50), variant = "windows")

  data(iris)
  lm1 <- lm(Sepal.Length ~ Species, data = iris)
  lm2 <- lm(Sepal.Length ~ Species + Petal.Length, data = iris)
  lm3 <- lm(Sepal.Length ~ Species * Petal.Length, data = iris)
  lm6 <- lm5 <- lm4 <- lm(Sepal.Length ~ Species * Petal.Length + Petal.Width, data = iris)

  tab <- parameters::compare_parameters(lm1, lm2, lm3, lm4, lm5, lm6)
  expect_snapshot(print(tab, ci_digits = 2, table_width = 80), variant = "windows")
})


test_that("export_table, table_width, no split", {
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
  expect_snapshot(print(out, ci_digits = 2, table_width = Inf), variant = "windows")

  data(iris)
  lm1 <- lm(Sepal.Length ~ Species, data = iris)
  lm2 <- lm(Sepal.Length ~ Species + Petal.Length, data = iris)
  lm3 <- lm(Sepal.Length ~ Species * Petal.Length, data = iris)
  lm6 <- lm5 <- lm4 <- lm(Sepal.Length ~ Species * Petal.Length + Petal.Width, data = iris)

  tab <- parameters::compare_parameters(lm1, lm2, lm3, lm4, lm5, lm6)
  expect_snapshot(print(tab, table_width = NULL), variant = "windows")
})


test_that("export_table, table_width, remove duplicated empty lines", {
  skip_if_not_installed("datawizard")
  data(efc, package = "datawizard")
  out <- datawizard::data_codebook(efc)
  out$.row_id <- NULL
  expect_snapshot(print(export_table(out, table_width = 60, remove_duplicates = FALSE)))
  expect_snapshot(print(export_table(out, table_width = 60, empty_line = "-", remove_duplicates = FALSE)))
  expect_snapshot(print(export_table(out, table_width = 60, empty_line = "-", sep = " | ", remove_duplicates = FALSE)))
  expect_snapshot(print(export_table(out, table_width = 60, empty_line = "-", cross = "+", remove_duplicates = FALSE)))
  # don't remove duplicates
  expect_snapshot(print(export_table(out, table_width = 60, remove_duplicates = TRUE)))
  expect_snapshot(print(export_table(out, table_width = 60, empty_line = "-", remove_duplicates = TRUE)))
  expect_snapshot(print(export_table(out, table_width = 60, empty_line = "-", sep = " | ", remove_duplicates = TRUE)))
  expect_snapshot(print(export_table(out, table_width = 60, empty_line = "-", cross = "+", remove_duplicates = TRUE)))

  data(efc_insight, package = "insight")
  out <- datawizard::data_codebook(efc_insight[, 1:4])
  out$.row_id <- NULL
  expect_snapshot(print(export_table(out, table_width = 60, remove_duplicates = TRUE, empty_line = "-", cross = "+")))
  expect_snapshot(print(export_table(out, table_width = 60, remove_duplicates = FALSE, empty_line = "-", cross = "+")))
  out <- datawizard::data_codebook(efc_insight[, 1:3])
  out$.row_id <- NULL
  expect_snapshot(print(export_table(out, table_width = 60, remove_duplicates = TRUE, empty_line = "-", cross = "+")))
  expect_snapshot(print(export_table(out, table_width = 60, remove_duplicates = FALSE, empty_line = "-", cross = "+")))
})


test_that("export_table, overlengthy lines", {
  data(iris)
  d <- iris
  colnames(d)[1] <- paste0(letters[1:26], "no1", collapse = "_")
  colnames(d)[2] <- paste0(letters[1:26], "no2", collapse = "_")
  expect_warning(export_table(d[1:10, ]), regex = "The table contains")
  expect_snapshot(print(export_table(d[1:10, ], verbose = FALSE)))

  d <- iris
  colnames(d)[2] <- paste0(letters[1:26], "no1", collapse = "_")
  colnames(d)[5] <- paste0(letters[1:26], "no2", collapse = "_")
  expect_warning(export_table(d[1:10, ]), regex = "The table contains")
  expect_snapshot(print(export_table(d[1:10, ], verbose = FALSE)))
})


test_that("export_table, gt, simple", {
  skip_if_not_installed("gt")
  skip_on_cran()
  d <- data.frame(a = c(1.3, 2, 543), b = c("ab", "cd", "abcde"), stringsAsFactors = FALSE)
  attr(d, "table_caption") <- "Table Title"
  set.seed(123)
  out <- gt::as_raw_html(export_table(d, format = "html"))
  expect_snapshot(as.character(out))
  set.seed(123)
  out <- gt::as_raw_html(export_table(d, format = "html", align = "rl"))
  expect_snapshot(as.character(out))

  d <- data.frame(
    a = c(1.3, 2, 543, 78),
    b = c("ab", "cd", "abcde", "hj"),
    g = c("g1", "g1", "g2", "g2"),
    stringsAsFactors = FALSE
  )
  set.seed(123)
  out <- gt::as_raw_html(export_table(d, format = "html", by = "g"))
  expect_snapshot(as.character(out))
  set.seed(123)
  out <- gt::as_raw_html(export_table(d, format = "html", align = "rl", by = "g"))
  expect_snapshot(as.character(out))
})


test_that("export_table, gt, complex with group indention", {
  skip_if_not_installed("gt")
  skip_if_not_installed("parameters")
  skip_on_cran()
  data(iris)

  lm1 <- lm(Sepal.Length ~ Species + Petal.Length, data = iris)
  lm2 <- lm(Sepal.Width ~ Species * Petal.Length, data = iris)

  cp <- parameters::compare_parameters(lm1, lm2, drop = "^\\(Intercept")

  set.seed(123)
  out <- gt::as_raw_html(print_html(cp,
    select = "{estimate}{stars}|({se})",
    groups = list(
      Species = c(
        "Species [versicolor]",
        "Species [virginica]"
      ),
      Interactions = c(
        "Species [versicolor] × Petal Length", # note the unicode char!
        "Species [virginica] × Petal Length"
      ),
      Controls = "Petal Length"
    )
  ))
  expect_snapshot(as.character(out))
})
