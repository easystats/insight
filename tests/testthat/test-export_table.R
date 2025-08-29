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
  skip_if_not_installed("parameters", minimum_version = "0.27.0.1")
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
        "Species (versicolor)",
        "Species (virginica)"
      ),
      Interactions = c(
        "Species (versicolor) × Petal Length", # note the unicode char!
        "Species (virginica) × Petal Length"
      ),
      Controls = "Petal Length"
    )
  ))
  expect_snapshot(as.character(out))
})

test_that("export_table, new column names", {
  data(iris)
  x <- as.data.frame(iris[1:5, ])
  out <- export_table(x, column_names = letters[1:5])
  expect_identical(
    strsplit(out, "\n")[[1]][1],
    "   a |    b |    c |    d |      e"
  )
  out <- export_table(x, column_names = c(Species = "a"))
  expect_identical(
    strsplit(out, "\n")[[1]][1],
    "Sepal.Length | Sepal.Width | Petal.Length | Petal.Width |      a"
  )

  # errors
  expect_error(
    export_table(x, column_names = letters[1:4]),
    regex = "Number of names"
  )
  expect_error(
    export_table(x, column_names = c(Species = "a", abc = "b")),
    regex = "Not all names"
  )
  expect_error(
    export_table(x, column_names = c(Species = "a", "b")),
    regex = "is a named vector"
  )
})


test_that("export_table, by in text format", {
  data(mtcars)
  data(iris)

  expect_snapshot(export_table(mtcars, by = c("cyl", "gear")))
  expect_snapshot(export_table(iris, by = "Species"))
  expect_snapshot(export_table(mtcars, by = ~ cyl + gear))

  # errors
  expect_error(
    export_table(iris, by = "Specis"),
    regex = "Not all variables"
  )
  expect_error(
    export_table(iris, by = 6),
    regex = "cannot be lower"
  )
})


test_that("export_table, tinytable with indented rows", {
  skip_on_cran()
  skip_if_not_installed("parameters")
  skip_if_not_installed("tinytable")
  skip_if_not_installed("knitr")

  data(mtcars)
  mtcars$cyl <- as.factor(mtcars$cyl)
  mtcars$gear <- as.factor(mtcars$gear)
  model <- lm(mpg ~ hp + gear * vs + cyl + drat, data = mtcars)

  # don't select "Intercept" parameter
  mp <- as.data.frame(format(parameters::model_parameters(model, drop = "^\\(Intercept")))

  groups <- list(
    Engine = c("cyl [6]", "cyl [8]", "vs", "hp"),
    Interactions = c(8, 9),
    Controls = c(2, 3, 7)
  )
  expect_snapshot(export_table(mp, format = "tt", row_groups = groups, table_width = Inf))
  expect_snapshot(export_table(
    mp,
    format = "text",
    row_groups = groups,
    table_width = Inf
  ))
  expect_snapshot(export_table(
    mp,
    format = "markdown",
    row_groups = groups,
    table_width = Inf
  ))
  expect_snapshot(export_table(
    mp,
    format = "text",
    row_groups = groups,
    table_width = Inf,
    align = "llrrlr"
  ))
  expect_snapshot(export_table(
    mp,
    format = "markdown",
    row_groups = groups,
    table_width = Inf,
    align = "llrrlr"
  ))

  attr(mp, "indent_rows") <- list(
    Engine = c("cyl [6]", "cyl [8]", "vs", "hp"),
    Interactions = c(8, 9),
    Controls = c(2, 3, 7)
  )
  expect_snapshot(export_table(mp, format = "tt", table_width = Inf))

  mp <- as.data.frame(format(parameters::model_parameters(model, drop = "^\\(Intercept")))
  mp$groups <- c(
    "Engine", "Controls", "Controls", "Engine", "Engine", "Engine", "Controls",
    "Interactions", "Interactions"
  )
  expect_snapshot(export_table(mp, format = "tt", by = "groups", table_width = Inf))
})


test_that("export_table, removing captions work", {
  skip_on_cran()
  skip_if_not_installed("modelbased", minimum_version = "0.13.0")
  skip_if_not_installed("marginaleffects", minimum_version = "0.29.0")

  data(iris)
  mod <- lm(Petal.Length ~ Species, data = iris)
  means <- modelbased::estimate_means(mod, by = "Species", type = "response")

  expect_snapshot(print(means, table_width = Inf))
  expect_snapshot(print(means, title = "", table_width = Inf))
  expect_snapshot(print(means, caption = "", table_width = Inf))
  expect_snapshot(print(means, footer = "", caption = "", table_width = Inf))

  skip_if_not_installed("gt")
  set.seed(123)
  out <- gt::as_raw_html(print_html(means))
  expect_snapshot(as.character(out))
  out <- gt::as_raw_html(print_html(means, footer = "", caption = ""))
  expect_snapshot(as.character(out))
})
