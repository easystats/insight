d <- data.frame(a = c(1.3, 2, 543), b = c("ab", "cd", "abcde"), stringsAsFactors = FALSE)

test_that("export_table", {
  skip_on_cran()
  expect_equal(
    export_table(d),
    structure(
      "     a |     b\n--------------\n  1.30 |    ab\n  2.00 |    cd\n543.00 | abcde\n",
      class = c("insight_table", "character")
    ),
    ignore_attr = TRUE
  )
})

test_that("export_table", {
  skip_on_cran()
  expect_equal(
    export_table(d, sep = " ", header = "*", digits = 1),
    structure(
      "    a     b\n***********\n  1.3    ab\n  2.0    cd\n543.0 abcde\n",
      class = c("insight_table", "character")
    ),
    ignore_attr = TRUE
  )
})


# snapshots have a very messy output for format = "md"

test_that("export_table", {
  out <- export_table(d, format = "md")
  expect_equal(
    out,
    structure(
      c(
        "|      a|     b|",
        "|------:|-----:|",
        "|   1.30|    ab|",
        "|   2.00|    cd|",
        "| 543.00| abcde|"
      ),
      format = "pipe",
      class = c("knitr_kable", "character")
    ),
    ignore_attr = TRUE
  )
})

d <- data.frame(a = c(1.3, 2, 543), b = c("ab", "cd", "abcde"), stringsAsFactors = FALSE)
attr(d, "table_caption") <- "Table Title"

test_that("export_table", {
  out <- export_table(d, format = "md")
  expect_equal(
    out,
    structure(
      c(
        "Table: Table Title",
        "",
        "|      a|     b|",
        "|------:|-----:|",
        "|   1.30|    ab|",
        "|   2.00|    cd|",
        "| 543.00| abcde|"
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
        "Table: Table Title",
        "",
        "|      a|     b|",
        "|------:|-----:|",
        "|   1.30|    ab|",
        "|   2.00|    cd|",
        "| 543.00| abcde|"
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
        "Table: Table Title",
        "",
        "|      a|     b|",
        "|------:|-----:|",
        "|   1.30|    ab|",
        "|   2.00|    cd|",
        "| 543.00| abcde|"
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
        "Table: Table Title",
        "",
        "|      a|     b|",
        "|------:|-----:|",
        "|   1.30|    ab|",
        "|   2.00|    cd|",
        "| 543.00| abcde|",
        "first",
        "second",
        "third"
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
  expect_identical(
    capture.output(print(out, ci_digits = 2, table_width = 50)),
    c(
      "# Comparison of Model Performance Indices",
      "",
      "Name   |  Model | Chi2(24) | p (Chi2)",
      "-------------------------------------",
      "model1 | lavaan |   85.306 |   < .001",
      "model2 | lavaan |   85.306 |   < .001",
      "",
      "Name   | Baseline(36) | p (Baseline) |   GFI",
      "--------------------------------------------",
      "model1 |      918.852 |       < .001 | 0.943",
      "model2 |      918.852 |       < .001 | 0.943",
      "",
      "Name   |  AGFI |   NFI |  NNFI |   CFI | RMSEA",
      "----------------------------------------------",
      "model1 | 0.894 | 0.907 | 0.896 | 0.931 | 0.092",
      "model2 | 0.894 | 0.907 | 0.896 | 0.931 | 0.092",
      "",
      "Name   |    RMSEA  CI | p (RMSEA) |   RMR |  SRMR",
      "-------------------------------------------------",
      "model1 | [0.07, 0.11] |    < .001 | 0.082 | 0.065",
      "model2 | [0.07, 0.11] |    < .001 | 0.082 | 0.065",
      "",
      "Name   |   RFI |  PNFI |   IFI |   RNI",
      "--------------------------------------",
      "model1 | 0.861 | 0.605 | 0.931 | 0.931",
      "model2 | 0.861 | 0.605 | 0.931 | 0.931",
      "",
      "Name   | Loglikelihood |  AIC (weights)",
      "---------------------------------------",
      "model1 |     -3737.745 | 7517.5 (0.500)",
      "model2 |     -3737.745 | 7517.5 (0.500)",
      "",
      "Name   |  BIC (weights) | BIC_adjusted",
      "--------------------------------------",
      "model1 | 7595.3 (0.500) |     7528.739",
      "model2 | 7595.3 (0.500) |     7528.739"
    )
  )

  data(iris)
  lm1 <- lm(Sepal.Length ~ Species, data = iris)
  lm2 <- lm(Sepal.Length ~ Species + Petal.Length, data = iris)
  lm3 <- lm(Sepal.Length ~ Species * Petal.Length, data = iris)
  lm6 <- lm5 <- lm4 <- lm(
    Sepal.Length ~ Species * Petal.Length + Petal.Width,
    data = iris
  )

  tab <- parameters::compare_parameters(lm1, lm2, lm3, lm4, lm5, lm6)
  expect_identical(
    capture.output(print(tab, ci_digits = 2, table_width = 80)),
    c(
      "Parameter                           |               lm1 |                  lm2",
      "------------------------------------------------------------------------------",
      "(Intercept)                         | 5.01 (4.86, 5.15) |  3.68 ( 3.47,  3.89)",
      "Species [versicolor]                | 0.93 (0.73, 1.13) | -1.60 (-1.98, -1.22)",
      "Species [virginica]                 | 1.58 (1.38, 1.79) | -2.12 (-2.66, -1.58)",
      "Petal Length                        |                   |  0.90 ( 0.78,  1.03)",
      "Species [versicolor] × Petal Length |                   |                     ",
      "Species [virginica] × Petal Length  |                   |                     ",
      "Petal Width                         |                   |                     ",
      "------------------------------------------------------------------------------",
      "Observations                        |               150 |                  150",
      "",
      "Parameter                           |                  lm3",
      "----------------------------------------------------------",
      "(Intercept)                         |  4.21 ( 3.41,  5.02)",
      "Species [versicolor]                | -1.81 (-2.99, -0.62)",
      "Species [virginica]                 | -3.15 (-4.41, -1.90)",
      "Petal Length                        |  0.54 ( 0.00,  1.09)",
      "Species [versicolor] × Petal Length |  0.29 (-0.30,  0.87)",
      "Species [virginica] × Petal Length  |  0.45 (-0.12,  1.03)",
      "Petal Width                         |                     ",
      "----------------------------------------------------------",
      "Observations                        |                  150",
      "",
      "Parameter                           |                  lm4",
      "----------------------------------------------------------",
      "(Intercept)                         |  4.21 ( 3.41,  5.02)",
      "Species [versicolor]                | -1.80 (-2.99, -0.62)",
      "Species [virginica]                 | -3.19 (-4.50, -1.88)",
      "Petal Length                        |  0.54 (-0.02,  1.09)",
      "Species [versicolor] × Petal Length |  0.28 (-0.30,  0.87)",
      "Species [virginica] × Petal Length  |  0.45 (-0.12,  1.03)",
      "Petal Width                         |  0.03 (-0.28,  0.34)",
      "----------------------------------------------------------",
      "Observations                        |                  150",
      "",
      "Parameter                           |                  lm5 |                  lm6",
      "---------------------------------------------------------------------------------",
      "(Intercept)                         |  4.21 ( 3.41,  5.02) |  4.21 ( 3.41,  5.02)",
      "Species [versicolor]                | -1.80 (-2.99, -0.62) | -1.80 (-2.99, -0.62)",
      "Species [virginica]                 | -3.19 (-4.50, -1.88) | -3.19 (-4.50, -1.88)",
      "Petal Length                        |  0.54 (-0.02,  1.09) |  0.54 (-0.02,  1.09)",
      "Species [versicolor] × Petal Length |  0.28 (-0.30,  0.87) |  0.28 (-0.30,  0.87)",
      "Species [virginica] × Petal Length  |  0.45 (-0.12,  1.03) |  0.45 (-0.12,  1.03)",
      "Petal Width                         |  0.03 (-0.28,  0.34) |  0.03 (-0.28,  0.34)",
      "---------------------------------------------------------------------------------",
      "Observations                        |                  150 |                  150"
    )
  )
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
  expect_identical(
    capture.output(print(out, ci_digits = 2, table_width = Inf)),
    c(
      "# Comparison of Model Performance Indices",
      "",
      "Name   |  Model | Chi2(24) | p (Chi2) | Baseline(36) | p (Baseline) |   GFI |  AGFI |   NFI |  NNFI |   CFI | RMSEA |    RMSEA  CI | p (RMSEA) |   RMR |  SRMR |   RFI |  PNFI |   IFI |   RNI | Loglikelihood |  AIC (weights) |  BIC (weights) | BIC_adjusted",
      "---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------",
      "model1 | lavaan |   85.306 |   < .001 |      918.852 |       < .001 | 0.943 | 0.894 | 0.907 | 0.896 | 0.931 | 0.092 | [0.07, 0.11] |    < .001 | 0.082 | 0.065 | 0.861 | 0.605 | 0.931 | 0.931 |     -3737.745 | 7517.5 (0.500) | 7595.3 (0.500) |     7528.739",
      "model2 | lavaan |   85.306 |   < .001 |      918.852 |       < .001 | 0.943 | 0.894 | 0.907 | 0.896 | 0.931 | 0.092 | [0.07, 0.11] |    < .001 | 0.082 | 0.065 | 0.861 | 0.605 | 0.931 | 0.931 |     -3737.745 | 7517.5 (0.500) | 7595.3 (0.500) |     7528.739"
    )
  )

  data(iris)
  lm1 <- lm(Sepal.Length ~ Species, data = iris)
  lm2 <- lm(Sepal.Length ~ Species + Petal.Length, data = iris)
  lm3 <- lm(Sepal.Length ~ Species * Petal.Length, data = iris)
  lm6 <- lm5 <- lm4 <- lm(
    Sepal.Length ~ Species * Petal.Length + Petal.Width,
    data = iris
  )

  tab <- parameters::compare_parameters(lm1, lm2, lm3, lm4, lm5, lm6)
  expect_identical(
    capture.output(print(tab, table_width = NULL)),
    c(
      "Parameter                           |               lm1 |                  lm2 |                  lm3 |                  lm4 |                  lm5 |                  lm6",
      "--------------------------------------------------------------------------------------------------------------------------------------------------------------------------",
      "(Intercept)                         | 5.01 (4.86, 5.15) |  3.68 ( 3.47,  3.89) |  4.21 ( 3.41,  5.02) |  4.21 ( 3.41,  5.02) |  4.21 ( 3.41,  5.02) |  4.21 ( 3.41,  5.02)",
      "Species [versicolor]                | 0.93 (0.73, 1.13) | -1.60 (-1.98, -1.22) | -1.81 (-2.99, -0.62) | -1.80 (-2.99, -0.62) | -1.80 (-2.99, -0.62) | -1.80 (-2.99, -0.62)",
      "Species [virginica]                 | 1.58 (1.38, 1.79) | -2.12 (-2.66, -1.58) | -3.15 (-4.41, -1.90) | -3.19 (-4.50, -1.88) | -3.19 (-4.50, -1.88) | -3.19 (-4.50, -1.88)",
      "Petal Length                        |                   |  0.90 ( 0.78,  1.03) |  0.54 ( 0.00,  1.09) |  0.54 (-0.02,  1.09) |  0.54 (-0.02,  1.09) |  0.54 (-0.02,  1.09)",
      "Species [versicolor] × Petal Length |                   |                      |  0.29 (-0.30,  0.87) |  0.28 (-0.30,  0.87) |  0.28 (-0.30,  0.87) |  0.28 (-0.30,  0.87)",
      "Species [virginica] × Petal Length  |                   |                      |  0.45 (-0.12,  1.03) |  0.45 (-0.12,  1.03) |  0.45 (-0.12,  1.03) |  0.45 (-0.12,  1.03)",
      "Petal Width                         |                   |                      |                      |  0.03 (-0.28,  0.34) |  0.03 (-0.28,  0.34) |  0.03 (-0.28,  0.34)",
      "--------------------------------------------------------------------------------------------------------------------------------------------------------------------------",
      "Observations                        |               150 |                  150 |                  150 |                  150 |                  150 |                  150"
    )
  )
})


test_that("export_table, table_width, remove duplicated empty lines", {
  skip_if_not_installed("datawizard")
  data(efc, package = "datawizard")
  out <- datawizard::data_codebook(efc)
  out$.row_id <- NULL
  expect_snapshot(print(export_table(out, table_width = 60, remove_duplicates = FALSE)))
  expect_snapshot(print(export_table(
    out,
    table_width = 60,
    empty_line = "-",
    remove_duplicates = FALSE
  )))
  expect_snapshot(print(export_table(
    out,
    table_width = 60,
    empty_line = "-",
    sep = " | ",
    remove_duplicates = FALSE
  )))
  expect_snapshot(print(export_table(
    out,
    table_width = 60,
    empty_line = "-",
    cross = "+",
    remove_duplicates = FALSE
  )))
  # don't remove duplicates
  expect_snapshot(print(export_table(out, table_width = 60, remove_duplicates = TRUE)))
  expect_snapshot(print(export_table(
    out,
    table_width = 60,
    empty_line = "-",
    remove_duplicates = TRUE
  )))
  expect_snapshot(print(export_table(
    out,
    table_width = 60,
    empty_line = "-",
    sep = " | ",
    remove_duplicates = TRUE
  )))
  expect_snapshot(print(export_table(
    out,
    table_width = 60,
    empty_line = "-",
    cross = "+",
    remove_duplicates = TRUE
  )))

  data(efc_insight, package = "insight")
  out <- datawizard::data_codebook(efc_insight[, 1:4])
  out$.row_id <- NULL
  expect_snapshot(print(export_table(
    out,
    table_width = 60,
    remove_duplicates = TRUE,
    empty_line = "-",
    cross = "+"
  )))
  expect_snapshot(print(export_table(
    out,
    table_width = 60,
    remove_duplicates = FALSE,
    empty_line = "-",
    cross = "+"
  )))
  out <- datawizard::data_codebook(efc_insight[, 1:3])
  out$.row_id <- NULL
  expect_snapshot(print(export_table(
    out,
    table_width = 60,
    remove_duplicates = TRUE,
    empty_line = "-",
    cross = "+"
  )))
  expect_snapshot(print(export_table(
    out,
    table_width = 60,
    remove_duplicates = FALSE,
    empty_line = "-",
    cross = "+"
  )))
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
  d <- data.frame(
    a = c(1.3, 2, 543),
    b = c("ab", "cd", "abcde"),
    stringsAsFactors = FALSE
  )
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
  out <- gt::as_raw_html(print_html(
    cp,
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
    strsplit(out, "\n", fixed = TRUE)[[1]][1],
    "   a |    b |    c |    d |      e"
  )
  out <- export_table(x, column_names = c(Species = "a"))
  expect_identical(
    strsplit(out, "\n", fixed = TRUE)[[1]][1],
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
  # fmt: skip
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

test_that("export_table with big_mark", {
  # Test with comma separator
  d <- data.frame(
    x = c(1234.56, 9876543.21, 12.34),
    y = c("a", "b", "c"),
    stringsAsFactors = FALSE
  )
  out <- export_table(d, big_mark = ",", format = "text")
  expect_true(any(grepl("1,234.56", out, fixed = TRUE)))
  expect_true(any(grepl("9,876,543.21", out, fixed = TRUE)))

  # Test with space separator
  out <- export_table(d, big_mark = " ", format = "text")
  expect_true(any(grepl("1 234.56", out, fixed = TRUE)))
  expect_true(any(grepl("9 876 543.21", out, fixed = TRUE)))

  # Test with markdown format
  out <- export_table(d, big_mark = ",", format = "md")
  expect_true(any(grepl("1,234.56", out, fixed = TRUE)))
  expect_true(any(grepl("9,876,543.21", out, fixed = TRUE)))

  # Test backward compatibility - no big_mark
  out <- export_table(d, format = "text")
  expect_true(any(grepl("1234.56", out, fixed = TRUE)))
  expect_true(any(grepl("9.88e+06", out, fixed = TRUE)))
})
