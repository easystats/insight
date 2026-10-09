skip_on_ci()

test_that("export_table-1", {
  skip_on_cran()
  d <- data.frame(
    a = c(1.3, 2, 543),
    b = c("ab", "cd", "abcde"),
    stringsAsFactors = FALSE
  )
  expect_equal(
    export_table(d),
    structure(
      "     a |     b\n--------------\n  1.30 |    ab\n  2.00 |    cd\n543.00 | abcde\n",
      class = c("insight_table", "character")
    ),
    ignore_attr = TRUE
  )
})

test_that("export_table-2", {
  skip_on_cran()
  d <- data.frame(
    a = c(1.3, 2, 543),
    b = c("ab", "cd", "abcde"),
    stringsAsFactors = FALSE
  )
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

test_that("export_table-3", {
  d <- data.frame(
    a = c(1.3, 2, 543),
    b = c("ab", "cd", "abcde"),
    stringsAsFactors = FALSE
  )
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


test_that("export_table-4", {
  d <- data.frame(
    a = c(1.3, 2, 543),
    b = c("ab", "cd", "abcde"),
    stringsAsFactors = FALSE
  )
  attr(d, "table_caption") <- "Table Title"
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


test_that("export_table-5", {
  d <- data.frame(
    a = c(1.3, 2, 543),
    b = c("ab", "cd", "abcde"),
    stringsAsFactors = FALSE
  )
  attr(d, "table_title") <- "Table Title"
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

test_that("export_table-6", {
  d <- data.frame(
    a = c(1.3, 2, 543),
    b = c("ab", "cd", "abcde"),
    stringsAsFactors = FALSE
  )
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

test_that("export_table-7", {
  d <- data.frame(
    a = c(1.3, 2, 543),
    b = c("ab", "cd", "abcde"),
    stringsAsFactors = FALSE
  )
  attr(d, "table_caption") <- "Table Title"
  attr(d, "table_footer") <- list("first", "second", "third")
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


test_that("export_table, table_width (lavaan)", {
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
      "model1 |      918.852 |       < .001 | 0.959",
      "model2 |      918.852 |       < .001 | 0.959",
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


test_that("export_table, table_width (lavaan), no split", {
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
      "model1 | lavaan |   85.306 |   < .001 |      918.852 |       < .001 | 0.959 | 0.894 | 0.907 | 0.896 | 0.931 | 0.092 | [0.07, 0.11] |    < .001 | 0.082 | 0.065 | 0.861 | 0.605 | 0.931 | 0.931 |     -3737.745 | 7517.5 (0.500) | 7595.3 (0.500) |     7528.739",
      "model2 | lavaan |   85.306 |   < .001 |      918.852 |       < .001 | 0.959 | 0.894 | 0.907 | 0.896 | 0.931 | 0.092 | [0.07, 0.11] |    < .001 | 0.082 | 0.065 | 0.861 | 0.605 | 0.931 | 0.931 |     -3737.745 | 7517.5 (0.500) | 7595.3 (0.500) |     7528.739"
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
  expect_identical(
    capture.output(print(export_table(out, table_width = 60, remove_duplicates = FALSE))),
    c(
      "ID |     Name |                                    Label",
      "--------------------------------------------------------",
      "1  |  c12hour | average number of hours of care per week",
      "   |          |                                         ",
      "2  |   e16sex |                           elder's gender",
      "   |          |                                         ",
      "   |          |                                         ",
      "3  |   e42dep |                       elder's dependency",
      "   |          |                                         ",
      "   |          |                                         ",
      "   |          |                                         ",
      "   |          |                                         ",
      "4  | c172code |               carer's level of education",
      "   |          |                                         ",
      "   |          |                                         ",
      "   |          |                                         ",
      "5  |  neg_c_7 |             Negative impact with 7 items",
      "   |          |                                         ",
      "",
      "ID |        Type |   Missings |   Values",
      "----------------------------------------",
      "1  |     numeric |   2 (2.0%) | [5, 168]",
      "   |             |            |         ",
      "2  |     numeric |   0 (0.0%) |        1",
      "   |             |            |        2",
      "   |             |            |         ",
      "3  | categorical |   3 (3.0%) |        1",
      "   |             |            |        2",
      "   |             |            |        3",
      "   |             |            |        4",
      "   |             |            |         ",
      "4  |     numeric | 10 (10.0%) |        1",
      "   |             |            |        2",
      "   |             |            |        3",
      "   |             |            |         ",
      "5  |     numeric |   3 (3.0%) |  [7, 28]",
      "   |             |            |         ",
      "",
      "ID |                    Value Labels |  N |  Prop",
      "-------------------------------------------------",
      "1  |                                 | 98 |      ",
      "   |                                 |    |      ",
      "2  |                            male | 46 | 46.0%",
      "   |                          female | 54 | 54.0%",
      "   |                                 |    |      ",
      "3  |                     independent |  2 |  2.1%",
      "   |              slightly dependent |  4 |  4.1%",
      "   |            moderately dependent | 28 | 28.9%",
      "   |              severely dependent | 63 | 64.9%",
      "   |                                 |    |      ",
      "4  |          low level of education |  8 |  8.9%",
      "   | intermediate level of education | 66 | 73.3%",
      "   |         high level of education | 16 | 17.8%",
      "   |                                 |    |      ",
      "5  |                                 | 97 |      ",
      "   |                                 |    |      "
    )
  )
  expect_identical(
    capture.output(print(export_table(
      out,
      table_width = 60,
      empty_line = "-",
      remove_duplicates = FALSE
    ))),
    c(
      "ID |     Name |                                    Label",
      "--------------------------------------------------------",
      "1  |  c12hour | average number of hours of care per week",
      "--------------------------------------------------------",
      "2  |   e16sex |                           elder's gender",
      "                                                        ",
      "--------------------------------------------------------",
      "3  |   e42dep |                       elder's dependency",
      "                                                        ",
      "                                                        ",
      "                                                        ",
      "--------------------------------------------------------",
      "4  | c172code |               carer's level of education",
      "                                                        ",
      "                                                        ",
      "--------------------------------------------------------",
      "5  |  neg_c_7 |             Negative impact with 7 items",
      "--------------------------------------------------------",
      "",
      "ID |        Type |   Missings |   Values",
      "----------------------------------------",
      "1  |     numeric |   2 (2.0%) | [5, 168]",
      "----------------------------------------",
      "2  |     numeric |   0 (0.0%) |        1",
      "   |             |            |        2",
      "----------------------------------------",
      "3  | categorical |   3 (3.0%) |        1",
      "   |             |            |        2",
      "   |             |            |        3",
      "   |             |            |        4",
      "----------------------------------------",
      "4  |     numeric | 10 (10.0%) |        1",
      "   |             |            |        2",
      "   |             |            |        3",
      "----------------------------------------",
      "5  |     numeric |   3 (3.0%) |  [7, 28]",
      "----------------------------------------",
      "",
      "ID |                    Value Labels |  N |  Prop",
      "-------------------------------------------------",
      "1  |                                 | 98 |      ",
      "-------------------------------------------------",
      "2  |                            male | 46 | 46.0%",
      "   |                          female | 54 | 54.0%",
      "-------------------------------------------------",
      "3  |                     independent |  2 |  2.1%",
      "   |              slightly dependent |  4 |  4.1%",
      "   |            moderately dependent | 28 | 28.9%",
      "   |              severely dependent | 63 | 64.9%",
      "-------------------------------------------------",
      "4  |          low level of education |  8 |  8.9%",
      "   | intermediate level of education | 66 | 73.3%",
      "   |         high level of education | 16 | 17.8%",
      "-------------------------------------------------",
      "5  |                                 | 97 |      ",
      "-------------------------------------------------"
    )
  )
  expect_identical(
    capture.output(print(export_table(
      out,
      table_width = 60,
      empty_line = "-",
      sep = " | ",
      remove_duplicates = FALSE
    ))),
    c(
      "ID |     Name |                                    Label",
      "--------------------------------------------------------",
      "1  |  c12hour | average number of hours of care per week",
      "--------------------------------------------------------",
      "2  |   e16sex |                           elder's gender",
      "                                                        ",
      "--------------------------------------------------------",
      "3  |   e42dep |                       elder's dependency",
      "                                                        ",
      "                                                        ",
      "                                                        ",
      "--------------------------------------------------------",
      "4  | c172code |               carer's level of education",
      "                                                        ",
      "                                                        ",
      "--------------------------------------------------------",
      "5  |  neg_c_7 |             Negative impact with 7 items",
      "--------------------------------------------------------",
      "",
      "ID |        Type |   Missings |   Values",
      "----------------------------------------",
      "1  |     numeric |   2 (2.0%) | [5, 168]",
      "----------------------------------------",
      "2  |     numeric |   0 (0.0%) |        1",
      "   |             |            |        2",
      "----------------------------------------",
      "3  | categorical |   3 (3.0%) |        1",
      "   |             |            |        2",
      "   |             |            |        3",
      "   |             |            |        4",
      "----------------------------------------",
      "4  |     numeric | 10 (10.0%) |        1",
      "   |             |            |        2",
      "   |             |            |        3",
      "----------------------------------------",
      "5  |     numeric |   3 (3.0%) |  [7, 28]",
      "----------------------------------------",
      "",
      "ID |                    Value Labels |  N |  Prop",
      "-------------------------------------------------",
      "1  |                                 | 98 |      ",
      "-------------------------------------------------",
      "2  |                            male | 46 | 46.0%",
      "   |                          female | 54 | 54.0%",
      "-------------------------------------------------",
      "3  |                     independent |  2 |  2.1%",
      "   |              slightly dependent |  4 |  4.1%",
      "   |            moderately dependent | 28 | 28.9%",
      "   |              severely dependent | 63 | 64.9%",
      "-------------------------------------------------",
      "4  |          low level of education |  8 |  8.9%",
      "   | intermediate level of education | 66 | 73.3%",
      "   |         high level of education | 16 | 17.8%",
      "-------------------------------------------------",
      "5  |                                 | 97 |      ",
      "-------------------------------------------------"
    )
  )
  expect_identical(
    capture.output(print(export_table(
      out,
      table_width = 60,
      empty_line = "-",
      cross = "+",
      remove_duplicates = FALSE
    ))),
    c(
      "ID |     Name |                                    Label",
      "---+----------+-----------------------------------------",
      "1  |  c12hour | average number of hours of care per week",
      "---+----------+-----------------------------------------",
      "2  |   e16sex |                           elder's gender",
      "   |          |                                         ",
      "---+----------+-----------------------------------------",
      "3  |   e42dep |                       elder's dependency",
      "   |          |                                         ",
      "   |          |                                         ",
      "   |          |                                         ",
      "---+----------+-----------------------------------------",
      "4  | c172code |               carer's level of education",
      "   |          |                                         ",
      "   |          |                                         ",
      "---+----------+-----------------------------------------",
      "5  |  neg_c_7 |             Negative impact with 7 items",
      "--------------------------------------------------------",
      "",
      "ID |        Type |   Missings |   Values",
      "---+-------------+------------+---------",
      "1  |     numeric |   2 (2.0%) | [5, 168]",
      "---+-------------+------------+---------",
      "2  |     numeric |   0 (0.0%) |        1",
      "   |             |            |        2",
      "---+-------------+------------+---------",
      "3  | categorical |   3 (3.0%) |        1",
      "   |             |            |        2",
      "   |             |            |        3",
      "   |             |            |        4",
      "---+-------------+------------+---------",
      "4  |     numeric | 10 (10.0%) |        1",
      "   |             |            |        2",
      "   |             |            |        3",
      "---+-------------+------------+---------",
      "5  |     numeric |   3 (3.0%) |  [7, 28]",
      "----------------------------------------",
      "",
      "ID |                    Value Labels |  N |  Prop",
      "---+---------------------------------+----+------",
      "1  |                                 | 98 |      ",
      "---+---------------------------------+----+------",
      "2  |                            male | 46 | 46.0%",
      "   |                          female | 54 | 54.0%",
      "---+---------------------------------+----+------",
      "3  |                     independent |  2 |  2.1%",
      "   |              slightly dependent |  4 |  4.1%",
      "   |            moderately dependent | 28 | 28.9%",
      "   |              severely dependent | 63 | 64.9%",
      "---+---------------------------------+----+------",
      "4  |          low level of education |  8 |  8.9%",
      "   | intermediate level of education | 66 | 73.3%",
      "   |         high level of education | 16 | 17.8%",
      "---+---------------------------------+----+------",
      "5  |                                 | 97 |      ",
      "-------------------------------------------------"
    )
  )
  # don't remove duplicates
  expect_identical(
    capture.output(print(export_table(out, table_width = 60, remove_duplicates = FALSE))),
    c(
      "ID |     Name |                                    Label",
      "--------------------------------------------------------",
      "1  |  c12hour | average number of hours of care per week",
      "   |          |                                         ",
      "2  |   e16sex |                           elder's gender",
      "   |          |                                         ",
      "   |          |                                         ",
      "3  |   e42dep |                       elder's dependency",
      "   |          |                                         ",
      "   |          |                                         ",
      "   |          |                                         ",
      "   |          |                                         ",
      "4  | c172code |               carer's level of education",
      "   |          |                                         ",
      "   |          |                                         ",
      "   |          |                                         ",
      "5  |  neg_c_7 |             Negative impact with 7 items",
      "   |          |                                         ",
      "",
      "ID |        Type |   Missings |   Values",
      "----------------------------------------",
      "1  |     numeric |   2 (2.0%) | [5, 168]",
      "   |             |            |         ",
      "2  |     numeric |   0 (0.0%) |        1",
      "   |             |            |        2",
      "   |             |            |         ",
      "3  | categorical |   3 (3.0%) |        1",
      "   |             |            |        2",
      "   |             |            |        3",
      "   |             |            |        4",
      "   |             |            |         ",
      "4  |     numeric | 10 (10.0%) |        1",
      "   |             |            |        2",
      "   |             |            |        3",
      "   |             |            |         ",
      "5  |     numeric |   3 (3.0%) |  [7, 28]",
      "   |             |            |         ",
      "",
      "ID |                    Value Labels |  N |  Prop",
      "-------------------------------------------------",
      "1  |                                 | 98 |      ",
      "   |                                 |    |      ",
      "2  |                            male | 46 | 46.0%",
      "   |                          female | 54 | 54.0%",
      "   |                                 |    |      ",
      "3  |                     independent |  2 |  2.1%",
      "   |              slightly dependent |  4 |  4.1%",
      "   |            moderately dependent | 28 | 28.9%",
      "   |              severely dependent | 63 | 64.9%",
      "   |                                 |    |      ",
      "4  |          low level of education |  8 |  8.9%",
      "   | intermediate level of education | 66 | 73.3%",
      "   |         high level of education | 16 | 17.8%",
      "   |                                 |    |      ",
      "5  |                                 | 97 |      ",
      "   |                                 |    |      "
    )
  )
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
  expect_identical(
    capture.output(print(export_table(d[1:10, ], verbose = FALSE))),
    c(
      "ano1_bno1_cno1_dno1_eno1_fno1_gno1_hno1_ino1_jno1_kno1_lno1_mno1_nno1_ono1_pno1_qno1_rno1_sno1_tno1_uno1_vno1_wno1_xno1_yno1_zno1",
      "---------------------------------------------------------------------------------------------------------------------------------",
      "                                                                                                                             5.10",
      "                                                                                                                             4.90",
      "                                                                                                                             4.70",
      "                                                                                                                             4.60",
      "                                                                                                                             5.00",
      "                                                                                                                             5.40",
      "                                                                                                                             4.60",
      "                                                                                                                             5.00",
      "                                                                                                                             4.40",
      "                                                                                                                             4.90",
      "",
      "ano2_bno2_cno2_dno2_eno2_fno2_gno2_hno2_ino2_jno2_kno2_lno2_mno2_nno2_ono2_pno2_qno2_rno2_sno2_tno2_uno2_vno2_wno2_xno2_yno2_zno2",
      "---------------------------------------------------------------------------------------------------------------------------------",
      "                                                                                                                             3.50",
      "                                                                                                                             3.00",
      "                                                                                                                             3.20",
      "                                                                                                                             3.10",
      "                                                                                                                             3.60",
      "                                                                                                                             3.90",
      "                                                                                                                             3.40",
      "                                                                                                                             3.40",
      "                                                                                                                             2.90",
      "                                                                                                                             3.10",
      "",
      "Petal.Length | Petal.Width | Species",
      "------------------------------------",
      "        1.40 |        0.20 |  setosa",
      "        1.40 |        0.20 |  setosa",
      "        1.30 |        0.20 |  setosa",
      "        1.50 |        0.20 |  setosa",
      "        1.40 |        0.20 |  setosa",
      "        1.70 |        0.40 |  setosa",
      "        1.40 |        0.30 |  setosa",
      "        1.50 |        0.20 |  setosa",
      "        1.40 |        0.20 |  setosa",
      "        1.50 |        0.10 |  setosa"
    )
  )

  d <- iris
  colnames(d)[2] <- paste0(letters[1:26], "no1", collapse = "_")
  colnames(d)[5] <- paste0(letters[1:26], "no2", collapse = "_")
  expect_warning(export_table(d[1:10, ]), regex = "The table contains")
  expect_identical(
    capture.output(print(export_table(d[1:10, ], verbose = FALSE))),
    c(
      "Sepal.Length",
      "------------",
      "        5.10",
      "        4.90",
      "        4.70",
      "        4.60",
      "        5.00",
      "        5.40",
      "        4.60",
      "        5.00",
      "        4.40",
      "        4.90",
      "",
      "ano1_bno1_cno1_dno1_eno1_fno1_gno1_hno1_ino1_jno1_kno1_lno1_mno1_nno1_ono1_pno1_qno1_rno1_sno1_tno1_uno1_vno1_wno1_xno1_yno1_zno1",
      "---------------------------------------------------------------------------------------------------------------------------------",
      "                                                                                                                             3.50",
      "                                                                                                                             3.00",
      "                                                                                                                             3.20",
      "                                                                                                                             3.10",
      "                                                                                                                             3.60",
      "                                                                                                                             3.90",
      "                                                                                                                             3.40",
      "                                                                                                                             3.40",
      "                                                                                                                             2.90",
      "                                                                                                                             3.10",
      "",
      "Petal.Length | Petal.Width | ano2_bno2_cno2_dno2_eno2_fno2_gno2_hno2_ino2_jno2_kno2_lno2_mno2_nno2_ono2_pno2_qno2_rno2_sno2_tno2_uno2_vno2_wno2_xno2_yno2_zno2",
      "--------------------------------------------------------------------------------------------------------------------------------------------------------------",
      "        1.40 |        0.20 |                                                                                                                            setosa",
      "        1.40 |        0.20 |                                                                                                                            setosa",
      "        1.30 |        0.20 |                                                                                                                            setosa",
      "        1.50 |        0.20 |                                                                                                                            setosa",
      "        1.40 |        0.20 |                                                                                                                            setosa",
      "        1.70 |        0.40 |                                                                                                                            setosa",
      "        1.40 |        0.30 |                                                                                                                            setosa",
      "        1.50 |        0.20 |                                                                                                                            setosa",
      "        1.40 |        0.20 |                                                                                                                            setosa",
      "        1.50 |        0.10 |                                                                                                                            setosa"
    )
  )
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
  expect_identical(
    as.character(out),
    "<div id=\"osncjrvket\" style=\"padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;\">\n  \n  <table class=\"gt_table\" data-quarto-disable-processing=\"false\" data-quarto-bootstrap=\"false\" style=\"-webkit-font-smoothing: antialiased; -moz-osx-font-smoothing: grayscale; font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji'; display: table; border-collapse: collapse; line-height: normal; margin-left: auto; margin-right: auto; color: #333333; font-size: 16px; font-weight: normal; font-style: normal; background-color: #FFFFFF; width: auto; border-top-style: solid; border-top-width: 2px; border-top-color: #A8A8A8; border-right-style: none; border-right-width: 2px; border-right-color: #D3D3D3; border-bottom-style: solid; border-bottom-width: 2px; border-bottom-color: #A8A8A8; border-left-style: none; border-left-width: 2px; border-left-color: #D3D3D3;\" bgcolor=\"#FFFFFF\">\n  <thead style=\"border-style: none;\">\n    <tr class=\"gt_heading\" style=\"border-style: none; background-color: #FFFFFF; text-align: center; border-bottom-color: #FFFFFF; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3;\" bgcolor=\"#FFFFFF\" align=\"center\">\n      <td colspan=\"2\" class=\"gt_heading gt_title gt_font_normal gt_bottom_border\" style=\"border-style: none; color: #333333; font-size: 125%; padding-top: 4px; padding-bottom: 4px; padding-left: 5px; padding-right: 5px; background-color: #FFFFFF; text-align: center; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; border-bottom-style: solid; border-bottom-width: 2px; border-bottom-color: #D3D3D3; font-weight: normal;\" bgcolor=\"#FFFFFF\" align=\"center\">Table Title</td>\n    </tr>\n    \n    <tr class=\"gt_col_headings\" style=\"border-style: none; border-top-style: solid; border-top-width: 2px; border-top-color: #D3D3D3; border-bottom-style: solid; border-bottom-width: 2px; border-bottom-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3;\">\n      <th class=\"gt_col_heading gt_columns_bottom_border gt_left\" rowspan=\"1\" colspan=\"1\" scope=\"col\" id=\"a\" style=\"border-style: none; color: #333333; background-color: #FFFFFF; font-size: 100%; font-weight: normal; text-transform: inherit; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: bottom; padding-top: 5px; padding-bottom: 6px; padding-left: 5px; padding-right: 5px; overflow-x: hidden; text-align: left;\" bgcolor=\"#FFFFFF\" valign=\"bottom\" align=\"left\">a</th>\n      <th class=\"gt_col_heading gt_columns_bottom_border gt_center\" rowspan=\"1\" colspan=\"1\" scope=\"col\" id=\"b\" style=\"border-style: none; color: #333333; background-color: #FFFFFF; font-size: 100%; font-weight: normal; text-transform: inherit; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: bottom; padding-top: 5px; padding-bottom: 6px; padding-left: 5px; padding-right: 5px; overflow-x: hidden; text-align: center;\" bgcolor=\"#FFFFFF\" valign=\"bottom\" align=\"center\">b</th>\n    </tr>\n  </thead>\n  <tbody class=\"gt_table_body\" style=\"border-style: none; border-top-style: solid; border-top-width: 2px; border-top-color: #D3D3D3; border-bottom-style: solid; border-bottom-width: 2px; border-bottom-color: #D3D3D3;\">\n    <tr style=\"border-style: none;\"><td headers=\"a\" class=\"gt_row gt_left\" style=\"border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: left;\" valign=\"middle\" align=\"left\">1.30</td>\n<td headers=\"b\" class=\"gt_row gt_center\" style=\"border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: center;\" valign=\"middle\" align=\"center\">ab</td></tr>\n    <tr style=\"border-style: none;\"><td headers=\"a\" class=\"gt_row gt_left\" style=\"border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: left;\" valign=\"middle\" align=\"left\">2.00</td>\n<td headers=\"b\" class=\"gt_row gt_center\" style=\"border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: center;\" valign=\"middle\" align=\"center\">cd</td></tr>\n    <tr style=\"border-style: none;\"><td headers=\"a\" class=\"gt_row gt_left\" style=\"border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: left;\" valign=\"middle\" align=\"left\">543.00</td>\n<td headers=\"b\" class=\"gt_row gt_center\" style=\"border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: center;\" valign=\"middle\" align=\"center\">abcde</td></tr>\n  </tbody>\n  \n</table>\n</div>"
  )
  set.seed(123)
  out <- gt::as_raw_html(export_table(d, format = "html", align = "rl"))
  expect_identical(
    as.character(out),
    "<div id=\"osncjrvket\" style=\"padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;\">\n  \n  <table class=\"gt_table\" data-quarto-disable-processing=\"false\" data-quarto-bootstrap=\"false\" style=\"-webkit-font-smoothing: antialiased; -moz-osx-font-smoothing: grayscale; font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji'; display: table; border-collapse: collapse; line-height: normal; margin-left: auto; margin-right: auto; color: #333333; font-size: 16px; font-weight: normal; font-style: normal; background-color: #FFFFFF; width: auto; border-top-style: solid; border-top-width: 2px; border-top-color: #A8A8A8; border-right-style: none; border-right-width: 2px; border-right-color: #D3D3D3; border-bottom-style: solid; border-bottom-width: 2px; border-bottom-color: #A8A8A8; border-left-style: none; border-left-width: 2px; border-left-color: #D3D3D3;\" bgcolor=\"#FFFFFF\">\n  <thead style=\"border-style: none;\">\n    <tr class=\"gt_heading\" style=\"border-style: none; background-color: #FFFFFF; text-align: center; border-bottom-color: #FFFFFF; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3;\" bgcolor=\"#FFFFFF\" align=\"center\">\n      <td colspan=\"2\" class=\"gt_heading gt_title gt_font_normal gt_bottom_border\" style=\"border-style: none; color: #333333; font-size: 125%; padding-top: 4px; padding-bottom: 4px; padding-left: 5px; padding-right: 5px; background-color: #FFFFFF; text-align: center; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; border-bottom-style: solid; border-bottom-width: 2px; border-bottom-color: #D3D3D3; font-weight: normal;\" bgcolor=\"#FFFFFF\" align=\"center\">Table Title</td>\n    </tr>\n    \n    <tr class=\"gt_col_headings\" style=\"border-style: none; border-top-style: solid; border-top-width: 2px; border-top-color: #D3D3D3; border-bottom-style: solid; border-bottom-width: 2px; border-bottom-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3;\">\n      <th class=\"gt_col_heading gt_columns_bottom_border gt_right\" rowspan=\"1\" colspan=\"1\" scope=\"col\" id=\"a\" style=\"border-style: none; color: #333333; background-color: #FFFFFF; font-size: 100%; font-weight: normal; text-transform: inherit; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: bottom; padding-top: 5px; padding-bottom: 6px; padding-left: 5px; padding-right: 5px; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;\" bgcolor=\"#FFFFFF\" valign=\"bottom\" align=\"right\">a</th>\n      <th class=\"gt_col_heading gt_columns_bottom_border gt_left\" rowspan=\"1\" colspan=\"1\" scope=\"col\" id=\"b\" style=\"border-style: none; color: #333333; background-color: #FFFFFF; font-size: 100%; font-weight: normal; text-transform: inherit; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: bottom; padding-top: 5px; padding-bottom: 6px; padding-left: 5px; padding-right: 5px; overflow-x: hidden; text-align: left;\" bgcolor=\"#FFFFFF\" valign=\"bottom\" align=\"left\">b</th>\n    </tr>\n  </thead>\n  <tbody class=\"gt_table_body\" style=\"border-style: none; border-top-style: solid; border-top-width: 2px; border-top-color: #D3D3D3; border-bottom-style: solid; border-bottom-width: 2px; border-bottom-color: #D3D3D3;\">\n    <tr style=\"border-style: none;\"><td headers=\"a\" class=\"gt_row gt_right\" style=\"border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;\" valign=\"middle\" align=\"right\">1.30</td>\n<td headers=\"b\" class=\"gt_row gt_left\" style=\"border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: left;\" valign=\"middle\" align=\"left\">ab</td></tr>\n    <tr style=\"border-style: none;\"><td headers=\"a\" class=\"gt_row gt_right\" style=\"border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;\" valign=\"middle\" align=\"right\">2.00</td>\n<td headers=\"b\" class=\"gt_row gt_left\" style=\"border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: left;\" valign=\"middle\" align=\"left\">cd</td></tr>\n    <tr style=\"border-style: none;\"><td headers=\"a\" class=\"gt_row gt_right\" style=\"border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;\" valign=\"middle\" align=\"right\">543.00</td>\n<td headers=\"b\" class=\"gt_row gt_left\" style=\"border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: left;\" valign=\"middle\" align=\"left\">abcde</td></tr>\n  </tbody>\n  \n</table>\n</div>"
  )

  d <- data.frame(
    a = c(1.3, 2, 543, 78),
    b = c("ab", "cd", "abcde", "hj"),
    g = c("g1", "g1", "g2", "g2"),
    stringsAsFactors = FALSE
  )
  set.seed(123)
  out <- gt::as_raw_html(export_table(d, format = "html", by = "g"))
  expect_identical(
    as.character(out),
    "<div id=\"osncjrvket\" style=\"padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;\">\n  \n  <table class=\"gt_table\" data-quarto-disable-processing=\"false\" data-quarto-bootstrap=\"false\" style=\"-webkit-font-smoothing: antialiased; -moz-osx-font-smoothing: grayscale; font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji'; display: table; border-collapse: collapse; line-height: normal; margin-left: auto; margin-right: auto; color: #333333; font-size: 16px; font-weight: normal; font-style: normal; background-color: #FFFFFF; width: auto; border-top-style: solid; border-top-width: 2px; border-top-color: #A8A8A8; border-right-style: none; border-right-width: 2px; border-right-color: #D3D3D3; border-bottom-style: solid; border-bottom-width: 2px; border-bottom-color: #A8A8A8; border-left-style: none; border-left-width: 2px; border-left-color: #D3D3D3;\" bgcolor=\"#FFFFFF\">\n  <thead style=\"border-style: none;\">\n    <tr class=\"gt_col_headings\" style=\"border-style: none; border-top-style: solid; border-top-width: 2px; border-top-color: #D3D3D3; border-bottom-style: solid; border-bottom-width: 2px; border-bottom-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3;\">\n      <th class=\"gt_col_heading gt_columns_bottom_border gt_left\" rowspan=\"1\" colspan=\"1\" scope=\"col\" id=\"a\" style=\"border-style: none; color: #333333; background-color: #FFFFFF; font-size: 100%; font-weight: normal; text-transform: inherit; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: bottom; padding-top: 5px; padding-bottom: 6px; padding-left: 5px; padding-right: 5px; overflow-x: hidden; text-align: left;\" bgcolor=\"#FFFFFF\" valign=\"bottom\" align=\"left\">a</th>\n      <th class=\"gt_col_heading gt_columns_bottom_border gt_center\" rowspan=\"1\" colspan=\"1\" scope=\"col\" id=\"b\" style=\"border-style: none; color: #333333; background-color: #FFFFFF; font-size: 100%; font-weight: normal; text-transform: inherit; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: bottom; padding-top: 5px; padding-bottom: 6px; padding-left: 5px; padding-right: 5px; overflow-x: hidden; text-align: center;\" bgcolor=\"#FFFFFF\" valign=\"bottom\" align=\"center\">b</th>\n    </tr>\n  </thead>\n  <tbody class=\"gt_table_body\" style=\"border-style: none; border-top-style: solid; border-top-width: 2px; border-top-color: #D3D3D3; border-bottom-style: solid; border-bottom-width: 2px; border-bottom-color: #D3D3D3;\">\n    <tr class=\"gt_group_heading_row\" style=\"border-style: none;\">\n      <th colspan=\"2\" class=\"gt_group_heading\" style=\"border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; color: #333333; background-color: #FFFFFF; font-size: 100%; font-weight: initial; text-transform: inherit; border-top-style: solid; border-top-width: 2px; border-top-color: #D3D3D3; border-bottom-style: solid; border-bottom-width: 2px; border-bottom-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; text-align: left; font-style: oblique;\" scope=\"colgroup\" id=\"g1\" bgcolor=\"#FFFFFF\" valign=\"middle\" align=\"left\">g1</th>\n    </tr>\n    <tr class=\"gt_row_group_first\" style=\"border-style: none;\"><td headers=\"g1  a\" class=\"gt_row gt_left\" style=\"border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: left; border-top-width: 2px;\" valign=\"middle\" align=\"left\">1.30</td>\n<td headers=\"g1  b\" class=\"gt_row gt_center\" style=\"border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: center; border-top-width: 2px;\" valign=\"middle\" align=\"center\">ab</td></tr>\n    <tr style=\"border-style: none;\"><td headers=\"g1  a\" class=\"gt_row gt_left\" style=\"border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: left;\" valign=\"middle\" align=\"left\">2.00</td>\n<td headers=\"g1  b\" class=\"gt_row gt_center\" style=\"border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: center;\" valign=\"middle\" align=\"center\">cd</td></tr>\n    <tr class=\"gt_group_heading_row\" style=\"border-style: none;\">\n      <th colspan=\"2\" class=\"gt_group_heading\" style=\"border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; color: #333333; background-color: #FFFFFF; font-size: 100%; font-weight: initial; text-transform: inherit; border-top-style: solid; border-top-width: 2px; border-top-color: #D3D3D3; border-bottom-style: solid; border-bottom-width: 2px; border-bottom-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; text-align: left; font-style: oblique;\" scope=\"colgroup\" id=\"g2\" bgcolor=\"#FFFFFF\" valign=\"middle\" align=\"left\">g2</th>\n    </tr>\n    <tr class=\"gt_row_group_first\" style=\"border-style: none;\"><td headers=\"g2  a\" class=\"gt_row gt_left\" style=\"border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: left; border-top-width: 2px;\" valign=\"middle\" align=\"left\">543.00</td>\n<td headers=\"g2  b\" class=\"gt_row gt_center\" style=\"border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: center; border-top-width: 2px;\" valign=\"middle\" align=\"center\">abcde</td></tr>\n    <tr style=\"border-style: none;\"><td headers=\"g2  a\" class=\"gt_row gt_left\" style=\"border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: left;\" valign=\"middle\" align=\"left\">78.00</td>\n<td headers=\"g2  b\" class=\"gt_row gt_center\" style=\"border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: center;\" valign=\"middle\" align=\"center\">hj</td></tr>\n  </tbody>\n  \n</table>\n</div>"
  )
  set.seed(123)
  out <- gt::as_raw_html(export_table(d, format = "html", align = "rl", by = "g"))
  expect_identical(
    as.character(out),
    "<div id=\"osncjrvket\" style=\"padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;\">\n  \n  <table class=\"gt_table\" data-quarto-disable-processing=\"false\" data-quarto-bootstrap=\"false\" style=\"-webkit-font-smoothing: antialiased; -moz-osx-font-smoothing: grayscale; font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji'; display: table; border-collapse: collapse; line-height: normal; margin-left: auto; margin-right: auto; color: #333333; font-size: 16px; font-weight: normal; font-style: normal; background-color: #FFFFFF; width: auto; border-top-style: solid; border-top-width: 2px; border-top-color: #A8A8A8; border-right-style: none; border-right-width: 2px; border-right-color: #D3D3D3; border-bottom-style: solid; border-bottom-width: 2px; border-bottom-color: #A8A8A8; border-left-style: none; border-left-width: 2px; border-left-color: #D3D3D3;\" bgcolor=\"#FFFFFF\">\n  <thead style=\"border-style: none;\">\n    <tr class=\"gt_col_headings\" style=\"border-style: none; border-top-style: solid; border-top-width: 2px; border-top-color: #D3D3D3; border-bottom-style: solid; border-bottom-width: 2px; border-bottom-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3;\">\n      <th class=\"gt_col_heading gt_columns_bottom_border gt_right\" rowspan=\"1\" colspan=\"1\" scope=\"col\" id=\"a\" style=\"border-style: none; color: #333333; background-color: #FFFFFF; font-size: 100%; font-weight: normal; text-transform: inherit; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: bottom; padding-top: 5px; padding-bottom: 6px; padding-left: 5px; padding-right: 5px; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;\" bgcolor=\"#FFFFFF\" valign=\"bottom\" align=\"right\">a</th>\n      <th class=\"gt_col_heading gt_columns_bottom_border gt_left\" rowspan=\"1\" colspan=\"1\" scope=\"col\" id=\"b\" style=\"border-style: none; color: #333333; background-color: #FFFFFF; font-size: 100%; font-weight: normal; text-transform: inherit; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: bottom; padding-top: 5px; padding-bottom: 6px; padding-left: 5px; padding-right: 5px; overflow-x: hidden; text-align: left;\" bgcolor=\"#FFFFFF\" valign=\"bottom\" align=\"left\">b</th>\n    </tr>\n  </thead>\n  <tbody class=\"gt_table_body\" style=\"border-style: none; border-top-style: solid; border-top-width: 2px; border-top-color: #D3D3D3; border-bottom-style: solid; border-bottom-width: 2px; border-bottom-color: #D3D3D3;\">\n    <tr class=\"gt_group_heading_row\" style=\"border-style: none;\">\n      <th colspan=\"2\" class=\"gt_group_heading\" style=\"border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; color: #333333; background-color: #FFFFFF; font-size: 100%; font-weight: initial; text-transform: inherit; border-top-style: solid; border-top-width: 2px; border-top-color: #D3D3D3; border-bottom-style: solid; border-bottom-width: 2px; border-bottom-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; text-align: left; font-style: oblique;\" scope=\"colgroup\" id=\"g1\" bgcolor=\"#FFFFFF\" valign=\"middle\" align=\"left\">g1</th>\n    </tr>\n    <tr class=\"gt_row_group_first\" style=\"border-style: none;\"><td headers=\"g1  a\" class=\"gt_row gt_right\" style=\"border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums; border-top-width: 2px;\" valign=\"middle\" align=\"right\">1.30</td>\n<td headers=\"g1  b\" class=\"gt_row gt_left\" style=\"border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: left; border-top-width: 2px;\" valign=\"middle\" align=\"left\">ab</td></tr>\n    <tr style=\"border-style: none;\"><td headers=\"g1  a\" class=\"gt_row gt_right\" style=\"border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;\" valign=\"middle\" align=\"right\">2.00</td>\n<td headers=\"g1  b\" class=\"gt_row gt_left\" style=\"border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: left;\" valign=\"middle\" align=\"left\">cd</td></tr>\n    <tr class=\"gt_group_heading_row\" style=\"border-style: none;\">\n      <th colspan=\"2\" class=\"gt_group_heading\" style=\"border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; color: #333333; background-color: #FFFFFF; font-size: 100%; font-weight: initial; text-transform: inherit; border-top-style: solid; border-top-width: 2px; border-top-color: #D3D3D3; border-bottom-style: solid; border-bottom-width: 2px; border-bottom-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; text-align: left; font-style: oblique;\" scope=\"colgroup\" id=\"g2\" bgcolor=\"#FFFFFF\" valign=\"middle\" align=\"left\">g2</th>\n    </tr>\n    <tr class=\"gt_row_group_first\" style=\"border-style: none;\"><td headers=\"g2  a\" class=\"gt_row gt_right\" style=\"border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums; border-top-width: 2px;\" valign=\"middle\" align=\"right\">543.00</td>\n<td headers=\"g2  b\" class=\"gt_row gt_left\" style=\"border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: left; border-top-width: 2px;\" valign=\"middle\" align=\"left\">abcde</td></tr>\n    <tr style=\"border-style: none;\"><td headers=\"g2  a\" class=\"gt_row gt_right\" style=\"border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;\" valign=\"middle\" align=\"right\">78.00</td>\n<td headers=\"g2  b\" class=\"gt_row gt_left\" style=\"border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: left;\" valign=\"middle\" align=\"left\">hj</td></tr>\n  </tbody>\n  \n</table>\n</div>"
  )
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
  expect_identical(
    substr(as.character(out), 5000, 6500),
    "ign=\"center\">(SE)</th>\n      <th class=\"gt_col_heading gt_columns_bottom_border gt_center\" rowspan=\"1\" colspan=\"1\" scope=\"col\" id=\"Coefficient-(lm2)\" style=\"border-style: none; color: #333333; background-color: #FFFFFF; font-size: 100%; font-weight: normal; text-transform: inherit; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: bottom; padding-top: 5px; padding-bottom: 6px; padding-left: 5px; padding-right: 5px; overflow-x: hidden; text-align: center;\" bgcolor=\"#FFFFFF\" valign=\"bottom\" align=\"center\">Coefficient</th>\n      <th class=\"gt_col_heading gt_columns_bottom_border gt_center\" rowspan=\"1\" colspan=\"1\" scope=\"col\" id=\"a(SE)-(lm2)\" style=\"border-style: none; color: #333333; background-color: #FFFFFF; font-size: 100%; font-weight: normal; text-transform: inherit; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: bottom; padding-top: 5px; padding-bottom: 6px; padding-left: 5px; padding-right: 5px; overflow-x: hidden; text-align: center;\" bgcolor=\"#FFFFFF\" valign=\"bottom\" align=\"center\">(SE)</th>\n    </tr>\n  </thead>\n  <tbody class=\"gt_table_body\" style=\"border-style: none; border-top-style: solid; border-top-width: 2px; border-top-color: #D3D3D3; border-bottom-style: solid; border-bottom-width: 2px; border-bottom-color: #D3D3D3;\">"
  )
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

  expect_identical(
    as.data.frame(mp)$Coefficient,
    sprintf("%.2f", coef(model)[-1])
  )

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

  # manually validate correct coefficients
  junk <- capture.output(export_table(
    mp,
    format = "text",
    row_groups = groups,
    table_width = Inf
  ))
  # extract coefficients from table
  out <- trimws(substr(junk, 25, 29)[3:14])
  # remove empty strings
  out <- out[nzchar(out)]
  # compare to desired order from original model coeffients
  expect_identical(
    sprintf("%.2f", coef(model)[c(6, 7, 5, 2, 9, 10, 3, 4, 8)]),
    out
  )

  mp <- as.data.frame(format(parameters::model_parameters(
    model,
    drop = "^\\(Intercept"
  )))
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

test_that("export_table, empty strings remove captions for lists of tables", {
  d1 <- data.frame(x = 1:2)
  attr(d1, "table_caption") <- "# Fixed Effects"
  attr(d1, "table_subtitle") <- "Subtitle One"
  attr(d1, "table_footer") <- "Footer One"
  d2 <- data.frame(x = 3:4)
  attr(d2, "table_caption") <- "# Random Effects"
  attr(d2, "table_footer") <- "Footer Two"
  l <- list(d1, d2)

  for (fmt in c("text", "markdown")) {
    out <- paste(export_table(l, format = fmt), collapse = "\n")
    expect_match(out, "Fixed Effects", fixed = TRUE)
    expect_match(out, "Subtitle One", fixed = TRUE)
    expect_match(out, "Footer Two", fixed = TRUE)

    # "" removes captions, subtitles and footers stored as attributes
    out <- paste(export_table(l, format = fmt, caption = ""), collapse = "\n")
    expect_no_match(out, "Effects", fixed = TRUE)
    expect_no_match(out, "Subtitle One", fixed = TRUE)
    expect_match(out, "Footer One", fixed = TRUE)
    out <- paste(export_table(l, format = fmt, title = ""), collapse = "\n")
    expect_no_match(out, "Effects", fixed = TRUE)
    out <- paste(
      export_table(l, format = fmt, title = "", caption = NULL),
      collapse = "\n"
    )
    expect_no_match(out, "Effects", fixed = TRUE)
    out <- paste(export_table(l, format = fmt, subtitle = ""), collapse = "\n")
    expect_no_match(out, "Subtitle One", fixed = TRUE)
    expect_match(out, "Fixed Effects", fixed = TRUE)
    out <- paste(export_table(l, format = fmt, footer = ""), collapse = "\n")
    expect_no_match(out, "Footer", fixed = TRUE)
    expect_match(out, "Random Effects", fixed = TRUE)

    # a list of captions or footers removes them per table
    out <- paste(
      export_table(l, format = fmt, caption = list("", "Second"), footer = list("", "")),
      collapse = "\n"
    )
    expect_no_match(out, "Fixed Effects", fixed = TRUE)
    expect_match(out, "Random Effects", fixed = TRUE)
    expect_no_match(out, "Footer", fixed = TRUE)
  }
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

# returns the <table> element of a gt table as lines, with the random table
# id replaced, so that snapshots are stable
gt_table_lines <- function(x) {
  out <- as.character(gt::as_raw_html(x, inline_css = FALSE))
  table_id <- regmatches(out, regexpr("(?<=id=\")[a-z]+(?=\")", out, perl = TRUE))
  out <- gsub(table_id, "ID", out, fixed = TRUE)
  out <- regmatches(out, regexpr("<table[\\s\\S]*</table>", out, perl = TRUE))
  out <- trimws(strsplit(out, "\n", fixed = TRUE)[[1]])
  out[nzchar(out)]
}

test_that("export_table, html output for lists with group columns", {
  skip_if_not_installed("gt")
  # tables that already have an "Effects" column
  d_fixed <- data.frame(
    Parameter = c("(Intercept)", "x"),
    Coefficient = c(1.5, 2),
    Effects = "fixed",
    stringsAsFactors = FALSE
  )
  attr(d_fixed, "table_caption") <- "Fixed Effects"
  d_random <- data.frame(
    Parameter = "SD",
    Coefficient = 0.3,
    Effects = "random",
    stringsAsFactors = FALSE
  )
  attr(d_random, "table_caption") <- "Random Effects"
  expect_snapshot(gt_table_lines(
    export_table(list(d_fixed, d_random), format = "html")
  ))
  # tables that already have a "Component" column
  d_cond <- data.frame(
    Parameter = "x",
    Coefficient = 2,
    Component = "conditional",
    stringsAsFactors = FALSE
  )
  attr(d_cond, "table_caption") <- "Conditional"
  d_zi <- data.frame(
    Parameter = "x",
    Coefficient = 0.1,
    Component = "zero_inflated",
    stringsAsFactors = FALSE
  )
  attr(d_zi, "table_caption") <- "Zero-Inflated"
  expect_snapshot(gt_table_lines(
    export_table(list(d_cond, d_zi), format = "html")
  ))
  # a list footer gives one note per non-empty entry (an error on main)
  out <- export_table(
    list(d_fixed, d_random),
    format = "html",
    footer = list("", "F2")
  )
  expect_identical(
    vapply(compact_list(out[["_source_notes"]]), as.character, character(1)),
    "F2"
  )
  # a list footer with only "" entries removes the footer attribute of the
  # first table
  d_fixed_f <- d_fixed
  attr(d_fixed_f, "table_footer") <- "Footer Fixed"
  out <- export_table(
    list(d_fixed_f, d_random),
    format = "html",
    footer = list("", "")
  )
  expect_length(compact_list(out[["_source_notes"]]), 0)
  # NULL entries keep it, as in text output
  out <- export_table(
    list(d_fixed_f, d_random),
    format = "html",
    footer = list(NULL, "")
  )
  expect_identical(
    vapply(compact_list(out[["_source_notes"]]), as.character, character(1)),
    "Footer Fixed"
  )
})

test_that("export_table, html output for a colored footer with new lines", {
  skip_if_not_installed("gt")
  d_footer <- data.frame(x = 1:2)
  attr(d_footer, "table_footer") <- c("\nF\n", "yellow")
  expect_snapshot(gt_table_lines(export_table(d_footer, format = "html")))
})

# returns the value of an option of a gt table
gt_table_option <- function(x, option) {
  opts <- x[["_options"]]
  opts$value[[which(opts$parameter == option)]]
}

# returns the title, the source notes and the row groups of a gt table
gt_parts <- function(x) {
  list(
    title = x[["_heading"]]$title,
    notes = vapply(compact_list(x[["_source_notes"]]), as.character, character(1)),
    groups = as.character(x[["_row_groups"]])
  )
}

test_that("export_table, html footers for lists of tables", {
  skip_if_not_installed("gt")
  d_one <- data.frame(x = 1:2)
  d_two <- data.frame(x = 3:4)
  d_one_f <- d_one
  attr(d_one_f, "table_footer") <- "Footer One"
  d_two_f <- d_two
  attr(d_two_f, "table_footer") <- "Footer Two"

  # a list footer gives one note per table (an error on main)
  out <- export_table(list(d_one, d_two), format = "html", footer = list("F1", "F2"))
  expect_identical(gt_parts(out)$notes, c("F1", "F2"))

  # footers stored as attributes are kept (on main, only the first one)
  out <- export_table(list(d_one_f, d_two_f), format = "html")
  expect_identical(gt_parts(out)$notes, c("Footer One", "Footer Two"))

  # a string footer comes after the table footers
  out <- export_table(list(d_one_f, d_two_f), format = "html", footer = "Main")
  expect_identical(gt_parts(out)$notes, c("Footer One", "Footer Two", "Main"))

  # footer = "" removes all footers, also those stored as attributes
  out <- export_table(list(d_one_f, d_two_f), format = "html", footer = "")
  expect_length(gt_parts(out)$notes, 0)

  # a "" entry in a list footer removes the footer of that table only
  out <- export_table(list(d_one_f, d_two_f), format = "html", footer = list("", "F2"))
  expect_identical(gt_parts(out)$notes, "Footer Two")

  # a list footer of the wrong length is not used
  out <- export_table(list(d_one, d_two), format = "html", footer = list("F1"))
  expect_length(gt_parts(out)$notes, 0)

  # a "" entry removes the footer also in a list of the wrong length
  out <- export_table(list(d_one_f, d_two_f), format = "html", footer = list(""))
  expect_identical(gt_parts(out)$notes, "Footer Two")

  # an attribute wins over a non-empty list entry
  out <- export_table(list(d_one_f, d_two), format = "html", footer = list("F1", "F2"))
  expect_identical(gt_parts(out)$notes, c("Footer One", "F2"))

  # leading and trailing new lines are removed, inner ones become line breaks
  d_newline <- d_one
  attr(d_newline, "table_footer") <- "\nLine one\nLine two\n"
  out <- export_table(list(d_newline, d_two), format = "html")
  expect_identical(gt_parts(out)$notes, "Line one<br>Line two")

  # a colored multi-line footer list gives one note, joined like text output
  d_colored <- d_one
  attr(d_colored, "table_footer") <- list(
    c("\nA yellow line", "yellow"),
    c("\nA red line", "red")
  )
  out <- export_table(list(d_colored, d_two), format = "html")
  expect_identical(gt_parts(out)$notes, "A yellow line<br>A red line")
})

test_that("export_table, html captions for lists of tables", {
  skip_if_not_installed("gt")
  d_one <- data.frame(x = 1:2)
  d_two <- data.frame(x = 3:4)
  d_one_c <- d_one
  attr(d_one_c, "table_caption") <- "Caption One"
  d_two_c <- d_two
  attr(d_two_c, "table_caption") <- "Caption Two"

  # a list caption labels the row groups (a two-element title on main)
  out <- export_table(list(d_one, d_two), format = "html", caption = list("C1", "C2"))
  expect_null(gt_parts(out)$title)
  expect_identical(gt_parts(out)$groups, c("C1", "C2"))

  # caption attributes label the row groups, and there is no title (on main,
  # the title repeated the first label)
  out <- export_table(list(d_one_c, d_two_c), format = "html")
  expect_null(gt_parts(out)$title)
  expect_identical(gt_parts(out)$groups, c("Caption One", "Caption Two"))

  # a string caption is the title
  out <- export_table(list(d_one_c, d_two_c), format = "html", caption = "Main")
  expect_identical(gt_parts(out)$title, "Main")
  expect_identical(gt_parts(out)$groups, c("Caption One", "Caption Two"))

  # title wins over caption
  out <- export_table(
    list(d_one_c, d_two_c),
    format = "html",
    title = "Title",
    caption = "Caption"
  )
  expect_identical(gt_parts(out)$title, "Title")

  # a list title takes the place of a NULL caption
  out <- export_table(list(d_one, d_two), format = "html", title = list("T1", "T2"))
  expect_null(gt_parts(out)$title)
  expect_identical(gt_parts(out)$groups, c("T1", "T2"))

  # a table_title attribute is a caption, too
  d_one_t <- d_one
  attr(d_one_t, "table_title") <- "Title One"
  out <- export_table(list(d_one_t, d_two_c), format = "html")
  expect_identical(gt_parts(out)$groups, c("Title One", "Caption Two"))

  # an attribute wins over a non-empty list entry
  out <- export_table(list(d_one_c, d_two), format = "html", caption = list("C1", "C2"))
  expect_identical(gt_parts(out)$groups, c("Caption One", "C2"))

  # a "" entry removes the caption of that table, also an attribute
  out <- export_table(list(d_one_c, d_two_c), format = "html", caption = list("", "C2"))
  expect_identical(gt_parts(out)$groups, c("", "Caption Two"))

  # a caption is the first string of a two-string (colored) attribute
  d_one_col <- d_one
  attr(d_one_col, "table_caption") <- c("A", "blue")
  d_two_col <- d_two
  attr(d_two_col, "table_caption") <- c("B", "red")
  out <- export_table(list(d_one_col, d_two_col), format = "html")
  expect_identical(gt_parts(out)$groups, c("A", "B"))

  # a table without a caption gets the label "" (an error on main)
  out <- export_table(list(d_one_c, d_two), format = "html")
  expect_identical(gt_parts(out)$groups, c("Caption One", ""))

  # one table: its caption is the title, and there are no row groups
  out <- export_table(list(d_one_c), format = "html")
  expect_identical(gt_parts(out)$title, "Caption One")
  expect_length(gt_parts(out)$groups, 0)

  # caption = "" or title = "" removes the title and the row groups
  out <- export_table(list(d_one_c, d_two_c), format = "html", caption = "")
  expect_null(gt_parts(out)$title)
  expect_length(gt_parts(out)$groups, 0)
  out <- export_table(list(d_one_c, d_two_c), format = "html", title = "")
  expect_null(gt_parts(out)$title)
  expect_length(gt_parts(out)$groups, 0)
})

test_that("export_table, html passes gt::gt() arguments from ...", {
  skip_if_not_installed("gt")
  d_one <- data.frame(x = 1:2, y = c("a", "b"), stringsAsFactors = FALSE)
  d_two <- data.frame(x = 3:4, y = c("c", "d"), stringsAsFactors = FALSE)
  attr(d_one, "table_caption") <- "Caption One"
  attr(d_two, "table_caption") <- "Caption Two"

  # "id" sets the table id, for a data frame and for a list
  out <- export_table(d_one, format = "html", id = "tab1")
  expect_identical(gt_table_option(out, "table_id"), "tab1")
  out <- export_table(list(d_one, d_two), format = "html", id = "tab1")
  expect_identical(gt_table_option(out, "table_id"), "tab1")

  # "rowname_col" puts that column into the stub
  out <- export_table(d_one, format = "html", rowname_col = "y")
  expect_identical(out[["_boxhead"]]$type[out[["_boxhead"]]$var == "y"], "stub")
  # also when row groups are active
  out <- export_table(
    d_one,
    format = "html",
    rowname_col = "y",
    row_groups = list(G = 2)
  )
  expect_identical(out[["_boxhead"]]$type[out[["_boxhead"]]$var == "y"], "stub")
  # the row group headers stay italic when the first column is the stub: the
  # same rows are styled, in the stub instead of the body
  d_stub <- data.frame(y = c("a", "b"), x = 1:2, stringsAsFactors = FALSE)
  out <- export_table(
    d_stub,
    format = "html",
    rowname_col = "y",
    row_groups = list(G = 2)
  )
  out_body <- export_table(d_stub, format = "html", row_groups = list(G = 2))
  expect_identical(unique(out[["_styles"]]$locname), "stub")
  expect_identical(out[["_styles"]]$rownum, out_body[["_styles"]]$rownum)

  # gt shows the stub first, so "align" follows the display order when
  # "rowname_col" names a column that is not the first: b, a, c
  d_three <- data.frame(
    a = c("x", "y"),
    b = c("1.0", "2.0"),
    c = c("3.0", "4.0"),
    stringsAsFactors = FALSE
  )
  col_align <- function(x) {
    stats::setNames(x[["_boxhead"]]$column_align, x[["_boxhead"]]$var)
  }
  out <- export_table(d_three, format = "html", align = "lcr", rowname_col = "b")
  expect_identical(col_align(out), c(a = "center", b = "left", c = "right"))
  # the default "firstleft" left-aligns the stub
  out <- export_table(d_three, format = "html", rowname_col = "b")
  expect_identical(col_align(out), c(a = "center", b = "left", c = "center"))
  # a "Component" column with two values becomes row groups and is not shown,
  # so no "align" character goes to it
  d_comp <- cbind(Component = c("conditional", "zero_inflated"), d_three)
  out <- export_table(d_comp, format = "html", align = "lcr", rowname_col = "b")
  expect_identical(
    col_align(out)[c("a", "b", "c")],
    c(a = "center", b = "left", c = "right")
  )
  # two "rowname_col" columns are shown first, in the order given: c, b, a
  out <- export_table(
    d_three,
    format = "html",
    align = "lcr",
    rowname_col = c("c", "b")
  )
  expect_identical(
    col_align(out)[c("a", "b", "c")],
    c(a = "right", b = "center", c = "left")
  )
  # without "rowname_col", "align" follows the column order
  out <- export_table(d_three, format = "html", align = "lcr")
  expect_identical(col_align(out), c(a = "left", b = "center", c = "right"))

  # arguments that gt::gt() does not have are ignored
  expect_identical(
    export_table(d_one, format = "html", not_a_gt_argument = TRUE),
    export_table(d_one, format = "html")
  )
  # also when their name matches an internal argument, fully or partly
  expect_identical(
    export_table(d_one, format = "html", final = d_two),
    export_table(d_one, format = "html")
  )
  expect_identical(
    export_table(d_one, format = "html", fi = d_two),
    export_table(d_one, format = "html")
  )
})

test_that("export_table, html edge cases for lists of tables", {
  skip_if_not_installed("gt")
  d_one <- data.frame(x = 1:2)
  d_two <- data.frame(x = 3:4)
  attr(d_one, "table_caption") <- "Caption One"
  attr(d_two, "table_caption") <- "Caption Two"

  # caption = "" removes title and row groups, also when title is given
  out <- export_table(list(d_one, d_two), format = "html", title = "T", caption = "")
  expect_null(gt_parts(out)$title)
  expect_length(gt_parts(out)$groups, 0)
  # title = "" removes them, also when caption is given
  out <- export_table(list(d_one, d_two), format = "html", title = "", caption = "C")
  expect_null(gt_parts(out)$title)
  expect_length(gt_parts(out)$groups, 0)

  # a data frame with no rows and a list caption
  out <- export_table(
    list(data.frame(x = integer(0)), data.frame(x = 3:4)),
    format = "html",
    caption = list("A", "B")
  )
  expect_s3_class(out, "gt_tbl")

  # the subtitle of the first table shows without a main caption, under an
  # empty title (on main, the first caption was the title)
  d_sub <- d_one
  attr(d_sub, "table_subtitle") <- "Sub One"
  out <- export_table(list(d_sub, d_two), format = "html")
  expect_identical(gt_parts(out)$title, "")
  expect_identical(out[["_heading"]]$subtitle, "Sub One")
  expect_identical(gt_parts(out)$groups, c("Caption One", "Caption Two"))

  # a NULL element in a footer list is skipped, not shown as "NA"
  d_null <- data.frame(x = 1:2)
  attr(d_null, "table_footer") <- list("x", NULL, "y")
  out <- export_table(list(d_null, d_two), format = "html")
  expect_identical(gt_parts(out)$notes, "xy")
})

test_that("export_table, html list footer for a single data frame", {
  skip_if_not_installed("gt")
  # a list footer gives one note, joined like text output (an error on main)
  d_colored <- data.frame(x = 1:2)
  attr(d_colored, "table_footer") <- list(
    c("\nA yellow line", "yellow"),
    c("\nA red line", "red")
  )
  out <- export_table(d_colored, format = "html")
  expect_identical(gt_parts(out)$notes, "A yellow line<br>A red line")
})

test_that("export_table, tinytable output for lists", {
  skip_if_not_installed("tinytable")
  d_one <- data.frame(x = 1:2)
  attr(d_one, "table_caption") <- "Table One"
  attr(d_one, "table_footer") <- "Footer One"
  d_two <- data.frame(x = 3:4)
  attr(d_two, "table_caption") <- "Table Two"
  expect_snapshot(export_table(list(d_one, d_two), format = "tt", table_width = Inf))
})


test_that("export_table, tinytable output that lists of tables do not change", {
  skip_if_not_installed("tinytable")
  # one-column tables with caption attributes
  tt_one_a <- data.frame(x = 1:2)
  attr(tt_one_a, "table_caption") <- "One A"
  tt_one_b <- data.frame(x = 3:4)
  attr(tt_one_b, "table_caption") <- "One B"
  expect_snapshot(export_table(
    list(tt_one_a, tt_one_b),
    format = "tt",
    table_width = Inf
  ))

  # tables with an Effects column
  tt_eff_a <- data.frame(
    Parameter = c("a", "b"),
    Coefficient = 1:2,
    Effects = "fixed",
    stringsAsFactors = FALSE
  )
  attr(tt_eff_a, "table_caption") <- "Fixed"
  tt_eff_b <- data.frame(
    Parameter = "c",
    Coefficient = 3,
    Effects = "random",
    stringsAsFactors = FALSE
  )
  attr(tt_eff_b, "table_caption") <- "Random"
  expect_snapshot(export_table(
    list(tt_eff_a, tt_eff_b),
    format = "tt",
    table_width = Inf
  ))

  # tables with a Component column
  tt_comp_a <- data.frame(
    Parameter = c("a", "b"),
    Coefficient = 1:2,
    Component = "conditional",
    stringsAsFactors = FALSE
  )
  attr(tt_comp_a, "table_caption") <- "Conditional"
  tt_comp_b <- data.frame(
    Parameter = "c",
    Coefficient = 3,
    Component = "zero_inflated",
    stringsAsFactors = FALSE
  )
  attr(tt_comp_b, "table_caption") <- "Zero-Inflated"
  expect_snapshot(export_table(
    list(tt_comp_a, tt_comp_b),
    format = "tt",
    table_width = Inf
  ))

  # `by = "group"` with caption attributes
  tt_by_a <- data.frame(x = 1:2, y = 5:6)
  attr(tt_by_a, "table_caption") <- "By A"
  tt_by_b <- data.frame(x = 3:4, y = 7:8)
  attr(tt_by_b, "table_caption") <- "By B"
  expect_snapshot(export_table(
    list(tt_by_a, tt_by_b),
    format = "tt",
    by = "group",
    table_width = Inf
  ))
})


test_that("export_table, tinytable output that data frames do not change", {
  skip_if_not_installed("tinytable")
  tt_df <- data.frame(x = 1:2, y = c("a", "b"), stringsAsFactors = FALSE)
  # no footer
  expect_snapshot(export_table(tt_df, format = "tt", table_width = Inf))
  # a string footer with no leading or trailing new line
  expect_snapshot(export_table(
    tt_df,
    format = "tt",
    footer = "A note",
    table_width = Inf
  ))
})


# caption, notes, column names and row groups of a tinytable object. The row
# groups are the values of the first column of each group, by group label.
# `@group_index_i` holds the position of each group header in the table with
# the header rows, so the first data row of group i is at `index[i] - i + 1`.
tt_parts <- function(x) {
  index <- x@group_index_i
  groups <- NULL
  if (length(index)) {
    first_rows <- index - seq_along(index) + 1
    last_rows <- c(first_rows[-1] - 1, nrow(x@data))
    groups <- lapply(seq_along(index), function(i) {
      x@data[[1]][first_rows[i]:last_rows[i]]
    })
    names(groups) <- names(index)
  }
  list(
    caption = x@caption,
    notes = unlist(x@notes),
    names = x@names,
    groups = groups
  )
}

# a table with two columns, its first column holds `rows`
tt_table <- function(rows, caption = NULL, title = NULL, footer = NULL) {
  out <- data.frame(Parameter = rows, Value = seq_along(rows))
  attr(out, "table_caption") <- caption
  attr(out, "table_title") <- title
  attr(out, "table_footer") <- footer
  out
}


test_that("export_table, tinytable notes for lists of tables", {
  skip_if_not_installed("tinytable")
  t_a <- tt_table(c("a1", "a2"))
  t_b <- tt_table("b1")

  # a list footer, one entry per table
  out <- export_table(list(t_a, t_b), format = "tt", footer = list("Foot A", "Foot B"))
  expect_s4_class(out, "tinytable")
  expect_identical(tt_parts(out)$notes, c("Foot A", "Foot B"))

  # footer attributes of both tables (main kept only the first)
  f_a <- tt_table(c("a1", "a2"), footer = "Attr A")
  f_b <- tt_table("b1", footer = "Attr B")
  out <- export_table(list(f_a, f_b), format = "tt")
  expect_identical(tt_parts(out)$notes, c("Attr A", "Attr B"))

  # footer attributes and a string footer, which comes last
  out <- export_table(list(f_a, f_b), format = "tt", footer = "Main")
  expect_identical(tt_parts(out)$notes, c("Attr A", "Attr B", "Main"))

  # footer = "" removes the footer attributes
  out <- export_table(list(f_a, f_b), format = "tt", footer = "")
  expect_length(tt_parts(out)$notes, 0)

  # a "" list entry removes the attribute of its table
  out <- export_table(list(f_a, t_b), format = "tt", footer = list("", "Foot B"))
  expect_identical(tt_parts(out)$notes, "Foot B")

  # a list footer of the wrong length is not used, the attribute still is
  out <- export_table(list(f_a, t_b), format = "tt", footer = list("x", "y", "z"))
  expect_identical(tt_parts(out)$notes, "Attr A")

  # leading and trailing new lines are removed
  n_a <- tt_table(c("a1", "a2"), footer = "\nAttr A\n")
  out <- export_table(list(n_a, t_b), format = "tt")
  expect_identical(tt_parts(out)$notes, "Attr A")

  # a colored multi-line footer list gives one note, its text only
  c_a <- tt_table(
    c("a1", "a2"),
    footer = list(c("\nLine 1", "yellow"), c("\nLine 2", "red"))
  )
  out <- export_table(list(c_a, t_b), format = "tt")
  expect_identical(tt_parts(out)$notes, "Line 1\nLine 2")

  # a list that is not bound (a table has an Effects column) keeps one note
  # for each element of a list footer, as on main
  e_a <- t_a
  e_a$Effects <- "fixed"
  e_b <- t_b
  e_b$Effects <- "random"
  out <- export_table(list(e_a, e_b), format = "tt", footer = list("Foot A", "Foot B"))
  expect_identical(tt_parts(out)$notes, c("Foot A", "Foot B"))
})


test_that("export_table, tinytable row groups for lists of tables", {
  skip_if_not_installed("tinytable")
  t_a <- tt_table(c("a1", "a2"))
  t_b <- tt_table("b1")
  cap_a <- tt_table(c("a1", "a2"), caption = "A")
  cap_b <- tt_table("b1", caption = "B")
  ab_groups <- list(A = c("a1", "a2"), B = "b1")

  # caption attributes on every table (on main, a "group" column and the
  # first caption as title)
  out <- tt_parts(export_table(list(cap_a, cap_b), format = "tt"))
  expect_identical(out$groups, ab_groups)
  expect_null(out$caption)
  expect_identical(out$names, c("Parameter", "Value"))

  # a list caption (an error on main)
  out <- tt_parts(export_table(list(t_a, t_b), format = "tt", caption = list("A", "B")))
  expect_identical(out$groups, ab_groups)
  expect_null(out$caption)

  # a table_title attribute on one table and table_caption on the other
  # (an error on main)
  title_a <- tt_table(c("a1", "a2"), title = "A")
  out <- tt_parts(export_table(list(title_a, cap_b), format = "tt"))
  expect_identical(out$groups, ab_groups)

  # title and caption together, title is the main caption
  out <- tt_parts(export_table(
    list(cap_a, cap_b),
    format = "tt",
    title = "Main",
    caption = "Cap"
  ))
  expect_identical(out$groups, ab_groups)
  expect_identical(out$caption, "Main")

  # a list title takes the place of a missing caption
  out <- tt_parts(export_table(list(t_a, t_b), format = "tt", title = list("A", "B")))
  expect_identical(out$groups, ab_groups)
  expect_null(out$caption)

  # a "" list entry removes the caption attribute of its table
  out <- tt_parts(export_table(
    list(cap_a, cap_b),
    format = "tt",
    caption = list("", "X")
  ))
  expect_identical(out$groups, list(" " = c("a1", "a2"), B = "b1"))

  # captions "A" and none
  out <- tt_parts(export_table(list(cap_a, t_b), format = "tt"))
  expect_identical(out$groups, list(A = c("a1", "a2"), " " = "b1"))

  # an NA caption is no caption, and a caption " " shares its row group
  na_b <- tt_table("b1", caption = NA_character_)
  space_c <- tt_table("c1", caption = " ")
  out <- tt_parts(export_table(list(cap_a, na_b, space_c), format = "tt"))
  expect_identical(out$groups, list(A = c("a1", "a2"), " " = c("b1", "c1")))

  # the same caption on two tables that are not next to each other
  cap_c <- tt_table("c1", caption = "A")
  out <- tt_parts(export_table(list(cap_a, cap_b, cap_c), format = "tt"))
  expect_identical(out$groups, list(A = c("a1", "a2", "c1"), B = "b1"))

  # one caption value: no row groups, the caption is the title
  out <- tt_parts(export_table(list(cap_a), format = "tt"))
  expect_null(out$groups)
  expect_identical(out$caption, "A")
  out <- tt_parts(export_table(list(cap_a, cap_c), format = "tt"))
  expect_null(out$groups)
  expect_identical(out$caption, "A")
  out <- tt_parts(export_table(list(cap_a, cap_c), format = "tt", caption = "Main"))
  expect_null(out$groups)
  expect_identical(out$caption, "Main")
  out <- tt_parts(export_table(list(t_a, t_b), format = "tt"))
  expect_null(out$groups)
  expect_null(out$caption)
  expect_identical(out$names, c("Parameter", "Value"))

  # caption = "" removes the caption attributes and row groups
  out <- tt_parts(export_table(list(cap_a, cap_b), format = "tt", caption = ""))
  expect_null(out$groups)
  expect_null(out$caption)
})


test_that("export_table, tinytable column names for lists with row groups", {
  skip_if_not_installed("tinytable")
  cap_a <- tt_table(c("a1", "a2"), caption = "A")
  cap_b <- tt_table("b1", caption = "B")

  # an unnamed `column_names` vector names the shown columns, not the hidden
  # column of the row groups (an error with two names)
  out <- tt_parts(export_table(
    list(cap_a, cap_b),
    format = "tt",
    column_names = c("P", "V")
  ))
  expect_identical(out$names, c("P", "V"))
  expect_identical(out$groups, list(A = c("a1", "a2"), B = "b1"))

  # a vector with a name for the hidden column has the wrong length
  expect_error(
    export_table(list(cap_a, cap_b), format = "tt", column_names = c("P", "V", "G")),
    regex = "Number of names in `column_names`"
  )

  # a named vector renames its columns only
  out <- tt_parts(export_table(
    list(cap_a, cap_b),
    format = "tt",
    column_names = c(Value = "V")
  ))
  expect_identical(out$names, c("Parameter", "V"))
  expect_identical(out$groups, list(A = c("a1", "a2"), B = "b1"))
})


test_that("export_table, tinytable notes for data frames", {
  skip_if_not_installed("tinytable")
  # a colored footer gives its text only (two notes on main)
  d <- tt_table(c("a1", "a2"), footer = c("Footer", "yellow"))
  expect_identical(tt_parts(export_table(d, format = "tt"))$notes, "Footer")
  # a list of colored lines gives one note (an error on main)
  d <- tt_table(c("a1", "a2"), footer = list(c("Line 1", "yellow"), c("\nLine 2", "red")))
  expect_identical(tt_parts(export_table(d, format = "tt"))$notes, "Line 1\nLine 2")
  # leading new lines are removed
  d <- tt_table(c("a1", "a2"))
  expect_identical(
    tt_parts(export_table(d, format = "tt", footer = "\nNote"))$notes,
    "Note"
  )
})
