skip_on_ci()

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
