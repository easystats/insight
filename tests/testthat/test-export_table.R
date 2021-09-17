if (requiet("testthat") && requiet("insight")) {
  d <- data.frame(a = c(1.3, 2, 543), b = c("ab", "cd", "abcde"), stringsAsFactors = FALSE)

  test_that("export_table", {
    out <- capture.output(cat(export_table(d)))
    expect_equal(out, c(
      "     a |     b", "--------------", "  1.30 |    ab",
      "  2.00 |    cd", "543.00 | abcde"
    ))
  })

  test_that("export_table", {
    out <- capture.output(cat(export_table(d, sep = " ", header = "*", digits = 1)))
    expect_equal(out, c(
      "    a     b", "***********", "  1.3    ab",
      "  2.0    cd", "543.0 abcde"
    ))
  })

  test_that("export_table", {
    out <- export_table(d, format = "md")
    expect_equal(out, structure(c(
      "|      a|     b|", "|------:|-----:|",
      "|   1.30|    ab|", "|   2.00|    cd|",
      "| 543.00| abcde|"
    ),
    format = "pipe",
    class = c("knitr_kable", "character")
    ))
  })

  d <- data.frame(a = c(1.3, 2, 543), b = c("ab", "cd", "abcde"), stringsAsFactors = FALSE)
  attr(d, "table_caption") <- "Table Title"

  test_that("export_table", {
    out <- export_table(d, format = "md")
    expect_equal(
      out,
      structure(c(
        "Table: Table Title", "", "|      a|     b|", "|------:|-----:|",
        "|   1.30|    ab|", "|   2.00|    cd|", "| 543.00| abcde|"
      ),
      format = "pipe",
      class = c("knitr_kable", "character")
      )
    )
  })

  d <- data.frame(a = c(1.3, 2, 543), b = c("ab", "cd", "abcde"), stringsAsFactors = FALSE)
  attr(d, "table_title") <- "Table Title"

  test_that("export_table", {
    out <- export_table(d, format = "md")
    expect_equal(
      out,
      structure(c(
        "Table: Table Title", "", "|      a|     b|", "|------:|-----:|",
        "|   1.30|    ab|", "|   2.00|    cd|", "| 543.00| abcde|"
      ),
      format = "pipe",
      class = c("knitr_kable", "character")
      )
    )
  })

  d <- data.frame(a = c(1.3, 2, 543), b = c("ab", "cd", "abcde"), stringsAsFactors = FALSE)

  test_that("export_table", {
    out <- export_table(d, format = "md", title = "Table Title")
    expect_equal(
      out,
      structure(c(
        "Table: Table Title", "", "|      a|     b|", "|------:|-----:|",
        "|   1.30|    ab|", "|   2.00|    cd|", "| 543.00| abcde|"
      ),
      format = "pipe",
      class = c("knitr_kable", "character")
      )
    )
  })

  d <- data.frame(a = c(1.3, 2, 543), b = c("ab", "cd", "abcde"), stringsAsFactors = FALSE)
  attr(d, "table_caption") <- "Table Title"
  attr(d, "table_footer") <- list("first", "second", "third")

  test_that("export_table", {
    out <- export_table(d, format = "md")
    expect_equal(
      out,
      structure(c(
        "Table: Table Title", "", "|      a|     b|", "|------:|-----:|",
        "|   1.30|    ab|", "|   2.00|    cd|", "| 543.00| abcde|",
        "first", "second", "third"
      ),
      format = "pipe",
      class = c("knitr_kable", "character")
      )
    )
  })
}
