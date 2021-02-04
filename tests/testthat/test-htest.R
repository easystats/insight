if (require("testthat") && require("insight")) {
  x <- t.test(1:3, c(1, 1:3))
  test_that("get_data.t-test", {
    expect_equal(get_data(x)$x, c(1, 2, 3, NA))
  })

  dat <<- matrix(c(794, 86, 150, 570),
    nrow = 2,
    dimnames = list(
      "1st Survey" = c("Approve", "Disapprove"),
      "2nd Survey" = c("Approve", "Disapprove")
    )
  )
  m <- mcnemar.test(dat)
  test_that("get_data.mcnemar", {
    expect_equal(
      get_data(m),
      structure(c(794, 86, 150, 570),
        .Dim = c(2L, 2L),
        .Dimnames = list(
          `1st Survey` = c("Approve", "Disapprove"),
          `2nd Survey` = c("Approve", "Disapprove")
        ), class = "table"
      )
    )
  })


  TeaTasting <<-
    matrix(c(3, 1, 1, 3),
      nrow = 2,
      dimnames = list(
        Guess = c("Milk", "Tea"),
        Truth = c("Milk", "Tea")
      )
    )
  m <- fisher.test(TeaTasting, alternative = "greater")
  test_that("get_data.fisher", {
    expect_equal(
      get_data(m),
      structure(c(3, 1, 1, 3),
        .Dim = c(2L, 2L),
        .Dimnames = list(
          Guess = c("Milk", "Tea"),
          Truth = c("Milk", "Tea")
        ), class = "table"
      )
    )
  })


  wb <<- aggregate(warpbreaks$breaks,
    by = list(
      w = warpbreaks$wool,
      t = warpbreaks$tension
    ),
    FUN = mean
  )
  m <- friedman.test(wb$x, wb$w, wb$t)
  test_that("get_data.freedman", {
    expect_equal(
      get_data(m),
      data.frame(
        x = c(
          44.5555555555556, 28.2222222222222, 24,
          28.7777777777778, 24.5555555555556, 18.7777777777778
        ),
        w = c(1L, 2L, 1L, 2L, 1L, 2L),
        t = c(1L, 1L, 2L, 2L, 3L, 3L)
      ),
      tolerance = 1e-3,
      ignore_attr = TRUE
    )
  })
}
