if (require("testthat") && require("insight")) {
  x <- t.test(1:3, c(1,1:3))
  test_that("get_data.t-test", {
    expect_equal(get_data(x)$x, c(1, 2, 3, NA))
  })

  d <- matrix(c(794, 86, 150, 570),
              nrow = 2,
              dimnames = list("1st Survey" = c("Approve", "Disapprove"),
                              "2nd Survey" = c("Approve", "Disapprove")))
  m <- mcnemar.test(d)
  test_that("get_data.mcnemar", {
    expect_equal(
      get_data(m),
      structure(c(794, 86, 150, 570), .Dim = c(2L, 2L),
                .Dimnames = list( `1st Survey` = c("Approve", "Disapprove"),
                                  `2nd Survey` = c("Approve", "Disapprove")), class = "table")
    )
  })
}
