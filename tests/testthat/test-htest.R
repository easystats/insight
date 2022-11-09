if (requiet("testthat") && requiet("insight")) {
  # t test ---------------

  x <- t.test(1:3, c(1, 1:3))
  test_that("get_data.t-test", {
    expect_equal(get_data(x)$x, c(1, 2, 3, NA))
  })

  test_that("model_info.t-test", {
    expect_true(model_info(x)$is_ttest)
  })


  # mcnemar test ---------------

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

  test_that("model_info.mcnemar-test", {
    expect_true(model_info(m)$is_chi2test)
    expect_true(model_info(m)$is_xtab)
  })


  # fisher test ---------------

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

  test_that("model_info.fisher-test", {
    expect_true(model_info(m)$is_chi2test)
    expect_true(model_info(m)$is_xtab)
  })


  # friedmann test ---------------

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

  test_that("model_info.friedman-test", {
    expect_true(model_info(m)$is_ranktest)
  })


  # shapiro test ---------------

  set.seed(123)
  m <- shapiro.test(rnorm(10, mean = 5, sd = 3))
  test_that("get_data.freedman", {
    expect_equal(
      get_data(m),
      data.frame(
        x <- c(
          8.67224539231838, 6.07944148117209, 6.20231435178216,
          5.33204814783536, 3.33247659573778, 10.3607394104092, 6.49355143468772,
          -0.899851469888914, 7.10406770469106, 3.5816257768162
        )
      ),
      tolerance = 1e-3,
      ignore_attr = TRUE
    )
  })

  test_that("model_info.shapiro-test", {
    expect_true(model_info(m)$is_variancetest)
    expect_equal(model_info(m)$family, "shapiro")
  })
}
