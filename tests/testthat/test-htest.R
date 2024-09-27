# t test ---------------

x <- t.test(1:3, c(1, 1:3))
test_that("get_data.t-test", {
  expect_identical(colnames(get_data(x)), c("x", "y"))
  expect_equal(
    get_data(x)$x,
    c(1, 2, 3, 1, 1, 2, 3),
    ignore_attr = TRUE
  )
  expect_equal(
    get_data(x)$y,
    c(1, 1, 1, 2, 2, 2, 2),
    ignore_attr = TRUE
  )
})

test_that("model_info.t-test", {
  expect_true(model_info(x)$is_ttest)
})

# One sample
test_that("get_data.t-test, one-sample", {
  tt1 <- t.test(mtcars$mpg)
  tt2 <- t.test(mtcars$mpg ~ 1)
  expect_equal(
    head(get_data(tt1)$mpg),
    c(21, 21, 22.8, 21.4, 18.7, 18.1)
  )
  expect_identical(nrow(get_data(tt1)), 32L)
  expect_equal(
    head(get_data(tt2)$mpg),
    c(21, 21, 22.8, 21.4, 18.7, 18.1)
  )
  expect_identical(nrow(get_data(tt2)), 32L)
  expect_true(model_info(tt1)$is_ttest)
  expect_true(model_info(tt2)$is_ttest)
})

# Two sample
test_that("get_data.t-test, two-sample", {
  tt3 <- t.test(mtcars$mpg ~ mtcars$am)
  tt4 <- t.test(mtcars$mpg[mtcars$am == 0], mtcars$mpg[mtcars$am == 1])
  expect_identical(colnames(get_data(tt3)), c("x", "y"))
  expect_identical(nrow(get_data(tt3)), 32L)
  expect_equal(
    head(get_data(tt3)$x),
    c(21, 21, 22.8, 21.4, 18.7, 18.1)
  )
  expect_equal(
    head(get_data(tt3)$y),
    c(2, 2, 2, 1, 1, 1),
    ignore_attr = TRUE
  )

  expect_identical(colnames(get_data(tt4)), c("x", "y"))
  expect_identical(nrow(get_data(tt4)), 32L)
  expect_equal(
    head(get_data(tt3)$x),
    c(21, 21, 22.8, 21.4, 18.7, 18.1)
  )
  expect_equal(
    head(get_data(tt3)$y),
    c(2, 2, 2, 1, 1, 1),
    ignore_attr = TRUE
  )

  expect_true(model_info(tt3)$is_ttest)
  expect_true(model_info(tt4)$is_ttest)
})

# # Paired
# test_that("get_data.t-test, two-sample", {
#   data(sleep)
#   sleep <<- sleep
#   tt5 <- t.test(sleep$extra ~ sleep$group, paired = TRUE)
#   tt6 <- t.test(sleep$extra[sleep$group == "1"], sleep$extra[sleep$group == "2"], paired = TRUE)
#   tt7 <- t.test(Pair(sleep$extra[sleep$group == "1"], sleep$extra[sleep$group == "2"]) ~ 1)

#   expect_identical(colnames(get_data(tt5)), c("x", "y"))
#   expect_equal(
#     head(get_data(tt5))$x,
#     c(0.7, -1.6, -0.2, -1.2, -0.1, 3.4),
#     ignore_attr = TRUE
#   )
#   expect_equal(
#     head(get_data(tt5))$y,
#     structure(c(1L, 1L, 1L, 1L, 1L, 1L), levels = c("1", "2"), class = "factor"),
#     ignore_attr = TRUE
#   )

#   expect_identical(colnames(get_data(tt6)), c("x", "y"))
#   expect_equal(
#     head(get_data(tt6))$x,
#     c(0.7, -1.6, -0.2, -1.2, -0.1, 3.4),
#     ignore_attr = TRUE
#   )
#   expect_equal(
#     head(get_data(tt6))$y,
#     structure(c(1L, 1L, 1L, 1L, 1L, 1L), levels = c("1", "2"), class = "factor"),
#     ignore_attr = TRUE
#   )

#   expect_true(model_info(tt5)$is_ttest)
#   expect_true(model_info(tt6)$is_ttest)
#   expect_true(model_info(tt7)$is_ttest)
# })


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
    ),
    ignore_attr = TRUE
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
    ),
    ignore_attr = TRUE
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
      x = c(
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
  expect_identical(model_info(m)$family, "shapiro")
})


# kruskal test ---------------

set.seed(123)
d <<- data.frame(
  x = sample(1:8, 50, TRUE),
  y = sample(1:3, 50, TRUE)
)

test_that("model_info.shapiro-test", {
  k1 <- kruskal.test(x ~ y, data = d)
  expect_null(get_data(k1))
  k2 <- kruskal.test(list(d$x, d$y))
  out <- get_data(k2)
  expect_identical(
    out,
    list(x1 = c(
      7L, 7L, 3L, 6L, 3L, 2L, 2L, 6L, 3L, 5L, 4L, 6L, 6L,
      1L, 2L, 3L, 8L, 5L, 3L, 3L, 1L, 4L, 1L, 1L, 5L, 3L, 8L, 2L, 7L,
      2L, 1L, 6L, 3L, 4L, 6L, 1L, 3L, 7L, 5L, 4L, 7L, 8L, 2L, 5L, 7L,
      1L, 1L, 2L, 7L, 3L
    ), x2 = c(
      1L, 3L, 1L, 3L, 2L, 1L, 2L, 1L, 1L,
      3L, 1L, 2L, 1L, 1L, 3L, 1L, 2L, 1L, 3L, 1L, 3L, 2L, 3L, 2L, 2L,
      3L, 2L, 2L, 3L, 3L, 1L, 2L, 2L, 1L, 2L, 1L, 1L, 2L, 3L, 3L, 1L,
      2L, 1L, 2L, 1L, 3L, 3L, 2L, 3L, 1L
    ))
  )
})
