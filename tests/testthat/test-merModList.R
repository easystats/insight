skip_if_not_installed("merTools")
skip_if_not_installed("lme4")

test_that("get_data, merModList, verbose", {
  d <- split(lme4::sleepstudy, rep(1:2, 90))
  m <- merTools::lmerModList(Reaction ~ Days + (1 | Subject), data = d)
  expect_warning(
    {
      out <- get_data(m)
    },
    regex = "Can't access data"
  )
  expect_null(out)
  expect_silent({
    out <- get_data(m, verbose = FALSE)
  })
  expect_null(out)
})
