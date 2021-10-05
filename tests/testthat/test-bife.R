skip_if_not_installed("bife")
requiet("bife")

test_that("get_predicted", {
    dataset <- bife::psid
    mod <- bife(LFP ~ AGE + I(INCH / 1000) + KID1 + KID2 + KID3 | ID, data = dataset)
    # link
    x <- get_predicted(mod, predict = "link", data = dataset)
    y <- get_predicted(mod, predict = NULL, type = "link", data = dataset)
    z <- predict(mod, type = "link", X_new = dataset)
    expect_equal(x, y)
    expect_equal(as.vector(x), z)
    # resopnse
    x = get_predicted(mod, predict = "expectation", data = dataset)
    y = get_predicted(mod, predict = NULL, type = "response", data = dataset)
    z <- predict(mod, type = "response", X_new = dataset)
    expect_equal(x, y)
    expect_equal(as.vector(x), z)
})
