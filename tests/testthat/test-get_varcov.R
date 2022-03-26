requiet("sandwich")
requiet("clubSandwich")

test_that("informative error in get_varcov.default", {
    requiet("lme4")
    mod <- lmer(mpg ~ hp + (1 | cyl), data = mtcars)
    # sandwich: not supported
    expect_error(get_varcov(mod, vcov = "HC2"), regexp = "not be supported")
    # clubSandwich: supported
    expect_error(get_varcov(mod, vcov = "CR0"), NA)
})

test_that("lm: sandwich", {
    mod <- lm(mpg ~ hp * wt, data = mtcars)
    expect_equal(get_varcov(mod, vcov = "HC1"),
                 vcovHC(mod, type = "HC1"))
    expect_equal(get_varcov(mod, vcov = "HC4"),
                 vcovHC(mod, type = "HC4"))
    expect_equal(get_varcov(mod, vcov = "HC", vcov_args = list(type = "HC4")),
                 vcovHC(mod, type = "HC4"))
    expect_equal(get_varcov(mod, vcov = vcovOPG),
                 vcovOPG(mod),
                 tolerance = 1e-5)
})

test_that("lm: clubSandwich", {
    mod <- lm(mpg ~ hp * wt, data = mtcars)
    expect_equal(get_varcov(mod,
                            vcov = "CR",
                            vcov_args = list(cluster = mtcars$cyl, type = "CR0")),
                 clubSandwich::vcovCR(mod, cluster = mtcars$cyl, type = "CR0"),
                 tolerance = 1e-5,
                 ignore_attr = TRUE)
})

