if (require("testthat") && require("insight")) {
  gfe <- insight:::.get_fixed_effects

  test_that(".get_fixed_effects", {

    f <- "am ~ disp:wt + (1|gear) + wt + (1+wt|carb)"
    expect_equal(
      gfe(stats::as.formula(f)),
      stats::as.formula("am ~ disp:wt + wt")
    )

    f <- "am ~ disp:wt + wt + (1|gear) + (1+wt|carb)"
    expect_equal(
      gfe(stats::as.formula(f)),
      stats::as.formula("am ~ disp:wt + wt")
    )

    f <- "am ~ (1|gear) + (1+wt|carb) + disp:wt + wt"
    expect_equal(
      gfe(stats::as.formula(f)),
      stats::as.formula("am ~ disp:wt + wt")
    )

    f <- "am ~ 1 + (1|gear)"
    expect_equal(
      gfe(stats::as.formula(f)),
      stats::as.formula("am ~ 1")
    )

    f <- "am ~ 1 + (1+wt|gear)"
    expect_equal(
      gfe(stats::as.formula(f)),
      stats::as.formula("am ~ 1")
    )

    f <- "am ~ disp:wt + (1|gear) + wt + (1+wt|carb)"
    expect_equal(
      gfe(stats::as.formula(f)),
      stats::as.formula("am ~ disp:wt + wt")
    )

    f <- "am ~ disp:wt + (1|gear) + wt + (1*wt|carb)"
    expect_equal(
      gfe(stats::as.formula(f)),
      stats::as.formula("am ~ disp:wt + wt")
    )

    f <- "am ~ (1|gear) + (1+wt|carb) + disp:wt * wt"
    expect_equal(
      gfe(stats::as.formula(f)),
      stats::as.formula("am ~ disp:wt * wt")
    )

    f <- "am ~ disp:wt + poly(gear, 2) + wt + (1+wt|carb)"
    expect_equal(
      gfe(stats::as.formula(f)),
      stats::as.formula("am ~ disp:wt + poly(gear, 2) + wt")
    )

    f <- "y ~ post + time1 + (1 | g2 / g1 / g0) + (post + time1 - 1 | g2)"
    expect_equal(
      gfe(stats::as.formula(f)),
      stats::as.formula("y ~ post + time1")
    )

    f <- "count ~ mined + (1 | site) + offset(Wtemp)"
    expect_equal(
      gfe(stats::as.formula(f)),
      stats::as.formula("count ~ mined + offset(Wtemp)")
    )

    f <- "count ~ mined + offset(Wtemp) + (1 | site)"
    expect_equal(
      gfe(stats::as.formula(f)),
      stats::as.formula("count ~ mined + offset(Wtemp)")
    )

    f <- "time | cens(censored) ~ age * sex + disease + (1|patient)"
    expect_equal(
      gfe(stats::as.formula(f)),
      stats::as.formula("time | cens(censored) ~ age * sex + disease")
    )

    f <- "success | trials(ntrials) ~ x + (1 | patient)"
    expect_equal(
      gfe(stats::as.formula(f)),
      stats::as.formula("success | trials(ntrials) ~ x")
    )
  })
}
