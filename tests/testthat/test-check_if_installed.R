test_that("check_if_installed", {
  skip_if(interactive())
  skip_if_not_installed("datawizard")
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("marginaleffects", minimum_version = "0.13.0")

  # mimic package name if cat were to walk on a keyboard
  expect_error(check_if_installed("xklfueofi8eur3rnfalfb"))
  expect_error(check_if_installed("datawizard", minimum_version = "9.9.9"))
  expect_no_error(check_if_installed(c("datawizard", "rstanarm")))
  expect_no_error(check_if_installed(
    c("datawizard", "rstanarm"),
    minimum_version = c("0.8.0", "2.21.1")
  ))
  expect_no_error(check_if_installed(
    c(
      "datawizard",
      "rstanarm"
    ),
    minimum_version = c(NA, "2.21.1")
  ))

  expect_no_error(check_if_installed(
    c(
      "datawizard",
      "rstanarm"
    ),
    minimum_version = c("0.8.0", NA)
  ))

  expect_no_error(check_if_installed("marginaleffects", minimum_version = "0.28.0"))

  out <- check_if_installed(
    c("insight", "datawizard"),
    minimum_version = c("999.30.0", NA),
    prompt = FALSE,
    quiet = TRUE
  )
  expect_equal(out, c(FALSE, TRUE), ignore_attr = TRUE)

  out <- check_if_installed(
    c("insight", "datawizard"),
    prompt = FALSE,
    quiet = TRUE
  )
  expect_equal(out, c(TRUE, TRUE), ignore_attr = TRUE)

  out <- check_if_installed(
    c("insight", "datawizard"),
    minimum_version = c("0.1.0", "0.1.0"),
    prompt = FALSE,
    quiet = TRUE
  )
  expect_equal(out, c(TRUE, TRUE), ignore_attr = TRUE)
})

test_that("installed packages that fail to load report the load error", {
  skip_if_not_installed("withr")

  lib <- withr::local_tempdir()
  package <- "insighttestbroken"
  dir.create(file.path(lib, package))
  writeLines(
    c(paste("Package:", package), "Version: 1.0"),
    file.path(lib, package, "DESCRIPTION")
  )
  withr::local_libpaths(lib, action = "prefix")
  withr::local_options(list(width = 500L))

  cause <- tryCatch(loadNamespace(package), error = conditionMessage)
  result <- tryCatch(
    check_if_installed(package, prompt = FALSE),
    error = conditionMessage
  )
  expect_match(result, "Could not load package `insighttestbroken`", fixed = TRUE)
  expect_match(result, cause, fixed = TRUE)
  expect_false(grepl("install.packages", result, fixed = TRUE))

  expect_warning(
    check_if_installed(package, stop = FALSE, prompt = FALSE),
    "Could not load package"
  )
  out <- expect_silent(
    check_if_installed(c(working = "stats", broken = package), quietly = TRUE)
  )
  expect_identical(unclass(out), c(working = TRUE, broken = FALSE))
  expect_output(print(out), "Packages that failed the check:", fixed = TRUE)

  result <- tryCatch(
    check_if_installed(c(package, "insighttestmissing"), prompt = FALSE),
    error = conditionMessage
  )
  expect_match(result, "Could not load package", fixed = TRUE)
  expect_match(result, 'install.packages("insighttestmissing")', fixed = TRUE)
})
