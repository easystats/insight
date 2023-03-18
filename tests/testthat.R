if (require("testthat")) {
  library(insight)

  is_dev_version <- length(strsplit(packageDescription("insight")$Version, "\\.")[[1]]) > 3

  if (is_dev_version) {
    Sys.setenv("RunAllinsightTests" = "yes")
  } else {
    Sys.setenv("RunAllinsightTests" = "no")
  }
  si <- Sys.info()

  osx <- tryCatch(
    {
      if (!is.null(si["sysname"])) {
        si["sysname"] == "Darwin" || grepl("^darwin", R.version$os)
      } else {
        FALSE
      }
    },
    error = function(e) {
      FALSE
    }
  )

  # disable / enable if needed
  if (.Platform$OS.type == "unix" && is_dev_version) {
    Sys.setenv("RunAllinsightStanTests" = "yes")
  } else {
    Sys.setenv("RunAllinsightStanTests" = "no")
  }

  test_check("insight")
}
