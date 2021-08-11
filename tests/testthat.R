if (require("testthat")) {
  library(insight)

  if (length(strsplit(packageDescription("insight")$Version, "\\.")[[1]]) > 3) {
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

  solaris <- tryCatch(
    {
      if (!is.null(si["sysname"])) {
        grepl("SunOS", si["sysname"], ignore.case = TRUE)
      } else {
        FALSE
      }
    },
    error = function(e) {
      FALSE
    }
  )

  # disable / enable if needed
  Sys.setenv("RunAllinsightStanTests" = "no")

  if (!osx && !solaris) {
    test_check("insight")
  }
}
