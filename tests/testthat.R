library(testthat)
library(insight)

if (length(strsplit(packageDescription("insight")$Version, "\\.")[[1]]) > 3) {
  Sys.setenv("RunAllinsightTests" = "yes")
} else {
  Sys.setenv("RunAllinsightTests" = "no")
}

test_check("insight")