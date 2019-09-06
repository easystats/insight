result <- tools::check_packages_in_dir(
  dir = "C:/Users/mail/Documents/R/easystats",
  check_args = c("--as-cran", ""),
  reverse = list(repos = getOption("repos")["CRAN"])
)


result <- tools::check_packages_in_dir("C:/Users/mail/Documents/R/easystats", reverse = list())
