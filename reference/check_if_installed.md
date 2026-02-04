# Checking if needed package is installed

Checking if needed package is installed

## Usage

``` r
check_if_installed(
  package,
  reason = "for this function to work",
  stop = TRUE,
  minimum_version = NULL,
  quietly = FALSE,
  prompt = interactive(),
  ...
)
```

## Arguments

- package:

  A character vector naming the package(s), whose installation needs to
  be checked in any of the libraries.

- reason:

  A phrase describing why the package is needed. The default is a
  generic description.

- stop:

  Logical that decides whether the function should stop if the needed
  package is not installed. Ignored if `quietly = TRUE`.

- minimum_version:

  A character vector, representing the minimum package version that is
  required for each package. Should be of same length as `package`. If
  `NULL`, will automatically check the DESCRIPTION file for the correct
  minimum version. If using `minimum_version` with more than one
  package, `NA` should be used instead of `NULL` for packages where a
  specific version is not necessary.

- quietly:

  Logical, if `TRUE`, invisibly returns a vector of logicals (`TRUE` for
  each installed package, `FALSE` otherwise), and does not stop or throw
  a warning. If `quietly = TRUE`, arguments `stop` and `prompt` are
  ignored. Use this argument to internally check for package
  dependencies without stopping or warnings.

- prompt:

  If `TRUE`, will prompt the user to install needed package(s). Ignored
  if `quietly = TRUE`.

- ...:

  Currently ignored

## Value

If `stop = TRUE`, and `package` is not yet installed, the function stops
and throws an error. Else, a named logical vector is returned,
indicating which of the packages are installed, and which not.

## Examples

``` r
# \donttest{
check_if_installed("insight")
try(check_if_installed("datawizard", stop = FALSE))
try(check_if_installed("rstanarm", stop = FALSE))
try(check_if_installed("nonexistent_package", stop = FALSE))
#> Warning: Package `nonexistent_package` required for this function to work.
#>   Please install it by running `install.packages("nonexistent_package")`.
try(check_if_installed("insight", minimum_version = "99.8.7"))
#> Error : Package `insight` is installed, but package version `99.8.7` is
#>   required.
#>   Please update it by running `install.packages("insight")`.
try(check_if_installed(c("nonexistent", "also_not_here"), stop = FALSE))
#> Warning: Packages `nonexistent` and `also_not_here` required for this function to
#>   work.
#>   Please install them by running `install.packages("nonexistent",
#>   "also_not_here")`.
try(check_if_installed(c("datawizard", "rstanarm"), stop = FALSE))
try(check_if_installed(c("datawizard", "rstanarm"),
  minimum_version = c(NA, "2.21.1"), stop = FALSE
))
# }
```
