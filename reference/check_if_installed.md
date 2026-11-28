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

  Logical controlling whether a failed check stops or warns when no
  installation or update prompt is shown. Ignored if `quietly = TRUE`.

- minimum_version:

  A character vector, representing the minimum package version that is
  required for each package. Should be of same length as `package`. If
  `NULL`, minimum version requirements are ignored. If using
  `minimum_version` with more than one package, `NA` should be used for
  packages where a specific version is not necessary.

- quietly:

  Logical, if `TRUE`, returns the result invisibly without reporting
  failed checks. Arguments `stop` and `prompt` are ignored.

- prompt:

  If `TRUE`, will prompt the user to install needed package(s). Ignored
  if `quietly = TRUE`. By default, `prompt` is set to
  [`interactive()`](https://rdrr.io/r/base/interactive.html).

- ...:

  Further arguments passed down to
  [`utils::install.packages()`](https://rdrr.io/r/utils/install.packages.html).

## Value

A named logical vector indicating which packages passed the check.

## Details

Installed packages that fail to load are reported with the original load
error. Installation prompts apply to missing packages and packages below
the requested minimum version.

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
