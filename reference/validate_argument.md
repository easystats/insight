# Validate arguments against a given set of options

This is a replacement for
[`match.arg()`](https://rdrr.io/r/base/match.arg.html), however, the
error string should be more informative for users. The name of the
affected argument is shown, and possible typos as well as remaining
valid options. Note that the argument `several.ok` is always `FALSE` in
`validate_argument()`, i.e. this function - unlike
[`match.arg()`](https://rdrr.io/r/base/match.arg.html) - does *not*
allow evaluating several valid options at once.

## Usage

``` r
validate_argument(argument, options)
```

## Arguments

- argument:

  The bare name of the argument to be validated.

- options:

  Valid options, usually a character vector.

## Value

`argument` if it is a valid option, else an error is thrown.

## Examples

``` r
foo <- function(test = "small") {
  validate_argument(test, c("small", "medium", "large"))
}
foo("small")
#> [1] "small"
# errors:
# foo("masll")
```
