# Confidence/Credible Interval (CI) Formatting

Confidence/Credible Interval (CI) Formatting

## Usage

``` r
format_ci(CI_low, ...)

# S3 method for class 'numeric'
format_ci(
  CI_low,
  CI_high,
  ci = 0.95,
  digits = 2,
  brackets = TRUE,
  width = NULL,
  width_low = width,
  width_high = width,
  missing = "",
  zap_small = FALSE,
  ci_string = "CI",
  ...
)
```

## Arguments

- CI_low:

  Lower CI bound. Usually a numeric value, but can also be a CI output
  returned `bayestestR`, in which case the remaining arguments are
  unnecessary.

- ...:

  Arguments passed to or from other methods.

- CI_high:

  Upper CI bound.

- ci:

  CI level in percentage.

- digits:

  Number of digits for rounding or significant figures. May also be
  `"signif"` to return significant figures or `"scientific"` to return
  scientific notation. Control the number of digits by adding the value
  as suffix, e.g. `digits = "scientific4"` to have scientific notation
  with 4 decimal places, or `digits = "signif5"` for 5 significant
  figures (see also [`signif()`](https://rdrr.io/r/base/Round.html)).

- brackets:

  Either a logical, and if `TRUE` (default), values are encompassed in
  square brackets. If `FALSE` or `NULL`, no brackets are used. Else, a
  character vector of length two, indicating the opening and closing
  brackets.

- width:

  Minimum width of the returned string. If not `NULL` and `width` is
  larger than the string's length, leading whitespaces are added to the
  string. If `width="auto"`, width will be set to the length of the
  longest string.

- width_low, width_high:

  Like `width`, but only applies to the lower or higher confidence
  interval value. This can be used when the values for the lower and
  upper CI are of very different length.

- missing:

  Value by which `NA` values are replaced. By default, an empty string
  (i.e. `""`) is returned for `NA`.

- zap_small:

  Logical, if `TRUE`, small values are rounded after `digits` decimal
  places. If `FALSE`, values with more decimal places than `digits` are
  printed in scientific notation.

- ci_string:

  String to be used in the output to indicate the type of interval.
  Default is `"CI"`, but can be changed to `"HDI"` or anything else, if
  necessary.

## Value

A formatted string.

## Examples

``` r
format_ci(1.20, 3.57, ci = 0.90)
#> [1] "90% CI [1.20, 3.57]"
format_ci(1.20, 3.57, ci = NULL)
#> [1] "[1.20, 3.57]"
format_ci(1.20, 3.57, ci = NULL, brackets = FALSE)
#> [1] "1.20, 3.57"
format_ci(1.20, 3.57, ci = NULL, brackets = c("(", ")"))
#> [1] "(1.20, 3.57)"
format_ci(c(1.205645, 23.4), c(3.57, -1.35), ci = 0.90)
#> [1] "90% CI [1.21, 3.57]"   "90% CI [23.40, -1.35]"
format_ci(c(1.20, NA, NA), c(3.57, -1.35, NA), ci = 0.90)
#> [1] "90% CI [1.20, 3.57]" "90% CI [, -1.35]"    ""                   

# automatic alignment of width, useful for printing multiple CIs in columns
x <- format_ci(c(1.205, 23.4, 100.43), c(3.57, -13.35, 9.4))
cat(x, sep = "\n")
#> 95% CI [1.21, 3.57]
#> 95% CI [23.40, -13.35]
#> 95% CI [100.43, 9.40]

x <- format_ci(c(1.205, 23.4, 100.43), c(3.57, -13.35, 9.4), width = "auto")
cat(x, sep = "\n")
#> 95% CI [  1.21,   3.57]
#> 95% CI [ 23.40, -13.35]
#> 95% CI [100.43,   9.40]
```
