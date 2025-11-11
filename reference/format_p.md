# p-values formatting

Format p-values.

## Usage

``` r
format_p(
  p,
  stars = FALSE,
  stars_only = FALSE,
  whitespace = TRUE,
  name = "p",
  missing = "",
  decimal_separator = NULL,
  digits = 3,
  ...
)
```

## Arguments

- p:

  value or vector of p-values.

- stars:

  Add significance stars (e.g., p \< .001\*\*\*). For Bayes factors, the
  thresholds for "significant" results are values larger than 3, 10, and
  30.

- stars_only:

  Return only significance stars.

- whitespace:

  Logical, if `TRUE` (default), preserves whitespaces. Else, all
  whitespace characters are removed from the returned string.

- name:

  Name prefixing the text. Can be `NULL`.

- missing:

  Value by which `NA` values are replaced. By default, an empty string
  (i.e. `""`) is returned for `NA`.

- decimal_separator:

  Character, if not `NULL`, will be used as decimal separator.

- digits:

  Number of significant digits. May also be `"scientific"` to return
  exact p-values in scientific notation, or `"apa"` to use an APA 7th
  edition-style for p-values (equivalent to `digits = 3`). If
  `"scientific"`, control the number of digits by adding the value as a
  suffix, e.g.m `digits = "scientific4"` to have scientific notation
  with 4 decimal places.

- ...:

  Arguments from other methods.

## Value

A formatted string.

## Examples

``` r
format_p(c(.02, .065, 0, .23))
#> [1] "p = 0.020" "p = 0.065" "p < .001"  "p = 0.230"
format_p(c(.02, .065, 0, .23), name = NULL)
#> [1] "0.020"  "0.065"  "< .001" "0.230" 
format_p(c(.02, .065, 0, .23), stars_only = TRUE)
#> [1] "*"   ""    "***" ""   

model <- lm(mpg ~ wt + cyl, data = mtcars)
p <- coef(summary(model))[, 4]
format_p(p, digits = "apa")
#> [1] "p < .001"  "p < .001"  "p = 0.001"
format_p(p, digits = "scientific")
#> [1] "p = 3.04318e-20" "p = 2.22020e-04" "p = 1.06428e-03"
format_p(p, digits = "scientific2")
#> [1] "p = 3.04e-20" "p = 2.22e-04" "p = 1.06e-03"
```
