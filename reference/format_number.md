# Convert number to words

Convert number to words

## Usage

``` r
format_number(x, textual = TRUE, ...)
```

## Arguments

- x:

  Number.

- textual:

  Return words. If `FALSE`, will run
  [`format_value()`](https://easystats.github.io/insight/reference/format_value.md).

- ...:

  Arguments to be passed to
  [`format_value()`](https://easystats.github.io/insight/reference/format_value.md)
  if `textual` is `FALSE`.

## Value

A formatted string.

## Note

The code has been adapted from here
https://github.com/ateucher/useful_code/blob/master/R/numbers2words.r

## Examples

``` r
format_number(2)
#> [1] "two"
format_number(45)
#> [1] "forty five"
format_number(324.68765)
#> [1] "three hundred and twenty five"
```
