# String Values Formatting

String Values Formatting

## Usage

``` r
format_string(x, ...)

# S3 method for class 'character'
format_string(x, length = NULL, abbreviate = "...", ...)
```

## Arguments

- x:

  String value.

- ...:

  Arguments passed to or from other methods.

- length:

  Numeric, maximum length of the returned string. If not `NULL`, will
  shorten the string to a maximum `length`, however, it will not
  truncate inside words. I.e. if the string length happens to be inside
  a word, this word is removed from the returned string, so the returned
  string has a *maximum* length of `length`, but might be shorter.

- abbreviate:

  String that will be used as suffix, if `x` was shortened.

## Value

A formatted string.

## Examples

``` r
s <- "This can be considered as very long string!"
# string is shorter than max.length, so returned as is
format_string(s, 60)
#> [1] "This can be considered as very long string!"

# string is shortened to as many words that result in
# a string of maximum 20 chars
format_string(s, 20)
#> [1] "This can be..."
```
