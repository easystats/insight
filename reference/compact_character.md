# Remove empty strings from character

Remove empty strings from character

## Usage

``` r
compact_character(x)
```

## Arguments

- x:

  A single character or a vector of characters.

## Value

A character or a character vector with empty strings removed.

## Examples

``` r
compact_character(c("x", "y", NA))
#> [1] "x" "y"
compact_character(c("x", "NULL", "", "y"))
#> [1] "x" "y"
```
