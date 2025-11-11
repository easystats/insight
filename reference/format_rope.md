# Percentage in ROPE formatting

Percentage in ROPE formatting

## Usage

``` r
format_rope(rope_percentage, name = "in ROPE", digits = 2)
```

## Arguments

- rope_percentage:

  Value or vector of percentages in ROPE.

- name:

  Name prefixing the text. Can be `NULL`.

- digits:

  Number of significant digits. May also be `"scientific"` to return
  exact p-values in scientific notation, or `"apa"` to use an APA 7th
  edition-style for p-values (equivalent to `digits = 3`). If
  `"scientific"`, control the number of digits by adding the value as a
  suffix, e.g.m `digits = "scientific4"` to have scientific notation
  with 4 decimal places.

## Value

A formatted string.

## Examples

``` r
format_rope(c(0.02, 0.12, 0.357, 0))
#> [1] "2.00% in ROPE"  "12.00% in ROPE" "35.70% in ROPE" "0% in ROPE"    
format_rope(c(0.02, 0.12, 0.357, 0), name = NULL)
#> [1] "2.00%"  "12.00%" "35.70%" "0%"    
```
