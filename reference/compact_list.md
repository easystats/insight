# Remove empty elements from lists

Remove empty elements from lists

## Usage

``` r
compact_list(x, remove_na = FALSE)
```

## Arguments

- x:

  A list or vector.

- remove_na:

  Logical to decide if `NA`s should be removed.

## Examples

``` r
compact_list(list(NULL, 1, c(NA, NA)))
#> [[1]]
#> [1] 1
#> 
#> [[2]]
#> [1] NA NA
#> 
compact_list(c(1, NA, NA))
#> [1]  1 NA NA
compact_list(c(1, NA, NA), remove_na = TRUE)
#> [1] 1
```
