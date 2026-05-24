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

## Note

By default, `compact_list()` does not remove "empty" elements from
deeper list levels. Only if an element on the first level of a list is
"empty", it is removed.

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

# remove only NULL on top level, don't change deeper lists
compact_list(list(
  a = 1,
  NULL,
  b = list(NULL, list(1, 2, 3), list(list(x = 3, y = 4, NULL))),
  NULL
))
#> $a
#> [1] 1
#> 
#> $b
#> $b[[1]]
#> NULL
#> 
#> $b[[2]]
#> $b[[2]][[1]]
#> [1] 1
#> 
#> $b[[2]][[2]]
#> [1] 2
#> 
#> $b[[2]][[3]]
#> [1] 3
#> 
#> 
#> $b[[3]]
#> $b[[3]][[1]]
#> $b[[3]][[1]]$x
#> [1] 3
#> 
#> $b[[3]][[1]]$y
#> [1] 4
#> 
#> $b[[3]][[1]][[3]]
#> NULL
#> 
#> 
#> 
#> 
```
