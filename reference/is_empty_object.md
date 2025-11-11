# Check if object is empty

Check if object is empty

## Usage

``` r
is_empty_object(x)
```

## Arguments

- x:

  A list, a vector, or a dataframe.

## Value

A logical indicating whether the entered object is empty.

## Examples

``` r
is_empty_object(c(1, 2, 3, NA))
#> [1] FALSE
is_empty_object(list(NULL, c(NA, NA)))
#> [1] FALSE
is_empty_object(list(NULL, NA))
#> [1] FALSE
```
