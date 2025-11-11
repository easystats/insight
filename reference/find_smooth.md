# Find smooth terms from a model object

Return the names of smooth terms from a model object.

## Usage

``` r
find_smooth(x, flatten = FALSE)
```

## Arguments

- x:

  A (gam) model.

- flatten:

  Logical, if `TRUE`, the values are returned as character vector, not
  as list. Duplicated values are removed.

## Value

A character vector with the name(s) of the smooth terms.

## Examples

``` r
data(iris)
model <- mgcv::gam(Petal.Length ~ Petal.Width + s(Sepal.Length), data = iris)
find_smooth(model)
#> $smooth_terms
#> [1] "s(Sepal.Length)"
#> 
```
