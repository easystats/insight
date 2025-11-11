# Get the data from model predictors

Returns the data from all predictor variables (fixed effects).

## Usage

``` r
get_predictors(x, verbose = TRUE)
```

## Arguments

- x:

  A fitted model.

- verbose:

  Toggle messages and warnings.

## Value

The data from all predictor variables, as data frame.

## Examples

``` r
m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
head(get_predictors(m))
#>                      wt cyl vs
#> Mazda RX4         2.620   6  0
#> Mazda RX4 Wag     2.875   6  0
#> Datsun 710        2.320   4  1
#> Hornet 4 Drive    3.215   6  1
#> Hornet Sportabout 3.440   8  0
#> Valiant           3.460   6  1
```
