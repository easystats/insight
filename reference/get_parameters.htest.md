# Get model parameters from htest-objects

Returns the parameters from a hypothesis test.

## Usage

``` r
# S3 method for class 'htest'
get_parameters(x, ...)
```

## Arguments

- x:

  A fitted model.

- ...:

  Currently not used.

## Value

A data frame with two columns: the parameter names and the related point
estimates.

## Examples

``` r
get_parameters(t.test(1:10, y = c(7:20)))
#>          Parameter Estimate
#> 1 1:10 and c(7:20)       -8
```
