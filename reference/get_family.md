# A robust alternative to stats::family

A robust and resilient alternative to
[`stats::family`](https://rdrr.io/r/stats/family.html). To avoid issues
with models like `gamm4`.

## Usage

``` r
get_family(x, ...)
```

## Arguments

- x:

  A statistical model.

- ...:

  Further arguments passed to methods.

## Examples

``` r
data(mtcars)
x <- glm(vs ~ wt, data = mtcars, family = "binomial")
get_family(x)
#> 
#> Family: binomial 
#> Link function: logit 
#> 

x <- mgcv::gamm(
  vs ~ am + s(wt),
  random = list(cyl = ~1),
  data = mtcars,
  family = "binomial"
)
#> 
#>  Maximum number of PQL iterations:  20 
#> iteration 1
#> iteration 2
#> iteration 3
#> iteration 4
#> iteration 5
#> iteration 6
#> iteration 7
#> iteration 8
get_family(x)
#> 
#> Family: binomial 
#> Link function: logit 
#> 
```
