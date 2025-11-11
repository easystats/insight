# Get the model's function call

Returns the model's function call when available.

## Usage

``` r
get_call(x)
```

## Arguments

- x:

  A fitted mixed model.

## Value

A function call.

## Examples

``` r
data(mtcars)
m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
get_call(m)
#> lm(formula = mpg ~ wt + cyl + vs, data = mtcars)

m <- lme4::lmer(Sepal.Length ~ Sepal.Width + (1 | Species), data = iris)
get_call(m)
#> lme4::lmer(formula = Sepal.Length ~ Sepal.Width + (1 | Species), 
#>     data = iris)
```
