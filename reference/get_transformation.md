# Return function of transformed response variables

This functions checks whether any transformation, such as log- or
exp-transforming, was applied to the response variable (dependent
variable) in a regression formula, and returns the related function that
was used for transformation. See
[`find_transformation()`](https://easystats.github.io/insight/reference/find_transformation.md)
for an overview of supported transformations that are detected.

## Usage

``` r
get_transformation(x, include_all = FALSE, verbose = TRUE)
```

## Arguments

- x:

  A regression model or a character string of the formulation of the
  (response) variable.

- include_all:

  Logical, if `TRUE`, does not only check the response variable, but all
  model terms.

- verbose:

  Logical, if `TRUE`, prints a warning if the transformation could not
  be determined.

## Value

A list of two functions: `$transformation`, the function that was used
to transform the response variable; `$inverse`, the inverse-function of
`$transformation` (can be used for "back-transformation"). If no
transformation was applied, both list-elements `$transformation` and
`$inverse` just return `function(x) x`. If transformation is unknown,
`NULL` is returned.

## Examples

``` r
# identity, no transformation
model <- lm(Sepal.Length ~ Species, data = iris)
get_transformation(model)
#> $transformation
#> function (x) 
#> x
#> <bytecode: 0x55880a2bfa38>
#> <environment: 0x55880a2c1d48>
#> 
#> $inverse
#> function (x) 
#> x
#> <bytecode: 0x55880a2bfa38>
#> <environment: 0x55880a2c1d48>
#> 

# log-transformation
model <- lm(log(Sepal.Length) ~ Species, data = iris)
get_transformation(model)
#> $transformation
#> function (x, base = exp(1))  .Primitive("log")
#> 
#> $inverse
#> function (x)  .Primitive("exp")
#> 

# log-function
get_transformation(model)$transformation(0.3)
#> [1] -1.203973
log(0.3)
#> [1] -1.203973

# inverse function is exp()
get_transformation(model)$inverse(0.3)
#> [1] 1.349859
exp(0.3)
#> [1] 1.349859

# get transformations for all model terms
model <- lm(mpg ~ log(wt) + I(gear^2) + exp(am), data = mtcars)
get_transformation(model, include_all = TRUE)
#> $response
#> $response$mpg
#> $response$mpg$transformation
#> function (x) 
#> x
#> <bytecode: 0x55880a2bfa38>
#> <environment: 0x55880ab5c938>
#> 
#> $response$mpg$inverse
#> function (x) 
#> x
#> <bytecode: 0x55880a2bfa38>
#> <environment: 0x55880ab5c938>
#> 
#> 
#> 
#> $conditional
#> $conditional$wt
#> $conditional$wt$transformation
#> function (x, base = exp(1))  .Primitive("log")
#> 
#> $conditional$wt$inverse
#> function (x)  .Primitive("exp")
#> 
#> 
#> $conditional$gear
#> $conditional$gear$transformation
#> function (x) 
#> x^2
#> <environment: 0x55880ac268e0>
#> 
#> $conditional$gear$inverse
#> function (x) 
#> x^(2^-1)
#> <environment: 0x55880ac268e0>
#> 
#> 
#> $conditional$am
#> $conditional$am$transformation
#> function (x)  .Primitive("exp")
#> 
#> $conditional$am$inverse
#> function (x, base = exp(1))  .Primitive("log")
#> 
#> 
#> 
```
