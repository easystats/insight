# Find possible transformation of model variables

This functions checks whether any transformation, such as log- or
exp-transforming, was applied to the response variable (dependent
variable) in a regression formula. Optionally, all model terms can also
be checked for any such transformation. Currently, following patterns
are detected: `log`, `log1p`, `log2`, `log10`, `exp`, `expm1`, `sqrt`,
`log(y+<number>)`, `log-log`, `log(y,base=<number>)`, `power` (e.g. to
2nd power, like `I(y^2)`), `inverse` (like `1/y`), `scale` (e.g.,
`y/3`), and `box-cox` (e.g., `(y^lambda - 1) / lambda`).

## Usage

``` r
find_transformation(x, ...)

# Default S3 method
find_transformation(x, include_all = FALSE, ...)
```

## Arguments

- x:

  A regression model or a character string of the formulation of the
  (response) variable.

- ...:

  Currently not used.

- include_all:

  Logical, if `TRUE`, does not only check the response variable, but all
  model terms.

## Value

A string, with the name of the function of the applied transformation.
Returns `"identity"` for no transformation, and e.g. `"log(y+3)"` when a
specific values was added to the response variables before
log-transforming. For unknown transformations, returns `NULL`.

## Examples

``` r
# identity, no transformation
model <- lm(Sepal.Length ~ Species, data = iris)
find_transformation(model)
#> [1] "identity"

# log-transformation
model <- lm(log(Sepal.Length) ~ Species, data = iris)
find_transformation(model)
#> [1] "log"

# log+2
model <- lm(log(Sepal.Length + 2) ~ Species, data = iris)
find_transformation(model)
#> [1] "log(x+2)"

# find transformation for all model terms
model <- lm(mpg ~ log(wt) + I(gear^2) + exp(am), data = mtcars)
find_transformation(model, include_all = TRUE)
#> $response
#>        mpg 
#> "identity" 
#> 
#> $conditional
#>      wt    gear      am 
#>   "log" "power"   "exp" 
#> 

# inverse, response provided as character string
find_transformation("1 / y")
#> [1] "inverse"
```
