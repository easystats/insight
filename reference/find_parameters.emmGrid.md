# Find model parameters from estimated marginal means objects

Returns the parameter names from a model.

## Usage

``` r
# S3 method for class 'emmGrid'
find_parameters(x, flatten = FALSE, merge_parameters = FALSE, ...)
```

## Arguments

- x:

  A fitted model.

- flatten:

  Logical, if `TRUE`, the values are returned as character vector, not
  as list. Duplicated values are removed.

- merge_parameters:

  Logical, if `TRUE` and `x` has multiple columns for parameter names
  (like `emmGrid` objects may have), these are merged into a single
  parameter column, with parameters names and values as values.

- ...:

  Currently not used.

## Value

A list of parameter names. For simple models, only one list-element,
`conditional`, is returned.

## Examples

``` r
data(mtcars)
model <- lm(mpg ~ wt * factor(cyl), data = mtcars)
emm <- emmeans(model, c("wt", "cyl"))
find_parameters(emm)
#> $emmeans
#> [1] 3.21725 3.21725 3.21725
#> 
```
