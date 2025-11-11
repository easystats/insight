# Find names of model weights

Returns the name of the variable that describes the weights of a model.

## Usage

``` r
find_weights(x, ...)
```

## Arguments

- x:

  A fitted model.

- ...:

  Used for objects from package **survey**, to pass the `source`
  argument to
  [`get_data()`](https://easystats.github.io/insight/reference/get_data.md).
  See related documentation of that argument for further details.

## Value

The name of the weighting variable as character vector, or `NULL` if no
weights were specified.

## Examples

``` r
data(mtcars)
mtcars$weight <- rnorm(nrow(mtcars), 1, .3)
m <- lm(mpg ~ wt + cyl + vs, data = mtcars, weights = weight)
find_weights(m)
#> [1] "weight"
```
