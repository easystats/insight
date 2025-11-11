# Gather information about objects in ellipsis (dot dot dot)

Provides information regarding the models entered in an ellipsis. It
detects whether all are models, regressions, nested regressions etc.,
assigning different classes to the list of objects.

## Usage

``` r
ellipsis_info(objects, ...)

# Default S3 method
ellipsis_info(..., only_models = TRUE, verbose = TRUE)
```

## Arguments

- objects, ...:

  Arbitrary number of objects. May also be a list of model objects.

- only_models:

  Only keep supported models (default to `TRUE`).

- verbose:

  Toggle warnings.

## Value

The list with objects that were passed to the function, including
additional information as attributes (e.g. if models have same response
or are nested).

## Examples

``` r
m1 <- lm(Sepal.Length ~ Petal.Width + Species, data = iris)
m2 <- lm(Sepal.Length ~ Species, data = iris)
m3 <- lm(Sepal.Length ~ Petal.Width, data = iris)
m4 <- lm(Sepal.Length ~ 1, data = iris)
m5 <- lm(Petal.Width ~ 1, data = iris)

objects <- ellipsis_info(m1, m2, m3, m4)
class(objects)
#> [1] "ListNonNestedRegressions" "ListRegressions"         
#> [3] "ListModels"               "list"                    

objects <- ellipsis_info(m1, m2, m4)
attributes(objects)$is_nested
#> [1] TRUE

objects <- ellipsis_info(m1, m2, m5)
attributes(objects)$same_response
#> [1] FALSE
```
