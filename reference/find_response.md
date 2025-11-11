# Find name of the response variable

Returns the name(s) of the response variable(s) from a model object.

## Usage

``` r
find_response(x, combine = TRUE, ...)

# S3 method for class 'joint'
find_response(x, combine = TRUE, component = "conditional", ...)
```

## Arguments

- x:

  A fitted model.

- combine:

  Logical, if `TRUE` and the response is a matrix-column, the name of
  the response matches the notation in formula, and would for instance
  also contain patterns like `"cbind(...)"`. Else, the original variable
  names from the matrix-column are returned. See 'Examples'.

- ...:

  Currently not used.

- component:

  Character, if `x` is a joint model, this argument can be used to
  specify which component to return. Possible values are
  `"conditional"`, `"survival"` or `"all"`.

## Value

The name(s) of the response variable(s) from `x` as character vector, or
`NULL` if response variable could not be found.

## Examples

``` r
data(cbpp, package = "lme4")
cbpp$trials <- cbpp$size - cbpp$incidence
m <- glm(cbind(incidence, trials) ~ period, data = cbpp, family = binomial)

find_response(m, combine = TRUE)
#> [1] "cbind(incidence, trials)"
find_response(m, combine = FALSE)
#> [1] "incidence" "trials"   
```
