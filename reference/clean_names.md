# Get clean names of model terms

This function "cleans" names of model terms (or a character vector with
such names) by removing patterns like
[`log()`](https://rdrr.io/r/base/Log.html) or
[`as.factor()`](https://rdrr.io/r/base/factor.html) etc.

## Usage

``` r
clean_names(x, ...)

# S3 method for class 'character'
clean_names(x, include_names = FALSE, ...)
```

## Arguments

- x:

  A fitted model, or a character vector.

- ...:

  Currently not used.

- include_names:

  Logical, if `TRUE`, returns a named vector where names are the
  original values of `x`.

## Value

The "cleaned" variable names as character vector, i.e. pattern like
`s()` for splines or [`log()`](https://rdrr.io/r/base/Log.html) are
removed from the model terms.

## Note

Typically, this method is intended to work on character vectors, in
order to remove patterns that obscure the variable names. For
convenience reasons it is also possible to call `clean_names()` also on
a model object. If `x` is a regression model, this function is (almost)
equal to calling
[`find_variables()`](https://easystats.github.io/insight/reference/find_variables.md).
The main difference is that `clean_names()` always returns a character
vector, while
[`find_variables()`](https://easystats.github.io/insight/reference/find_variables.md)
returns a list of character vectors, unless `flatten = TRUE`. See
'Examples'.

## Examples

``` r
# example from ?stats::glm
counts <- c(18, 17, 15, 20, 10, 20, 25, 13, 12)
outcome <- as.numeric(gl(3, 1, 9))
treatment <- gl(3, 3)
m <- glm(counts ~ log(outcome) + as.factor(treatment), family = poisson())
clean_names(m)
#> [1] "counts"    "outcome"   "treatment"

# difference "clean_names()" and "find_variables()"
data(cbpp, package = "lme4")
m <- lme4::glmer(
  cbind(incidence, size - incidence) ~ period + (1 | herd),
  data = cbpp,
  family = binomial
)

clean_names(m)
#> [1] "incidence" "size"      "period"    "herd"     
find_variables(m)
#> $response
#> [1] "incidence" "size"     
#> 
#> $conditional
#> [1] "period"
#> 
#> $random
#> [1] "herd"
#> 
find_variables(m, flatten = TRUE)
#> [1] "incidence" "size"      "period"    "herd"     
```
