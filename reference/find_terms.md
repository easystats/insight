# Find all model terms

Returns a list with the names of all terms, including response value and
random effects, "as is". This means, on-the-fly tranformations or
arithmetic expressions like [`log()`](https://rdrr.io/r/base/Log.html),
[`I()`](https://rdrr.io/r/base/AsIs.html),
[`as.factor()`](https://rdrr.io/r/base/factor.html) etc. are preserved.

## Usage

``` r
find_terms(x, ...)

# Default S3 method
find_terms(x, flatten = FALSE, as_term_labels = FALSE, verbose = TRUE, ...)
```

## Arguments

- x:

  A fitted model.

- ...:

  Currently not used.

- flatten:

  Logical, if `TRUE`, the values are returned as character vector, not
  as list. Duplicated values are removed.

- as_term_labels:

  Logical, if `TRUE`, extracts model formula and tries to access the
  `"term.labels"` attribute. This should better mimic the
  [`terms()`](https://rdrr.io/r/stats/terms.html) behaviour even for
  those models that do not have such a method, but may be insufficient,
  e.g. for mixed models.

- verbose:

  Toggle warnings.

## Value

A list with (depending on the model) following elements (character
vectors):

- `response`, the name of the response variable

- `conditional`, the names of the predictor variables from the
  *conditional* model (as opposed to the zero-inflated part of a model)

- `random`, the names of the random effects (grouping factors)

- `zero_inflated`, the names of the predictor variables from the
  *zero-inflated* part of the model

- `zero_inflated_random`, the names of the random effects (grouping
  factors)

- `dispersion`, the name of the dispersion terms

- `instruments`, the names of instrumental variables

Returns `NULL` if no terms could be found (for instance, due to problems
in accessing the formula).

## Note

The difference to
[`find_variables()`](https://easystats.github.io/insight/reference/find_variables.md)
is that `find_terms()` may return a variable multiple times in case of
multiple transformations (see examples below), while
[`find_variables()`](https://easystats.github.io/insight/reference/find_variables.md)
returns each variable name only once.

## Parameters, Variables, Predictors and Terms

There are four functions that return information about the variables in
a model:
[`find_predictors()`](https://easystats.github.io/insight/reference/find_predictors.md),
[`find_variables()`](https://easystats.github.io/insight/reference/find_variables.md),
`find_terms()` and
[`find_parameters()`](https://easystats.github.io/insight/reference/find_parameters.md).
There are some differences between those functions, which are explained
using following model. Note that some, but not all of those functions
return information about the *dependent* and *independent* variables. In
this example, we only show the differences for the independent
variables.

    model <- lm(mpg ~ factor(gear), data = mtcars)

- `find_terms(model)` returns the model terms, i.e. how the variables
  were used in the model, e.g. applying transformations like
  [`factor()`](https://rdrr.io/r/base/factor.html),
  [`poly()`](https://rdrr.io/r/stats/poly.html) etc. `find_terms()` may
  return a variable name multiple times in case of multiple
  transformations. The return value would be `"factor(gear)"`.

- `find_parameters(model)` returns the names of the model parameters
  (coefficients). The return value would be `"(Intercept)"`,
  `"factor(gear)4"` and `"factor(gear)5"`.

- [`find_variables()`](https://easystats.github.io/insight/reference/find_variables.md)
  returns the original variable names.
  [`find_variables()`](https://easystats.github.io/insight/reference/find_variables.md)
  returns each variable name only once. The return value would be
  `"gear"`.

- [`find_predictors()`](https://easystats.github.io/insight/reference/find_predictors.md)
  is comparable to
  [`find_variables()`](https://easystats.github.io/insight/reference/find_variables.md)
  and also returns the original variable names, but excluded the
  *dependent* (response) variables. The return value would be `"gear"`.

## Examples

``` r
data(sleepstudy, package = "lme4")
m <- suppressWarnings(lme4::lmer(
  log(Reaction) ~ Days + I(Days^2) + (1 + Days + exp(Days) | Subject),
  data = sleepstudy
))

find_terms(m)
#> $response
#> [1] "log(Reaction)"
#> 
#> $conditional
#> [1] "Days"      "I(Days^2)"
#> 
#> $random
#> [1] "Days"      "exp(Days)" "Subject"  
#> 

# sometimes, it is necessary to retrieve terms from "term.labels" attribute
m <- lm(mpg ~ hp * (am + cyl), data = mtcars)
find_terms(m, as_term_labels = TRUE)
#> $conditional
#> [1] "hp"     "am"     "cyl"    "hp:am"  "hp:cyl"
#> 
```
