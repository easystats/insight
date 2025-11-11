# Get model parameters from estimated marginal means objects

Returns the coefficients from a model.

## Usage

``` r
# S3 method for class 'emmGrid'
get_parameters(x, summary = FALSE, merge_parameters = FALSE, ...)
```

## Arguments

- x:

  A fitted model.

- summary:

  Logical, indicates whether the full posterior samples
  (`summary = FALSE`)) or the summarized centrality indices of the
  posterior samples (`summary = TRUE`)) should be returned as estimates.

- merge_parameters:

  Logical, if `TRUE` and `x` has multiple columns for parameter names
  (like `emmGrid` objects may have), these are merged into a single
  parameter column, with parameters names and values as values.

- ...:

  Currently not used.

## Value

A data frame with two columns: the parameter names and the related point
estimates.

## Note

Note that `emmGrid` or `emm_list` objects returned by functions from
**emmeans** have a different structure compared to usual regression
models. Hence, the `Parameter` column does not always contain names of
*variables*, but may rather contain *values*, e.g. for contrasts. See an
example for pairwise comparisons below.

## Examples

``` r
data(mtcars)
model <- lm(mpg ~ wt * factor(cyl), data = mtcars)

emm <- emmeans(model, "cyl")
#> NOTE: Results may be misleading due to involvement in interactions
get_parameters(emm)
#>   cyl Estimate
#> 1   4 21.40330
#> 2   6 19.46455
#> 3   8 16.81441

emm <- emmeans(model, pairwise ~ cyl)
#> NOTE: Results may be misleading due to involvement in interactions
get_parameters(emm)
#>     Parameter  Estimate Component
#> 1           4 21.403304   emmeans
#> 2           6 19.464549   emmeans
#> 3           8 16.814408   emmeans
#> 4 cyl4 - cyl6  1.938755 contrasts
#> 5 cyl4 - cyl8  4.588896 contrasts
#> 6 cyl6 - cyl8  2.650141 contrasts
```
