# Get clean names of model parameters

This function "cleans" names of model parameters by removing patterns
like `"r_"` or `"b[]"` (mostly applicable to Stan models) and adding
columns with information to which group or component parameters belong
(i.e. fixed or random, count or zero-inflated...)

The main purpose of this function is to easily filter and select model
parameters, in particular of - but not limited to - posterior samples
from Stan models, depending on certain characteristics. This might be
useful when only selective results should be reported or results from
all parameters should be filtered to return only certain results (see
[`print_parameters()`](https://easystats.github.io/insight/reference/print_parameters.md)).

## Usage

``` r
clean_parameters(x, ...)
```

## Arguments

- x:

  A fitted model.

- ...:

  Currently not used.

## Value

A data frame with "cleaned" parameter names and information on effects,
component and group where parameters belong to. To be consistent across
different models, the returned data frame always has at least four
columns `Parameter`, `Effects`, `Component` and `Cleaned_Parameter`. See
'Details'.

## Details

The `Effects` column indicate if a parameter is a *fixed* or *random*
effect. The `Component` column refers to special model components like
*conditional*, *zero_inflated*, or *dispersion*. For models from package
**brms**, the various distributional parameters are also included in
this column. For models with random effects, the `Group` column
indicates the grouping factor of the random effects. For multivariate
response models from **brms** or **rstanarm**, an additional *Response*
column is included, to indicate which parameters belong to which
response formula. Furthermore, *Cleaned_Parameter* column is returned
that contains "human readable" parameter names (which are mostly
identical to `Parameter`, except for for models from **brms** or
**rstanarm**, or for specific terms like smooth- or spline-terms).

## Examples

``` r
# \donttest{
model <- download_model("brms_zi_2")
if (!is.null(model)) {
  clean_parameters(model)
}
#>                     Parameter Effects   Component              Group
#> 1                 b_Intercept   fixed conditional                   
#> 2                   b_persons   fixed conditional                   
#> 3                     b_child   fixed conditional                   
#> 4                    b_camper   fixed conditional                   
#> 5      r_persons[1,Intercept]  random conditional Intercept: persons
#> 6      r_persons[2,Intercept]  random conditional Intercept: persons
#> 7      r_persons[3,Intercept]  random conditional Intercept: persons
#> 8      r_persons[4,Intercept]  random conditional Intercept: persons
#> 9       sd_persons__Intercept  random conditional    SD/Cor: persons
#> 10             b_zi_Intercept   fixed          zi                   
#> 11                 b_zi_child   fixed          zi                   
#> 12                b_zi_camper   fixed          zi                   
#> 13 r_persons__zi[1,Intercept]  random          zi Intercept: persons
#> 14 r_persons__zi[2,Intercept]  random          zi Intercept: persons
#> 15 r_persons__zi[3,Intercept]  random          zi Intercept: persons
#> 16 r_persons__zi[4,Intercept]  random          zi Intercept: persons
#> 17   sd_persons__zi_Intercept  random          zi    SD/Cor: persons
#>    Cleaned_Parameter
#> 1        (Intercept)
#> 2            persons
#> 3              child
#> 4             camper
#> 5          persons.1
#> 6          persons.2
#> 7          persons.3
#> 8          persons.4
#> 9        (Intercept)
#> 10       (Intercept)
#> 11             child
#> 12            camper
#> 13         persons.1
#> 14         persons.2
#> 15         persons.3
#> 16         persons.4
#> 17       (Intercept)
# }
```
