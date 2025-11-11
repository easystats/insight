# Find names of random slopes

Return the name of the random slopes from mixed effects models.

## Usage

``` r
find_random_slopes(x)
```

## Arguments

- x:

  A fitted mixed model.

## Value

A list of character vectors with the name(s) of the random slopes, or
`NULL` if model has no random slopes. Depending on the model, the
returned list has following elements:

- `random`, the random slopes from the conditional part of model

- `zero_inflated_random`, the random slopes from the zero-inflation
  component of the model. For **brms**, this is named `zi_random`.

- `dispersion_random`, the random slopes from the dispersion component
  of the model

Models of class `brmsfit` may also contain elements for auxiliary
parameters.

## Examples

``` r
data(sleepstudy, package = "lme4")
m <- lme4::lmer(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)
find_random_slopes(m)
#> $random
#> [1] "Days"
#> 
```
