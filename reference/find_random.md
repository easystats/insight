# Find names of random effects

Return the name of the grouping factors from mixed effects models.

## Usage

``` r
find_random(x, split_nested = FALSE, flatten = FALSE)
```

## Arguments

- x:

  A fitted mixed model.

- split_nested:

  Logical, if `TRUE`, terms from nested random effects will be returned
  as separated elements, not as single string with colon. See
  'Examples'.

- flatten:

  Logical, if `TRUE`, the values are returned as character vector, not
  as list. Duplicated values are removed.

## Value

A list of character vectors that represent the name(s) of the random
effects (grouping factors). Depending on the model, the returned list
has following elements:

- `random`, the "random effects" terms from the conditional part of
  model

- `zero_inflated_random`, the "random effects" terms from the
  zero-inflation component of the model. For **brms**, this is named
  `zi_random`.

- `dispersion_random`, the "random effects" terms from the dispersion
  component of the model

Models of class `brmsfit` may also contain elements for auxiliary
parameters.

## Examples

``` r
data(sleepstudy, package = "lme4")
sleepstudy$mygrp <- sample(1:5, size = 180, replace = TRUE)
sleepstudy$mysubgrp <- NA
for (i in 1:5) {
  filter_group <- sleepstudy$mygrp == i
  sleepstudy$mysubgrp[filter_group] <-
    sample(1:30, size = sum(filter_group), replace = TRUE)
}

m <- lme4::lmer(
  Reaction ~ Days + (1 | mygrp / mysubgrp) + (1 | Subject),
  data = sleepstudy
)

find_random(m)
#> $random
#> [1] "mysubgrp:mygrp" "mygrp"          "Subject"       
#> 
find_random(m, split_nested = TRUE)
#> $random
#> [1] "mysubgrp" "mygrp"    "Subject" 
#> 
```
