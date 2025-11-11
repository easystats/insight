# Count number of random effect levels in a mixed model

Returns the number of group levels of random effects from mixed models.

## Usage

``` r
n_grouplevels(x, ...)
```

## Arguments

- x:

  A mixed model.

- ...:

  Additional arguments that can be passed to the function. Currently,
  you can use `data` to provide the model data, if available, to avoid
  retrieving model data multiple times.

## Value

The number of group levels in the model.

## Examples

``` r
data(sleepstudy, package = "lme4")
set.seed(12345)
sleepstudy$grp <- sample(1:5, size = 180, replace = TRUE)
sleepstudy$subgrp <- NA
for (i in 1:5) {
  filter_group <- sleepstudy$grp == i
  sleepstudy$subgrp[filter_group] <-
    sample(1:30, size = sum(filter_group), replace = TRUE)
}
model <- lme4::lmer(
  Reaction ~ Days + (1 | grp / subgrp) + (1 | Subject),
  data = sleepstudy
)
n_grouplevels(model)
#> Group      | N_levels
#> ---------------------
#> subgrp     |       30
#> grp        |        5
#> Subject    |       18
#> subgrp:grp |      108
```
