# Get the data from random effects

Returns the data from all random effects terms.

## Usage

``` r
get_random(x)
```

## Arguments

- x:

  A fitted mixed model.

## Value

The data from all random effects terms, as data frame. Or `NULL` if
model has no random effects.

## Examples

``` r
data(sleepstudy)
# prepare some data...
sleepstudy$mygrp <- sample(1:5, size = 180, replace = TRUE)
sleepstudy$mysubgrp <- NA
for (i in 1:5) {
  filter_group <- sleepstudy$mygrp == i
  sleepstudy$mysubgrp[filter_group] <-
    sample(1:30, size = sum(filter_group), replace = TRUE)
}

m <- lmer(
  Reaction ~ Days + (1 | mygrp / mysubgrp) + (1 | Subject),
  data = sleepstudy
)
#> boundary (singular) fit: see help('isSingular')

head(get_random(m))
#>   mysubgrp mygrp Subject
#> 1        2     4     308
#> 2       20     3     308
#> 3       11     1     308
#> 4       27     4     308
#> 5       17     1     308
#> 6        3     3     308
```
