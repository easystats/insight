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

head(get_random(m))
#>   mysubgrp mygrp Subject
#> 1       22     4     308
#> 2       22     4     308
#> 3        8     3     308
#> 4        6     4     308
#> 5       12     5     308
#> 6       26     1     308
```
