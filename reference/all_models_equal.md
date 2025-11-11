# Checks if all objects are models of same class

Small helper that checks if all objects are *supported* (regression)
model objects and of same class.

## Usage

``` r
all_models_equal(..., verbose = FALSE)

all_models_same_class(..., verbose = FALSE)
```

## Arguments

- ...:

  A list of objects.

- verbose:

  Toggle off warnings.

## Value

A logical, `TRUE` if `x` are all supported model objects of same class.

## Examples

``` r
data(mtcars)
data(sleepstudy, package = "lme4")

m1 <- lm(mpg ~ wt + cyl + vs, data = mtcars)
m2 <- lm(mpg ~ wt + cyl, data = mtcars)
m3 <- lme4::lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy)
m4 <- glm(formula = vs ~ wt, family = binomial(), data = mtcars)

all_models_same_class(m1, m2)
#> [1] TRUE
all_models_same_class(m1, m2, m3)
#> [1] FALSE
all_models_same_class(m1, m4, m2, m3, verbose = TRUE)
#> Following objects are not identical with m1 (of class "lm"): m4 ("glm"),
#>   m3 ("lmerMod")
#> [1] FALSE
all_models_same_class(m1, m4, mtcars, m2, m3, verbose = TRUE)
#> Following objects are no (supported) models: mtcars
#> Following objects are not identical with m1 (of class "lm"): m4 ("glm"),
#>   mtcars ("data.frame"), m3 ("lmerMod")
#> [1] FALSE
```
