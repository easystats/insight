# Get link-function from model object

Returns the link-function from a model object.

## Usage

``` r
link_function(x, ...)

# S3 method for class 'betamfx'
link_function(x, what = c("mean", "precision"), ...)

# S3 method for class 'gamlss'
link_function(x, what = c("mu", "sigma", "nu", "tau"), ...)

# S3 method for class 'betareg'
link_function(x, what = c("mean", "precision"), ...)

# S3 method for class 'DirichletRegModel'
link_function(x, what = c("mean", "precision"), ...)
```

## Arguments

- x:

  A fitted model.

- ...:

  Currently not used.

- what:

  For `gamlss` models, indicates for which distribution parameter the
  link (inverse) function should be returned; for `betareg` or
  `DirichletRegModel`, can be `"mean"` or `"precision"`.

## Value

A function, describing the link-function from a model-object. For
multivariate-response models, a list of functions is returned.

## Examples

``` r
# example from ?stats::glm
counts <- c(18, 17, 15, 20, 10, 20, 25, 13, 12)
outcome <- gl(3, 1, 9)
treatment <- gl(3, 3)
m <- glm(counts ~ outcome + treatment, family = poisson())

link_function(m)(0.3)
#> [1] -1.203973
# same as
log(0.3)
#> [1] -1.203973
```
