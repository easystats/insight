# Get a model objects that is saved as attribute

This functions tries to get a model object from the object `x`, where
the model object is saved as an (arbitrarily named) attribute. This is
useful for example, when a model is fitted and saved as an attribute of
a data frame.

## Usage

``` r
get_model(x, name = "model", element = NULL, ...)
```

## Arguments

- x:

  An object that contains a model object as an attribute. This could be
  a data frame or any other object that has an attribute containing the
  model.

- name:

  The name of the attribute that contains the model object. Defaults to
  `"model"`.

- element:

  String or character vector. If provided, this argument allows you to
  specify which element(s) of the model object to return. This can be
  useful if the model object is a list or has multiple components, and
  you only want to extract a specific part.

- ...:

  Not used.

## Value

The object that is stored as an attribute of `x` with the name `name`,
or the specific element of that object if `element` is provided. If the
attribute or element does not exist, an error is raised.

## Examples

``` r
# Example of using get_model
d <- data.frame(x = rnorm(100), y = rnorm(100))
# fit a model and save it as an attribute
model <- lm(y ~ x, data = d)
attr(d, "model") <- model
# get the model back
get_model(d)
#> 
#> Call:
#> lm(formula = y ~ x, data = d)
#> 
#> Coefficients:
#> (Intercept)            x  
#>    -0.11556      0.09456  
#> 
# get the coefficients of the model
get_model(d, element = "coefficients")
#> (Intercept)           x 
#> -0.11555541  0.09455584 
```
