# insight 0.2.0

## New functions

* `get_variances()`, to calculate the variance components from mixed models of class `merMod`, `glmmTMB` and `stanreg` (#52).
* `find_algorithm()`, to get information about sampling algorithms and optimizers, and for Bayesian models also about chains and iterations (#38).
* `print_color()` (resp. `print_colour()`) to print coloured output to the console. Mainly implemented to reduce package dependencies.

## Changes to functions

* `find_parameters()` and `get_parameters()` gets a `parameters`-argument for `brmsfit` and `stanreg` models, to allow selection of specific parameters that should be returned (#55).

## Bug fixes

* `find_parameters()` and `get_parameters()` did not preserve coefficients of monotonic category-specific effects from **brmsfit**-objects.

# insight 0.1.2

## General

* Initial release.