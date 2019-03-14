# insight 0.2.0

## New functions

* `get_variances()`, to calculate the variance components from mixed models of class `merMod`, `glmmTMB` and `stanreg` (#52).
* `find_algorithm()`, to get information about sampling algorithms and optimizers, and for Bayesian models also about chains and iterations (#38).
* `print_color()` (resp. `print_colour()`) to print coloured output to the console. Mainly implemented to reduce package dependencies.

## Changes to functions

* `find_parameters()` and `get_parameters()` get a `parameters`-argument for `brmsfit` and `stanreg` models, to allow selection of specific parameters that should be returned (#55).
* `find_parameters()` and `get_parameters()` now also return simplex parameters of monotic effects and smooth terms for splines (**brms** only).

## Bug fixes

* `find_parameters()` and `get_parameters()` did not preserve coefficients of monotonic category-specific effects from **brmsfit**-objects.
* Fixed bug that sometimes returned more elements for `find_predictors()` or `get_parameters()` than requested.

# insight 0.1.2

## General

* Initial release.