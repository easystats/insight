# insight 0.1.2

## New functions

* `get_variances()`, to calculate the variance components from mixed models of class `merMod`, `glmmTMB` and `stanreg` (#52).
* `find_algorithm()`, to get information about sampling algorithms and optimizers, and for Bayesian models also about chains and iterations (#38).

## Changes to functions

* `find_parameters()` and `get_parameters()` gets a `pars`-argument for `brmsfit` and `stanreg` models, to allow selection of specific parameters that should be returned (#55).

# insight 0.1.2

## General

* Initial release.