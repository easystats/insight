# insight 0.2.0

## General

* Better handling of `AsIs`-variables with division-operation as dependent variables, e.g. if outcome was defined as `I(income/frequency)`, especially for `find_response()` and `get_data()`.

## New suppored model classes

* `iv_robust` (*estimatr*), `gamlss` (*gamlss*), `lmrob` and `glmrob` (*robustbase*, #64), `rq`, `rqss` and `crq` (*quantreg*), `rlmer` (*robustlmm*) and `mixed` (*afex*).

## New functions

* `get_variance()`, to calculate the variance components from mixed models of class `merMod`, `glmmTMB`, `MixMod`, `rlmer`, `mixed`, `lme` and `stanreg` (#52). Furthermore, convenient shortcuts to return the related components directly, like `get_variance_random()` or `get_variance_residual()`.
* `find_algorithm()`, to get information about sampling algorithms and optimizers, and for Bayesian models also about chains and iterations (#38).
* `find_random_slopes()`, which returns the names of the random slopes of mixed models.
* `get_priors()`, to get a summary of priors used for a model (#39).
* `print_color()` (resp. `print_colour()`) to print coloured output to the console. Mainly implemented to reduce package dependencies.

## Changes to functions

* `find_parameters()` and `get_parameters()` get a `parameters`-argument for `brmsfit` and `stanreg` models, to allow selection of specific parameters that should be returned (#55).
* `find_parameters()` and `get_parameters()` now also return simplex parameters of monotic effects (**brms** only) and smooth terms (e.g. for gam-models).
* `find_terms()` and `find_predictors()` no longer return constants, in particular `pi` (#26).
* For `gls` and `lme` objects, functions like `find_formula()` etc. also return the correlation component (#19).

## Bug fixes

* `find_parameters()` and `get_parameters()` did not preserve coefficients of monotonic category-specific effects from **brmsfit**-objects.
* Fixed bug that sometimes returned more elements for `find_predictors()` or `get_parameters()` than requested.
* Fixed bug in `get_data()` for **MixMod**-objects when response variable was defined via `cbind()`.
* Fixed bug in `get_response()` for models that used `cbind()` with a substraction (e.g. `cbind(success, total - success)`). In such cases, values for second column (in this example: `total`) were the substracted values `total - success`, not the original values from `total`.

# insight 0.1.2

## General

* Initial release.