# insight 0.3.1

## New supported model classes

* `betabin` and `negbin` (*aod*), `BBreg` and `BBmm` (*HRQoL*), `wbm` (*panelr*), `survfit` (*survival*)

## New functions

* `clean_parameters()`, which returns a data frame with "decomposed" parameters, i.e. a data frame with information about the clean paramter name, whether it is a fixed or random effect, from conditional or zero-inflated component, and if it is a parameter related to specific grouping factors of random effects (#106).
* `find_interactions()` to return all low/high order interaction terms in a model.
* `find_weights()` and `get_weights()` to find / get model weights.

## Breaking changes

* To reduce interface complexity, `find_parameters()` and `get_parameters()` for objects of class `aovlist` now return the elements `$conditional` and `$random`, to be in line with other supported objects.

## Changes to functions

* `find_parameters()` and `get_parameters()` now allow to return a `sigma`-element for multivariate-response models (*brmsfit*, *stanmvreg*).
* `find_parameters()` gets an `effects` and a `component`-argument, similar to many other functions in _insight_.
* `get_priors()` now returns `distribuion = "uniform"` when model was fitted with flat priors.
* `model_info()` now returns an element `$is_hurdle` for hurdle models.

## Bug fixes

* `find_parameters()` returned priors for `brmsfit`-objects as `$random`-element. Now, `find_parameters()` returns a `$priors`-element (#98).
* `find_parameters()` did not remove smooth-parameters that used `te()` or `ti()`.
* Fixed various issues with non-linear *brms*-models.
* `find_formula()` (and hence, `find_response()` or `get_data()`) did not work for multi-column responses in null-models (#100).
* Fixed bugs with models from package *plm* that occured during the latest plm-update.
* `find_predictors()` did not split nested random effects when these were written as `g1:g2` instead `g2/g2` in the random part of the model formula.
* Fixed issues with `all_model_equal()`.
* `get_data()` did not return `(weights)` columns for some model objects.
* `get_priors()` for *stanreg*-models returned the priors in sorted order, so sometimes parameter names and associated prior values did not match (#111).
* `get_variance()` did not calculate random effect variances, when interaction terms in random slopes were also present in fixed effects, but the interaction was written in different order (e.g., `a*b` and `b*a`) (#112).
* Fixed issue with tibbles in `get_data()`.

# insight 0.3.0

## New supported model classes

* `biglm` and `bigglm` (*biglm*), `feis` (*feisr*), `gbm` (*gbm*), `BFBayesFactor` (*BayesFactor*), `psm` (*rms*), `LORgee` (*multgee*), `censReg` (*censReg*), `ols` (*rms*), `speedlm` and `speedglm` (*speedglm*), `svyolr` (*survey*)

## New functions

* `is_nullmodel()` to check if model is a null-model (intercept-only), i.e. if the conditional part of the model has no predictors.
* `has_intercept()` to check if model has an intercept.

## Breaking Changes

* Functions like `find_predictors()` or `find_terms()` return `NULL` for null-models (intercept-only models). Use `is_nullmodel()` to check if your model only has an intercept-parameter (but no predictors).
* `get_variance()` no longer stops if random effects variance cannot be calculated. Rather, the return-value for `$var.random` will be `NULL`.

## Changes to functions

* `get_variance()` now computes the full variance for mixed models with zero-inflation component.
* `get_priors()` now returns the default-prior that was defined for all parameters of a class, if certain parameters have no specific prior.
* `find_parameters()` gets a `flatten`-argument, to either return results as list or as simple vector.
* `find_variables()` gets a `flatten`-argument, to either return results as list or as simple vector.

## Bug fixes

* `get_data()` did not work when model formula contained a function with namespace-prefix (like `lm(Sepal.Length ~ splines::bs(Petal.Width, df=4)`) (#93).
* `get_priors()` failed for *stanreg*-models, when one or more priors had no adjusted scales (#74).
* `find_random()` failed for mixed models with multiple responses.
* `get_random()` failed for *brmsfit* and *stanreg* models.
* `get_parameters()` and `find_parameters()` did not work for `MixMod`-objects _without_ zero-inflation component, when `component = "all"` (the default).
* `find_formula()` did not work for `plm`-models without instrumental variables.
* `find_formula()` returned random effects as conditional part of the formula for null-models (only intercept in fixed parts) (#87).
* Fixed issue with invalid notation of instrumental-variables formula in `felm`-models for R-devel on Linux.
* Fixed issue with `get_data()` for *gee* models, where incomplete cases were not removed from the data.
* Fixed potential issue with `get_data()` for null-models (only intercept in fixed parts) from models of class `glmmTMB`, `brmsfit`, `MixMod` and `rstanarm` (#91).
* `find_variables()` no longer returns (multiple) `"1"` for random effects.

# insight 0.2.0

## General

* Better handling of `AsIs`-variables with division-operation as dependent variables, e.g. if outcome was defined as `I(income/frequency)`, especially for `find_response()` and `get_data()`.
* Revised package-functions related to `felm`-models due to breaking changes in the *lfe*-package.

## New supported model classes

* `iv_robust` (*estimatr*), `crch` (*crch*), `gamlss` (*gamlss*), `lmrob` and `glmrob` (*robustbase*, #64), `rq`, `rqss` and `crq` (*quantreg*), `rlmer` (*robustlmm*), `mixed` (*afex*), `tobit` (*AER*) and `survreg` (*survival*).

## New functions

* `get_variance()`, to calculate the variance components from mixed models of class `merMod`, `glmmTMB`, `MixMod`, `rlmer`, `mixed`, `lme` and `stanreg` (#52). Furthermore, convenient shortcuts to return the related components directly, like `get_variance_random()` or `get_variance_residual()`.
* `find_algorithm()`, to get information about sampling algorithms and optimizers, and for Bayesian models also about chains and iterations (#38).
* `find_random_slopes()`, which returns the names of the random slopes of mixed models.
* `get_priors()`, to get a summary of priors used for a model (#39).
* `is_model()` to check whether an object is a (supported) regression model (#69).
* `all_models_equal()` to check whether objects are all (supported) regression models and of same class.
* `print_color()` (resp. `print_colour()`) to print coloured output to the console. Mainly implemented to reduce package dependencies.

## Changes to functions

* `find_parameters()` and `get_parameters()` get a `parameters`-argument for `brmsfit` and `stanreg` models, to allow selection of specific parameters that should be returned (#55).
* `find_parameters()` and `get_parameters()` now also return simplex parameters of monotic effects (**brms** only) and smooth terms (e.g. for gam-models).
* `find_terms()` and `find_predictors()` no longer return constants, in particular `pi` (#26).
* For `gls` and `lme` objects, functions like `find_formula()` etc. also return the correlation component (#19).
* `model_info` now returns `$is_tweedie` for models from tweedie-families.

## Bug fixes

* `find_parameters()` and `get_parameters()` did not preserve coefficients of monotonic category-specific effects from **brmsfit**-objects.
* Fixed bug that sometimes returned more elements for `find_predictors()` or `get_parameters()` than requested.
* Fixed bug in `get_data()` for **MixMod**-objects when response variable was defined via `cbind()`.
* Fixed bug in `get_response()` for models that used `cbind()` with a substraction (e.g. `cbind(success, total - success)`). In such cases, values for second column (in this example: `total`) were the substracted values `total - success`, not the original values from `total`.

# insight 0.1.2

## General

* Initial release.
