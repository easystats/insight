# insight 0.5.1

## Bug fixes

  - Fixed issue in `find_parameters()` and `get_parameters()` for *gamlss* models.

# insight 0.5.0

## Breaking Changes

  - The previous `is_model()` function has been renamed to `is_model_supported()`
    since it was unclear if the function checked the entered object was a model
    or a supported model in *insight*. The new `is_model()` function checks if the
    entered object is a model object, while `is_model_supported()` checks if a
    supported model object.

## New functions

  - `find_statistic()` to return the test statistic of a regression model.
  - `format_value()` and `format_table()` as utility-functions to format (model) 
    output, especially for tabular output.
  - `color_if()` as utility-function to add color formatting to values, depending
    on certain conditions.

## General

  - Make extraction of model family information more stable for gam-objects.

## Changes to functions

  - `find_parameters()` and `get_parameters()` now also support objects of class `sim` and `sim.merMod` (from `arm::sim()`).
  - `get_variance()` now also supports models of class *clmm*.
  - `find_predictors()` and `find_variables()` now include the Euclidean distance matrix for spatial models from *glmmTMB* (returned as random effects element, or more precise, as random slope).

## Bug fixes

  - `find_formula()` now extracts group factors of random effects for *gamlss*
    models.
  - `find_parameters()` and `get_parameters()` no longer show `NA` coefficients
    from group factors of random effects for *gamlss* models.
  - `find_parameters()` and `get_parameters()` did not work for multivariate response models of class *brmsfit* when argument `parameters` was specified.
  - `get_data()` dropped value and variable label attributes, when model frame
    contained matrix variables (like splines).
  - `get_priors()` swapped column names `location` and `scale` for *brmsfit* -objects.
  - `get_parameters()` did not work for *glmmTMB* models without zero-inflation component.
  - `find_predictors()` did not remove parentheses from terms in multiple nested random effects.
  - Better support for *gam* models (package *mgcv*) with `ziplss` or `mvn` families.

# insight 0.4.1

## Changes to functions

* `get_variance()` now supports models with Gamma-family.
* `get_weights()` and `find_weights()` now work for *brms*-models.

## Bug fixes

* Fix CRAN-check issues due to recent update from the *panelr*-package.

# insight 0.4.0

## General

* Updates `CITATION`, based on publication in [JOSS](https://doi.org/10.21105/joss.01412).
* Added `nobs()`-method for those model-objects supported by *insight* that did not yet provide such a method.

## New supported model classes

* `betabin` and `negbin` (*aod*), `BBreg` and `BBmm` (*HRQoL*), `wbm` (*panelr*), `survfit` (*survival*)

## New functions

* `clean_parameters()`, which returns a data frame with "decomposed" parameters, i.e. a data frame with information about the clean parameter name, whether it is a fixed or random effect, from conditional or zero-inflated component, and if it is a parameter related to specific grouping factors of random effects (#106).
* `print_parameters()`, which can be called on top of `clean_parameters()` to get a list of data frames that represent the different model components (fixed, random, zero-inflated, ...) and which is in shape for printing summary statistics of complex models.
* `find_interactions()` to return all low/high order interaction terms in a model.
* `find_weights()` and `get_weights()` to find / get model weights.

## Breaking changes

* To reduce interface complexity, `find_parameters()` and `get_parameters()` for objects of class `aovlist` now return the elements `$conditional` and `$random`, to be in line with other supported objects.
* The argument `resp` in `get_response()` was renamed to `select`, to have a more clear verb (#114).
* The functions `find_variables()` and `find_terms()` were flipped, because what previously was considered as "term" was actually a "variable", and vice versa.

## Changes to functions

* `find_parameters()` and `get_parameters()` now allow to return a `sigma`-element for multivariate-response models (*brmsfit*, *stanmvreg*).
* `find_parameters()` and `get_parameters()` now return the intercepts for **polr** models.
* `find_parameters()` gets an `effects` and a `component`-argument, similar to many other functions in _insight_.
* `get_priors()` now returns `distribution = "uniform"` when model was fitted with flat priors.
* `model_info()` now returns an element `$is_hurdle` for hurdle models.

## Bug fixes

* `find_parameters()` returned priors for `brmsfit`-objects as `$random`-element. Now, `find_parameters()` returns a `$priors`-element (#98).
* `find_parameters()` did not remove smooth-parameters that used `te()` or `ti()`.
* Fixed various issues with non-linear *brms*-models.
* `find_formula()` (and hence, `find_response()` or `get_data()`) did not work for multi-column responses in null-models (#100).
* Fixed bugs with models from package *plm* that occurred during the latest plm-update.
* `find_predictors()` did not split nested random effects when these were written as `g1:g2` instead `g2/g2` in the random part of the model formula.
* Fixed issues with `all_model_equal()`.
* `get_data()` did not return `(weights)` columns for some model objects.
* `get_priors()` for *stanreg*-models returned the priors in sorted order, so sometimes parameter names and associated prior values did not match (#111).
* `get_variance()` did not calculate random effect variances, when interaction terms in random slopes were also present in fixed effects, but the interaction was written in different order (e.g., `a*b` and `b*a`) (#112).
* Fixed issue with tibbles in `get_data()`.
* Fixed issue with `get_priors()` for *stanreg*-models, when `prior_summary()` returned `NULL` for a prior (#116).
* Fixed issue with the recent update of *GLMMadaptive*, which broke some of the functions.

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
