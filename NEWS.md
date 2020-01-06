# insight 0.8.0

## New supported model classes

* `brglm` (*brglm*), `cgam`, `cgamm` (*cgam*), `cpglm`, `cpglmm` (*cplm*), `feglm` (*apaca*), `glmmadmb` (*glmmADMB*), `glmx` (*glmx*), partial support for `mcmc` (*coda*), `mixor` (*mixor*), `MANOVA`, `RM` (*MANOVA.RM*).

## General

* Better handling of `clm2`, `clmm2` and `rqss` models.

## New functions

* `format_ci()` (re-implemented and slightly enhanced from _parameters_), to format confidence/credible intervals.

## Changes to functions

* `find_parameters()` now also works for `BFBayesFactor` objects.
* Suppress non-informative warning in `get_data()` for model data with weights.
* `format_value()` automatically uses scientific notation for *very* large numbers (> 1e+5). Furthermore, the check for integer values was made more robust, to avoid warnings when checking *very* large numbers for integer type.
* Improved `find_parameters()`, `get_parameters()` and `clean_parameters()` for `BFBayesFactor`-objects.
* `get_priors()` now works for `stanmvreg` objects.
* Other minor improvements.

## Bug fixes

* Better detect Tweedie-models in `model_info()`.
* Fixed issue in `find_random_slopes()` for *panelr*-models with multiple random-effect parts.
* Fixed issues with `zerotrunc` models.
* Fixed issues with `brmsfit` models with correlated random effects.
* Fixed isses with edge-cases in `clean_names()`.
* Further minor bug fixes.

# insight 0.7.1

## New supported model classes

* `complmrob` (*complmrob*), `fixest` (*fixest*), `mclogit` and `mmclogit` (*mclogit*).

## Bug fixes

* Fixed bug in `find_formula()` for mixed models, when random effects are written before any fixed effects terms (like `social ~ (1|school) + open + extro`).
* Fixed bug in `model_info()` for *VGAM* models, where logit-link was not always correctly identified.
* Fixed issue in `get_priors()` for *brmsfit* models, where parameters of conditional and zero-inflated model components had identical names. This caused errors in `bayestestR::simulate_prior()`.
* Fixed CRAN check issue.

# insight 0.7.0

## Breaking changes

* In order to unify column names across easystats-packages, `get_parameters()` and `get_priors()` now return column names according to our naming conventions (i.e. capitalized column names).
* `model_info()` returned both `$is_zeroinf` and `$is_zero_inflated` for zero-inflated models. Now `$is_zeroinf` is softly deprecated, so `model_info()` will return `$is_zero_inflated` only in future updates.

## New supported model classes

* `aareg` (*survival*), `brmultinom` and `bracl` (*brglm2*), and `wbgee` (*panelr*). Furthermore, for different model-types from *panelr* models (within-between, between, etc.) are now also supported.
* Preliminary support for `rma` models (*metafor*).

## Changes to functions

* `get_statistic()` supports `multinom` models (*nnet*).
* `link_inverse()` gets a `what`-argument, to return the link-inverse function for specific distribution parameters from **gamls** models.

## Bug fixes

* Fixed edge case for `find_weights()`.
* Fixed bug in `get_statistic()` for *glmmTMB* models that won't return any data.

# insight 0.6.0

## New supported model classes

* `bayesx` (*R2BayesX*), `bamlss` (*bamlss*) and `flexsurvreg` (*flexsurv*). Note that support for these models is still somewhat experimental.
* Support for *lavaan* and *blavaan* was added, but only applies to some of the functions: `get_data()`, `get_parameters()`, `find_parameters()`, `clean_parameters()`, `find_algorithm()` and `get_priors()` (the two latter only for *blavaan*).

## New functions

* `get_statistic()` to return the test statistic of model estimates.
* `get_varcov()` to return the variance-covariance matrix for models.
* `supported_models()` to print a list of supported models.

## Changes to functions

* `model_info()` now returns the element `is_survival` for survival models.
* `model_info()` now returns the element `is_truncated` for truncated regression, or *brmsfit* models with `trunc()` as additional response part.
* `model_info()` now recognizes beta and beta inflated families from package *gamlss*.
* Better support for nonlinear quantile regression (`quantreg::nlrq()`).
* Better support for nonlinear mixed models (`lme4::nlmer()`). Note that model-specification requires the random term to be written in parentheses, i.e. `(slope | group)`.

## Bug fixes

* Fixed issues in `get_data()`, `find_parameters()` and `get_parameters()` for *gamlss* models.
* Fixed issue in `get_data()` for *plm* models, where the `index`-argument was used in the `plm()`-function call.
* Fixed issue in `get_data()`, `find_predictors()` and `find_variables()` for *brmsfit*  multi-membership-models.
* `is_model()` did not recognize objects of class `anova` and `manova`.
* `model_info()` now correctly recognizes censored regression models from *brmsfit*.
* Fixed issues in `find_parameters()` and `get_parameters()` with *multinom* models.
* Fixed issues in `clean_names()` for cases where variable transformations where made in specific patterns, like `log(test/10)`.

# insight 0.5.0

## Breaking Changes

* The previous `is_model()` function has been renamed to `is_model_supported()` since it was unclear if the function checked the entered object was a model or a supported model in *insight*. The new `is_model()` function checks if the entered object is a model object, while `is_model_supported()` checks if a supported model object.

## New functions

* `find_statistic()` to return the test statistic of a regression model.
* `format_value()` and `format_table()` as utility-functions to format (model) output, especially for tabular output.
* `color_if()` as utility-function to add color formatting to values, depending on certain conditions.

## General

* Make extraction of model family information more stable for gam-objects.

## Changes to functions

* `find_parameters()` and `get_parameters()` now also support objects of class `sim` and `sim.merMod` (from `arm::sim()`).
* `get_variance()` now also supports models of class *clmm*.
* `find_predictors()` and `find_variables()` now include the Euclidean distance matrix for spatial models from *glmmTMB* (returned as random effects element, or more precise, as random slope).

## Bug fixes

* `find_formula()` now extracts group factors of random effects for *gamlss* models.
* `find_parameters()` and `get_parameters()` no longer show `NA` coefficients from group factors of random effects for *gamlss* models.
* `find_parameters()` and `get_parameters()` did not work for multivariate response models of class *brmsfit* when argument `parameters` was specified.
* `get_data()` dropped value and variable label attributes, when model frame contained matrix variables (like splines).
* `get_priors()` swapped column names `location` and `scale` for *brmsfit* -objects.
* `get_parameters()` did not work for *glmmTMB* models without zero-inflation component.
* `find_predictors()` did not remove parentheses from terms in multiple nested random effects.
* Better support for *gam* models (package *mgcv*) with `ziplss` or `mvn` families.

# insight 0.4.1

## Changes to functions

* `get_variance()` now supports models with Gamma-family.
* `get_weights()` and `find_weights()` now work for *brms*-models.

## Bug fixes

* Fix CRAN-check issues due to recent update from the *panelr*-package.
