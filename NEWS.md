# insight 0.11.2

## Breaking changes

* `format_table()` is an alias for `parameters_table()`, and is no longer
  referring to `export_table()`.

## New supported model classes

* Support for `coxr` (*coxrobust*), `coeftest` (*lmtest*), `ivFixed` (*ivFixed*),
  `ivprobit` (*ivprobit*), `riskRegression` (*riskRegression*). `summary.lm`.

## New functions

* Added `get_predicted()` as a robust alternative to `stats::fitted()`

* Added `loglikelihood()` function as a robust alternative to `stats::logLik()`.

* Added `ellipsis_info()` to specify the nature of ellipsis (`...`) inputs.

* Added generic `print_html()`, to allow other packages to create tables in
  HTML format (via `export_table()`) when not printing the output to console.

* `is_mixed_model()`, to safely check if a model is a mixed effects model. This
  function also works for multivariate response models.

* `get_residuals()` to extract model residuals.

## Changes to functions

* `find_formula()`, `find_predictor()`, `find_random()` and related functions
  now also return names of random effects from generalized  additive mixed
  models (`gamm`, `gamm4`, `stan_gamm4`).

* Added support for more BFBayesFactor objects.

* `model_info()` now returns `$is_xtab` for `chisq.test()` and
  `BayesFactor::contingencyTableBF()`. Furthermore, the `$family` element for
  those objects is set to `"categorical"`.
  
* `n_obs()` now handles number of observations from models with binomial family
  correctly when these have matrix-columns as response variable.

## Bug fixes

* Fixed issue in `find_statistic()` for *fixest* models, which did not
  return the correct value `"t-statistic"` for `feols()`.

# insight 0.11.1

## General

* Warnings that formerly were printed using `print_color()` now use `warning()`,
  to better suppress warning messages if required.

## New functions

* `find_smooth()`, to return in particular smooth terms used in a model.

## Changes to functions

* `get_variance()` and `get_variance_random()` gain a `tolerance`-argument, to
  set the tolerance level for singularity checks when computing random effect
  variances.

* `parameters_table()` formats more objects from the *easystats* packages,
  like ROPE-range or `p_rope()`.

* `find_statistic()` now supports models of class *scam*.

* `get_data()` now also supports `htest`-object, where possible.

## Bug fixes

* Fix CRAN check issues.

* `find_formula()` for `stan_gamm4()` now correctly includes random effects.

# insight 0.11.0

## Breaking changes

* `model_info()` now also detects models from `oneway.test()`, `binom.test()`
  `chisq.test()`, `mcnemar.test()` and `prop.test()`. Furthermore,
  `model_info()` better deals with objects from `BFBayesFactor`, and censored
  regression models no longer return `TRUE` for `$is_linear`.

* `format_table()` is going to be renamed in a future update. Please use its
  alias `export_table()`.

## New supported model classes

* Support for `scam` (*scam*), `meta_random` and `meta_fixed` (*metaBMA*), `Glm`
  (*rms*), `ridgelm` (*MASS*), `mediate` (*mediation*). Partial support for
  `mcmc.list` (e.g. *bayesGARCH*)

## New function

* `parameters_table()`, which was moved from package *parameters* to *insight*.
  Note that this function is going to be renamed into `format_table()` in a
  future update.

* `find_offset()`, to find the name of offset-terms.

* Added generics for `display()` and `print_md()`, to allow other packages to
  create tables in other formats when not printing the output to console.

## Changes to functions

* `standardize_names()` tries to be as loyal to the *broom*-naming conventions
  as possible.

* The function of the `brackets`-argument in `format_ci()` was changed. It is
  now also possible to provide a length-two character vector, to define own
  brackets that encompass the CI-values.

* Related to the change in `format_ci()`, the function of the
  `brackets`-argument in `parameters_table()` was changed accordingly.
  Furthermore, `parameters_table()` gains a `preserve_attributes`-argument, to
  preserve any attributes from the input data frame.

* `export_table()` gains several new arguments that allows to create tables in
  markdown-format.

* `print_parameters()` gains a `keep_parameter_column`-argument, to keep
  (default) both the `"Cleaned_Parameter"` and `"Parameter"` columns, or - if
  `FALSE` - use `"Cleaned_Parameter"` as new `"Parameter"` column.

## Bug fixes

### `get_data()`

* Fixed issue in `get_data()` for `MixMod` objects, which were caused due to
  internal changes in *GLMMadaptive*.

* `get_data()` for zero-inflated models from *pscl* did not include the
  offset-term in cases where the offset was defined as argument, not inside the
  model formula.

* Fixed issue in `get_data()` for `coxph` models with survival-objects with
  `event`-argument as response.

* Fixed edge case in `get_data()` for column name of response values that were
  log-transformed using `log(x+1)`.

### Other bug fixes

* Fixed issue with `survreg` models that included `strata()` in their formula.

* Fixed warning in CRAN checks for forthcoming R-devel.

# insight 0.10.0

## New function

* `get_sigma()` to return the residual standard deviation.

* `standardize_names()`, which was moved from package *parameters* to
  *insight*.

## New supported model classes

* Support for `maov` (*stats*), `HLfit` (*spaMM*), preliminary support for
  `margins` (*margins*), `merModList` (*merTools*).

## General

* Better support for (weighted) multivariate response models of class `mlm` for
  functions like `get_varcov()` or `clean_parameters()`.

* Make `find_formula()` work with t-tests from *BayesFactor*.

* Improved handling for *mira* objects.

## Changes to functions

* `format_bf()` gains a `na_reference` argument, to set the "reference" for
  Bayes factor values that are `NA`, and an `exact` argument for returning
  scientific formatted extreme values.

* `format_value()` gains a `zap_small` argument, to prevent scientific printing
  of numbers if these have more decimal places than indicated by `digits`.

* `get_weights()` now also returns `NULL` when all weights were 1.

* `get_parameters()` for *BFBayesFactor* objects gets a `verbose` argument.

* `get_parameters()` for *emmGrid* and *emm_list* objects gets a `summary`
  argument, to either return the full posterior samples or the summarized
  centrality indices for Bayesian models.

* `find_formula()` for `MuMIn::model.avg()` now tries to retrieve the random
  effects part of a formula, when present.

* `get_weights()` gains a `na_rm` argument to remove possible missing values.

## Bug fixes

* Fix issues with one-sample Bayesian t-tests (
  https://github.com/easystats/parameters/issues/297 ).

* Fix issue in `format_value()` that printed `"100%"` as `"1e+02%"`.

* Removed unnecessary white-spaces in `format_ci()` when upper or lower interval
  was larger than 1e+5.

* `has_intercept()` did not work correctly when intercept was removed from
  formula using `-1`.

* `find_terms()` now shows removal of intercept formula using `-1` as term
  `"-1"`.

* Fix issues with `get_statistic()` for *vgam* models.

# insight 0.9.6

## Changes to functions

* `get_data()` now works for models from `afex_aov()`.

* `get_parameters()` returns a more informative message for `BFBayesFactor`
  objects when not the first model is indexed.

* `clean_names()` now also removes `exp()`-pattern.

* `clean_names()` for character-objects now works with "interaction patterns"
  (like `clean_names("scale(a):scale(b)")`).

* `format_bf()` gains a `protect_ratio` argument, to print numbers smaller than
  1 as ratios.

## Bug fixes

* Fix issues in CRAN checks.

* `get_priors()` now works for more complex `BFBayesFactor` objects that have
  multiple custom priors.

# insight 0.9.5

## Breaking changes

* `get_data()` did not always "back-transform" log-transformed or scaled
  variables to return the original values. Now this bug has been fixed, and
  `get_data()` should return all variables on the original scale (as if these
  variables were not transformed), as stated in the docs.

## Bug fixes

* `get_data()` now returns the correct original data for "empty" polynomials
  (i.e. `poly(x, 1)`).

* Fix CRAN check issues due to latest _estimatr_ update.

# insight 0.9.1

## New supported model classes

* Support for `mipo` (*mice*), `lqmm` and `lqm` (*lqmm*). Preliminary support
  for `semLME` (*smicd*), `mle` (*stats4*) and `mle2` (*bbmle*).

## Changes to functions

* `model_info()` returns `$is_meta = TRUE` for *brms*-meta-analysis models.

* Make `find_statistic()` work with `mgcv::bam()`.

* `get_variance()` now also support `truncated_nbinom2()` family from
  *glmmTMB*.

## Bug fixes

* Fixed issue with correctly detecting sigma-parameters in `find_parameters()`
  for multiple-response `brmsfit`-models.

* Fixed issue with `find_formula()` for models from `stan_nlmer()`.

* Fixed issues with `find_terms()` when response variable included a namespace,
  like `survival::Surv()`.

* Fixed issues with `get_priors()` for _stanreg_ models, probably caused by the
  latest update to *rstanarm 2.21.2*.

* Fixed issues in `get_variance()` for *brmsfit* models.

* Fixed some issues around `crq` objects (package *quantreg*).

# insight 0.9.0

## New supported model classes

* `BGGM` (*BGGM*), `metaplus` (*metaplus*), `glht` (*multcomp*), `glmm`
  (*glmm*), improved support for `manova` (*stats*)

## New functions

* Value formatting functions `format_bf()`, `format_pd()`, `format_p()`,
  `format_rope()` and `format_number()` were moved from package *parameters* to
  *insight*.

## Changes to functions

* `get_variance()` now also returns the correlation among random slopes.

* `get_variance()` now also (partially) supports `brmsfit` models.

* `get_parameters()` for models that return (posterior or simulated) samples of
  model parameters gains a `summary`-argument, which - if `TRUE` - returns a
  point-estimate (mean of samples) instead of the full samples.

* `format_p()` returns `"> .999"` for p-values equal to or greater than 0.999.

## Bug fixes

* Fixed issue in `find_formula()` that did not properly work for models with
  random effects in formula (in *lme4* notation), when random effects were in
  between fixed effects parts.

* `get_variance()` did not return variance components for random effects for
  null-models with random slopes.

* Fixed issue with `get_variance()` for `lme`-models with categorical random
  slope.

* Fixed issue that occurred since R 4.0.0 in `find_weights()` when function call
  had no `weights`-argument.

* Fixed issue in `get_data()` for models with `cbind()`-response variables and
  matrix-like variables in the model frame (e.g. when using `poly()`).

* Fixed issues with `PROreg::BBmm()`, due to changes in latest package update.

# insight 0.8.5

## New supported model classes

* `robmixglm` (*robmixglm*), `betamfx`, `logitmfx`, `poissonmfx`, `probitmfx`,
  `negbinmfx`, `betaor`, `logitor`, `poissonirr`, `negbinirr` (*mfx*), partial
  support for *emmGrid*, *stanfit* and *bayesQR*.

## Changes to functions

* `get_varcov.glmmTMB()` now also returns the variance-covariance matrix for the
  dispersion model.

* `model_info()` returns `$is_dispersion = TRUE` for *glmmTMB* objects with
  dispersion model.

* `clean_names()` now also removes mathematical operations (like `100 * log(x)`,
  which will return `"x"`).

* `format_ci()` gains a `missing` argument, as option how to print missing
  values.

* `format_value()` now uses `NA_character_` as missing if `missing = NA`.

* `format_value()` also converts small numbers with many decimals into
  scientific notation.

# insight 0.8.4

## General

* *HRQoL* was removed from suggested packages, as it was removed from CRAN.

* Better support for dispersion models in *glmmTMB*.

## Changes to functions

* `null_model()` now also works for non-mixed models.

* `get_variance()` now also computes variance components for models (from mixed
  models packages) without random effects.

* Improved support for `afex_aov` and `aovlist` (i.e. Anova with error term).

## Bug fixes

* Fixed some issues with deparsings `NULL` strings under R 4.0.0.

* Fixed accuracy in `get_variance()` for models from Gamma family.

* Fixed edge case in `clean_names()`.

* Fixed issues with `find_formula.lme()` under R 4.0.0.

* Fixed issues with examples from `clean_names()` under R-devel.

# insight 0.8.3

## General

* The function to calculate null-models for mixed effects models is now exported
  (`null_model()`.)

## New supported model classes

* `arima` (*stats*), `averaging` (*MuMIn*)

## Changes to functions

* Improve family detection in `model_info()`, `link_inverse()` and
  `link_function()` for *MCMCglmm*.

* Minor revisions to meet changes in *mlogit* package.

* Improve support for *bayesx* and *BBmm* models.

## Bug fixes

* Fixed issue in `find_parameters()` and `clean_parameters()` for *brmsfit*
  models with specific variable name patterns.

* Fixed issue in `format_ci()` when confidence interval only contained `NA`s and
  `width` was set to `"auto"`.

* Fixed issue in `find_formula()` for mixed models when formula contained
  parentheses in the non-random parts, around a certain set of predictors.

* Fixed issue in `get_priors.BFBayesFactor()` for `BFMetat` class.

* Fixed issue in `clean_parameters.BFBayesFactor()` when model contained
  interaction terms and these were assigned to the "extra" component.

# insight 0.8.2

## Breaking changes

* `model_info()` now only returns `TRUE` for `$is_ordinal`, when model is an
  ordinal or cumulative link model. In past versions, `$is_ordinal` was also
  `TRUE` for multinomial models.

## New supported model classes

* `bife` (*bife*), `bcplm` and `zcpglm` (*cplm*)

## General

* Improved support for `clogit`-models.

## Bug fixes

* Fixed issue in `find_weights()` for `merMod` models.

* Fixed issue in `get_data()` for models with weights, when weights also
  contained missing data.

* Fixed issue in `get_data()` for mixed models with complex offset-terms.

* Fixed issue in `get_statistic()` for *zeroinfl* models with
  theta-coefficients.

* Fixed issue in `get_statistic()` for *lmerModLmerTest* models with.

* Fixed issue in `find_parameters()` for *brmsfit* models for rare situations
  where a specific pattern of variables names, when used as random effects, did
  not properly separate fixed from random effects in the return value.

* Fixed issue related to CRAN checks.

# insight 0.8.1

## New supported model classes

* `cglm` (*cglm*), `DirichletRegModel` (*DirichletReg*).

## General

* Improved efficiency of `find_parameters()` and `get_parameters()` for mixed
  models with large samples and many random effects, and only fixed effects
  where requested.

## Changes to functions

* `model_info()` now returns `$is_multinomial` for multinomial (but not ordinal
  or cumulative) link models.

* `format_value()` gets an `as_percent` argument to format values as
  percentages.

## Bug fixes

* Fixed issue in `get_data()` for *clmm2*-models.

* Fixed issue in `get_data()` for models that used the `lspline()`-function.

* Fixed issue in `get_statistic()` for *multinom* models.

* Fixed issue in `get_priors()` for *stanreg* models with flat intercept
  priors.

* Fixed tests that failed due to latest **fixest** update.

# insight 0.8.0

## New supported model classes

* `brglm` (*brglm*), `cgam`, `cgamm` (*cgam*), `cpglm`, `cpglmm` (*cplm*),
  `feglm` (*apaca*), `glmmadmb` (*glmmADMB*), `glmx` (*glmx*), partial support
  for `mcmc` (*coda*), `mixor` (*mixor*), `MANOVA`, `RM` (*MANOVA.RM*).

## General

* Better handling of `clm2`, `clmm2` and `rqss` models.

## New functions

* `format_ci()` (re-implemented and slightly enhanced from _parameters_), to
  format confidence/credible intervals.

## Changes to functions

* `find_parameters()` now also works for `BFBayesFactor` objects.

* Suppress non-informative warning in `get_data()` for model data with weights.

* `format_value()` automatically uses scientific notation for *very* large
  numbers (> 1e+5). Furthermore, the check for integer values was made more
  robust, to avoid warnings when checking *very* large numbers for integer
  type.

* Improved `find_parameters()`, `get_parameters()` and `clean_parameters()` for
  `BFBayesFactor`-objects.

* `get_priors()` now works for `stanmvreg` objects.

* Other minor improvements.

## Bug fixes

* Better detect Tweedie-models in `model_info()`.

* Fixed issue in `find_random_slopes()` for *panelr*-models with multiple
  random-effect parts.

* Fixed issues with `zerotrunc` models.

* Fixed issues with `brmsfit` models with correlated random effects.

* Fixed issue with edge-cases in `clean_names()`.

* Fixed issue with breaking changes with latest *brms*-update.

* Further minor bug fixes.

# insight 0.7.1

## New supported model classes

* `complmrob` (*complmrob*), `fixest` (*fixest*), `mclogit` and `mmclogit`
  (*mclogit*).

## Bug fixes

* Fixed bug in `find_formula()` for mixed models, when random effects are
  written before any fixed effects terms (like `social ~ (1|school) + open +
  extro`).

* Fixed bug in `model_info()` for *VGAM* models, where logit-link was not always
  correctly identified.

* Fixed issue in `get_priors()` for *brmsfit* models, where parameters of
  conditional and zero-inflated model components had identical names. This
  caused errors in `bayestestR::simulate_prior()`.

* Fixed CRAN check issue.

# insight 0.7.0

## Breaking changes

* In order to unify column names across easystats-packages, `get_parameters()`
  and `get_priors()` now return column names according to our naming conventions
  (i.e. capitalized column names).

* `model_info()` returned both `$is_zeroinf` and `$is_zero_inflated` for
  zero-inflated models. Now `$is_zeroinf` is softly deprecated, so
  `model_info()` will return `$is_zero_inflated` only in future updates.

## New supported model classes

* `aareg` (*survival*), `brmultinom` and `bracl` (*brglm2*), and `wbgee`
  (*panelr*). Furthermore, for different model-types from *panelr* models
  (within-between, between, etc.) are now also supported.

* Preliminary support for `rma` models (*metafor*).

## Changes to functions

* `get_statistic()` supports `multinom` models (*nnet*).

* `link_inverse()` gets a `what`-argument, to return the link-inverse function
  for specific distribution parameters from **gamls** models.

## Bug fixes

* Fixed edge case for `find_weights()`.

* Fixed bug in `get_statistic()` for *glmmTMB* models that won't return any
  data.

# insight 0.6.0

## New supported model classes

* `bayesx` (*R2BayesX*), `bamlss` (*bamlss*) and `flexsurvreg` (*flexsurv*).
  Note that support for these models is still somewhat experimental.

* Support for *lavaan* and *blavaan* was added, but only applies to some of the
  functions: `get_data()`, `get_parameters()`, `find_parameters()`,
  `clean_parameters()`, `find_algorithm()` and `get_priors()` (the two latter
  only for *blavaan*).

## New functions

* `get_statistic()` to return the test statistic of model estimates.

* `get_varcov()` to return the variance-covariance matrix for models.

* `supported_models()` to print a list of supported models.

## Changes to functions

* `model_info()` now returns the element `is_survival` for survival models.

* `model_info()` now returns the element `is_truncated` for truncated
  regression, or *brmsfit* models with `trunc()` as additional response part.

* `model_info()` now recognizes beta and beta inflated families from package
  *gamlss*.

* Better support for nonlinear quantile regression (`quantreg::nlrq()`).

* Better support for nonlinear mixed models (`lme4::nlmer()`). Note that
  model-specification requires the random term to be written in parentheses,
  i.e. `(slope | group)`.

## Bug fixes

* Fixed issues in `get_data()`, `find_parameters()` and `get_parameters()` for
  *gamlss* models.

* Fixed issue in `get_data()` for *plm* models, where the `index`-argument was
  used in the `plm()`-function call.

* Fixed issue in `get_data()`, `find_predictors()` and `find_variables()` for
  *brmsfit* multi-membership-models.

* `is_model()` did not recognize objects of class `anova` and `manova`.

* `model_info()` now correctly recognizes censored regression models from
  *brmsfit*.

* Fixed issues in `find_parameters()` and `get_parameters()` with *multinom*
  models.

* Fixed issues in `clean_names()` for cases where variable transformations where
  made in specific patterns, like `log(test/10)`.

# insight 0.5.0

## Breaking Changes

* The previous `is_model()` function has been renamed to `is_model_supported()`
  since it was unclear if the function checked the entered object was a model or
  a supported model in *insight*. The new `is_model()` function checks if the
  entered object is a model object, while `is_model_supported()` checks if a
  supported model object.

## New functions

* `find_statistic()` to return the test statistic of a regression model.

* `format_value()` and `format_table()` as utility-functions to format (model)
  output, especially for tabular output.

* `color_if()` as utility-function to add color formatting to values, depending
  on certain conditions.

## General

* Make extraction of model family information more stable for gam-objects.

## Changes to functions

* `find_parameters()` and `get_parameters()` now also support objects of class
  `sim` and `sim.merMod` (from `arm::sim()`).

* `get_variance()` now also supports models of class *clmm*.

* `find_predictors()` and `find_variables()` now include the Euclidean distance
  matrix for spatial models from *glmmTMB* (returned as random effects element,
  or more precise, as random slope).

## Bug fixes

* `find_formula()` now extracts group factors of random effects for *gamlss*
  models.

* `find_parameters()` and `get_parameters()` no longer show `NA` coefficients
  from group factors of random effects for *gamlss* models.

* `find_parameters()` and `get_parameters()` did not work for multivariate
  response models of class *brmsfit* when argument `parameters` was specified.

* `get_data()` dropped value and variable label attributes, when model frame
  contained matrix variables (like splines).

* `get_priors()` swapped column names `location` and `scale` for *brmsfit*
  -objects.

* `get_parameters()` did not work for *glmmTMB* models without zero-inflation
  component.

* `find_predictors()` did not remove parentheses from terms in multiple nested
  random effects.

* Better support for *gam* models (package *mgcv*) with `ziplss` or `mvn`
  families.

# insight 0.4.1

## Changes to functions

* `get_variance()` now supports models with Gamma-family.

* `get_weights()` and `find_weights()` now work for *brms*-models.

## Bug fixes

* Fix CRAN-check issues due to recent update from the *panelr*-package.

