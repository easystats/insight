# Package index

## Comprehensive Information from Model Objects

- [`model_info()`](https://easystats.github.io/insight/reference/model_info.md)
  : Access information from model objects

## Find Information from Model Objects

Functions to access names or information from model objects. These
functions have no `get_*()`-counterpart.

- [`find_algorithm()`](https://easystats.github.io/insight/reference/find_algorithm.md)
  : Find sampling algorithm and optimizers
- [`find_formula()`](https://easystats.github.io/insight/reference/find_formula.md)
  [`formula_ok()`](https://easystats.github.io/insight/reference/find_formula.md)
  : Find model formula
- [`find_interactions()`](https://easystats.github.io/insight/reference/find_interactions.md)
  : Find interaction terms from models
- [`find_offset()`](https://easystats.github.io/insight/reference/find_offset.md)
  : Find possible offset terms in a model
- [`find_random_slopes()`](https://easystats.github.io/insight/reference/find_random_slopes.md)
  : Find names of random slopes
- [`find_smooth()`](https://easystats.github.io/insight/reference/find_smooth.md)
  : Find smooth terms from a model object
- [`find_terms()`](https://easystats.github.io/insight/reference/find_terms.md)
  : Find all model terms
- [`find_variables()`](https://easystats.github.io/insight/reference/find_variables.md)
  : Find names of all variables

## Get Data from Model Objects

Functions to get values or data from model objects. These functions have
no `find_*()`-counterpart.

- [`get_data()`](https://easystats.github.io/insight/reference/get_data.md)
  : Get the data that was used to fit the model
- [`get_datagrid()`](https://easystats.github.io/insight/reference/get_datagrid.md)
  : Create a reference grid
- [`get_df()`](https://easystats.github.io/insight/reference/get_df.md)
  : Extract degrees of freedom
- [`get_intercept()`](https://easystats.github.io/insight/reference/get_intercept.md)
  : Get the value at the intercept
- [`get_loglikelihood()`](https://easystats.github.io/insight/reference/get_loglikelihood.md)
  [`loglikelihood()`](https://easystats.github.io/insight/reference/get_loglikelihood.md)
  [`get_loglikelihood_adjustment()`](https://easystats.github.io/insight/reference/get_loglikelihood.md)
  : Log-Likelihood and Log-Likelihood correction
- [`get_modelmatrix()`](https://easystats.github.io/insight/reference/get_modelmatrix.md)
  : Model Matrix
- [`get_predicted()`](https://easystats.github.io/insight/reference/get_predicted.md)
  : Model predictions (robust) and their confidence intervals
- [`get_predicted_ci()`](https://easystats.github.io/insight/reference/get_predicted_ci.md)
  : Confidence intervals around predicted values
- [`get_priors()`](https://easystats.github.io/insight/reference/get_priors.md)
  : Get summary of priors used for a model
- [`get_residuals()`](https://easystats.github.io/insight/reference/get_residuals.md)
  : Extract model residuals
- [`get_sigma()`](https://easystats.github.io/insight/reference/get_sigma.md)
  : Get residual standard deviation from models
- [`get_variance()`](https://easystats.github.io/insight/reference/get_variance.md)
  [`get_variance_residual()`](https://easystats.github.io/insight/reference/get_variance.md)
  [`get_variance_fixed()`](https://easystats.github.io/insight/reference/get_variance.md)
  [`get_variance_random()`](https://easystats.github.io/insight/reference/get_variance.md)
  [`get_variance_distribution()`](https://easystats.github.io/insight/reference/get_variance.md)
  [`get_variance_dispersion()`](https://easystats.github.io/insight/reference/get_variance.md)
  [`get_variance_intercept()`](https://easystats.github.io/insight/reference/get_variance.md)
  [`get_variance_slope()`](https://easystats.github.io/insight/reference/get_variance.md)
  [`get_correlation_slope_intercept()`](https://easystats.github.io/insight/reference/get_variance.md)
  [`get_correlation_slopes()`](https://easystats.github.io/insight/reference/get_variance.md)
  : Get variance components from random effects models
- [`get_varcov()`](https://easystats.github.io/insight/reference/get_varcov.md)
  : Get variance-covariance matrix from models
- [`get_deviance()`](https://easystats.github.io/insight/reference/get_deviance.md)
  : Model Deviance
- [`get_family()`](https://easystats.github.io/insight/reference/get_family.md)
  : A robust alternative to stats::family
- [`get_mixed_info()`](https://easystats.github.io/insight/reference/get_mixed_info.md)
  : Extract various information from mixed models

## Find Information or Get Data from Model Objects

Functions to list model-specific objects or to extract values (or data)
associated with model-specific objects.

- [`find_auxiliary()`](https://easystats.github.io/insight/reference/find_auxiliary.md)
  : Find auxiliary (distributional) parameters from models
- [`get_auxiliary()`](https://easystats.github.io/insight/reference/get_auxiliary.md)
  [`get_dispersion()`](https://easystats.github.io/insight/reference/get_auxiliary.md)
  : Get auxiliary parameters from models
- [`find_parameters()`](https://easystats.github.io/insight/reference/find_parameters.md)
  : Find names of model parameters
- [`get_parameters()`](https://easystats.github.io/insight/reference/get_parameters.md)
  : Get model parameters
- [`find_predictors()`](https://easystats.github.io/insight/reference/find_predictors.md)
  : Find names of model predictors
- [`get_predictors()`](https://easystats.github.io/insight/reference/get_predictors.md)
  : Get the data from model predictors
- [`find_random()`](https://easystats.github.io/insight/reference/find_random.md)
  : Find names of random effects
- [`get_random()`](https://easystats.github.io/insight/reference/get_random.md)
  : Get the data from random effects
- [`find_response()`](https://easystats.github.io/insight/reference/find_response.md)
  : Find name of the response variable
- [`get_response()`](https://easystats.github.io/insight/reference/get_response.md)
  : Get the values from the response variable
- [`find_statistic()`](https://easystats.github.io/insight/reference/find_statistic.md)
  : Find statistic for model
- [`get_statistic()`](https://easystats.github.io/insight/reference/get_statistic.md)
  : Get statistic associated with estimates
- [`find_transformation()`](https://easystats.github.io/insight/reference/find_transformation.md)
  : Find possible transformation of model variables
- [`get_transformation()`](https://easystats.github.io/insight/reference/get_transformation.md)
  : Return function of transformed response variables
- [`find_weights()`](https://easystats.github.io/insight/reference/find_weights.md)
  : Find names of model weights
- [`get_weights()`](https://easystats.github.io/insight/reference/get_weights.md)
  : Get the values from model weights

## Access Further Information from Model Objects

Functions to access specific information from model objects.

- [`get_call()`](https://easystats.github.io/insight/reference/get_call.md)
  : Get the model's function call
- [`get_model()`](https://easystats.github.io/insight/reference/get_model.md)
  : Get a model objects that is saved as attribute
- [`link_function()`](https://easystats.github.io/insight/reference/link_function.md)
  : Get link-function from model object
- [`link_inverse()`](https://easystats.github.io/insight/reference/link_inverse.md)
  : Get link-inverse function from model object
- [`model_name()`](https://easystats.github.io/insight/reference/model_name.md)
  : Name the model
- [`n_grouplevels()`](https://easystats.github.io/insight/reference/n_grouplevels.md)
  : Count number of random effect levels in a mixed model
- [`n_obs()`](https://easystats.github.io/insight/reference/n_obs.md) :
  Get number of observations from a model
- [`n_parameters()`](https://easystats.github.io/insight/reference/n_parameters.md)
  : Count number of parameters in a model
- [`trim_ws()`](https://easystats.github.io/insight/reference/trim_ws.md)
  [`n_unique()`](https://easystats.github.io/insight/reference/trim_ws.md)
  [`safe_deparse()`](https://easystats.github.io/insight/reference/trim_ws.md)
  [`safe_deparse_symbol()`](https://easystats.github.io/insight/reference/trim_ws.md)
  [`has_single_value()`](https://easystats.github.io/insight/reference/trim_ws.md)
  : Small helper functions

## Test Model Properties

Functions to test specific properties of model objects.

- [`all_models_equal()`](https://easystats.github.io/insight/reference/all_models_equal.md)
  [`all_models_same_class()`](https://easystats.github.io/insight/reference/all_models_equal.md)
  : Checks if all objects are models of same class
- [`has_intercept()`](https://easystats.github.io/insight/reference/has_intercept.md)
  : Checks if model has an intercept
- [`is_bayesian_model()`](https://easystats.github.io/insight/reference/is_bayesian_model.md)
  : Checks if a model is a Bayesian model
- [`is_converged()`](https://easystats.github.io/insight/reference/is_converged.md)
  : Convergence test for mixed effects models
- [`is_empty_object()`](https://easystats.github.io/insight/reference/is_empty_object.md)
  : Check if object is empty
- [`is_gam_model()`](https://easystats.github.io/insight/reference/is_gam_model.md)
  : Checks if a model is a generalized additive model
- [`is_mixed_model()`](https://easystats.github.io/insight/reference/is_mixed_model.md)
  : Checks if a model is a mixed effects model
- [`is_model()`](https://easystats.github.io/insight/reference/is_model.md)
  [`is_regression_model()`](https://easystats.github.io/insight/reference/is_model.md)
  : Checks if an object is a regression model or statistical test object
- [`is_model_supported()`](https://easystats.github.io/insight/reference/is_model_supported.md)
  [`supported_models()`](https://easystats.github.io/insight/reference/is_model_supported.md)
  : Checks if a regression model object is supported by the insight
  package
- [`is_multivariate()`](https://easystats.github.io/insight/reference/is_multivariate.md)
  : Checks if an object stems from a multivariate response model
- [`is_nested_models()`](https://easystats.github.io/insight/reference/is_nested_models.md)
  : Checks whether a list of models are nested models
- [`is_nullmodel()`](https://easystats.github.io/insight/reference/is_nullmodel.md)
  : Checks if model is a null-model (intercept-only)

## Value Formatting

Functions for formatting (summary) output for printing

- [`color_if()`](https://easystats.github.io/insight/reference/color_if.md)
  [`colour_if()`](https://easystats.github.io/insight/reference/color_if.md)
  : Color-formatting for data columns based on condition
- [`print_color()`](https://easystats.github.io/insight/reference/print_color.md)
  [`print_colour()`](https://easystats.github.io/insight/reference/print_color.md)
  [`color_text()`](https://easystats.github.io/insight/reference/print_color.md)
  [`colour_text()`](https://easystats.github.io/insight/reference/print_color.md)
  [`color_theme()`](https://easystats.github.io/insight/reference/print_color.md)
  : Coloured console output
- [`format_bf()`](https://easystats.github.io/insight/reference/format_bf.md)
  : Bayes Factor formatting
- [`format_capitalize()`](https://easystats.github.io/insight/reference/format_capitalize.md)
  : Capitalizes the first letter in a string
- [`format_ci()`](https://easystats.github.io/insight/reference/format_ci.md)
  : Confidence/Credible Interval (CI) Formatting
- [`format_message()`](https://easystats.github.io/insight/reference/format_message.md)
  [`format_alert()`](https://easystats.github.io/insight/reference/format_message.md)
  [`format_warning()`](https://easystats.github.io/insight/reference/format_message.md)
  [`format_error()`](https://easystats.github.io/insight/reference/format_message.md)
  : Format messages and warnings
- [`format_number()`](https://easystats.github.io/insight/reference/format_number.md)
  : Convert number to words
- [`format_p()`](https://easystats.github.io/insight/reference/format_p.md)
  : p-values formatting
- [`format_pd()`](https://easystats.github.io/insight/reference/format_pd.md)
  : Probability of direction (pd) formatting
- [`format_rope()`](https://easystats.github.io/insight/reference/format_rope.md)
  : Percentage in ROPE formatting
- [`format_string()`](https://easystats.github.io/insight/reference/format_string.md)
  : String Values Formatting
- [`format_table()`](https://easystats.github.io/insight/reference/format_table.md)
  : Parameter table formatting
- [`format_value()`](https://easystats.github.io/insight/reference/format_value.md)
  [`format_percent()`](https://easystats.github.io/insight/reference/format_value.md)
  : Numeric Values Formatting
- [`text_remove_backticks()`](https://easystats.github.io/insight/reference/text_remove_backticks.md)
  : Remove backticks from a string

## Utilities

Functions that are not specifically related to access model information,
but rather small utility functions (mostly for printing)

- [`clean_names()`](https://easystats.github.io/insight/reference/clean_names.md)
  : Get clean names of model terms
- [`clean_parameters()`](https://easystats.github.io/insight/reference/clean_parameters.md)
  : Get clean names of model parameters
- [`display()`](https://easystats.github.io/insight/reference/display.md)
  [`print_md()`](https://easystats.github.io/insight/reference/display.md)
  [`print_html()`](https://easystats.github.io/insight/reference/display.md)
  : Generic export of data frames into formatted tables
- [`download_model()`](https://easystats.github.io/insight/reference/download_model.md)
  : Download circus models
- [`ellipsis_info()`](https://easystats.github.io/insight/reference/ellipsis_info.md)
  : Gather information about objects in ellipsis (dot dot dot)
- [`export_table()`](https://easystats.github.io/insight/reference/export_table.md)
  : Data frame and Tables Pretty Formatting
- [`null_model()`](https://easystats.github.io/insight/reference/null_model.md)
  : Compute intercept-only model for regression models
- [`print_parameters()`](https://easystats.github.io/insight/reference/print_parameters.md)
  : Prepare summary statistics of model parameters for printing
- [`standardize_column_order()`](https://easystats.github.io/insight/reference/standardize_column_order.md)
  : Standardize column order
- [`standardize_names()`](https://easystats.github.io/insight/reference/standardize_names.md)
  : Standardize column names
- [`easystats_columns()`](https://easystats.github.io/insight/reference/easystats_columns.md)
  [`broom_columns()`](https://easystats.github.io/insight/reference/easystats_columns.md)
  : Easystats columns
- [`is_empty_object()`](https://easystats.github.io/insight/reference/is_empty_object.md)
  : Check if object is empty
- [`object_has_names()`](https://easystats.github.io/insight/reference/object_has_names.md)
  [`object_has_rownames()`](https://easystats.github.io/insight/reference/object_has_names.md)
  : Check names and rownames
- [`compact_character()`](https://easystats.github.io/insight/reference/compact_character.md)
  : Remove empty strings from character
- [`compact_list()`](https://easystats.github.io/insight/reference/compact_list.md)
  : Remove empty elements from lists
- [`check_if_installed()`](https://easystats.github.io/insight/reference/check_if_installed.md)
  : Checking if needed package is installed
- [`validate_argument()`](https://easystats.github.io/insight/reference/validate_argument.md)
  : Validate arguments against a given set of options
- [`trim_ws()`](https://easystats.github.io/insight/reference/trim_ws.md)
  [`n_unique()`](https://easystats.github.io/insight/reference/trim_ws.md)
  [`safe_deparse()`](https://easystats.github.io/insight/reference/trim_ws.md)
  [`safe_deparse_symbol()`](https://easystats.github.io/insight/reference/trim_ws.md)
  [`has_single_value()`](https://easystats.github.io/insight/reference/trim_ws.md)
  : Small helper functions

## Example Datasets

Used in examples or for testing

- [`efc_insight`](https://easystats.github.io/insight/reference/efc_insight.md)
  : Sample dataset from the EFC Survey
- [`fish`](https://easystats.github.io/insight/reference/fish.md) :
  Sample data set for count models

## Documentation of Specific Class Objects

- [`find_parameters(`*`<BGGM>`*`)`](https://easystats.github.io/insight/reference/find_parameters.BGGM.md)
  [`find_parameters(`*`<brmsfit>`*`)`](https://easystats.github.io/insight/reference/find_parameters.BGGM.md)
  : Find names of model parameters from Bayesian models

- [`find_parameters(`*`<emmGrid>`*`)`](https://easystats.github.io/insight/reference/find_parameters.emmGrid.md)
  : Find model parameters from estimated marginal means objects

- [`find_parameters(`*`<gamlss>`*`)`](https://easystats.github.io/insight/reference/find_parameters.gamlss.md)
  [`find_parameters(`*`<gam>`*`)`](https://easystats.github.io/insight/reference/find_parameters.gamlss.md)
  : Find names of model parameters from generalized additive models

- [`find_parameters(`*`<betamfx>`*`)`](https://easystats.github.io/insight/reference/find_parameters.betamfx.md)
  : Find names of model parameters from marginal effects models

- [`find_parameters(`*`<glmmTMB>`*`)`](https://easystats.github.io/insight/reference/find_parameters.glmmTMB.md)
  : Find names of model parameters from mixed models

- [`find_parameters(`*`<zeroinfl>`*`)`](https://easystats.github.io/insight/reference/find_parameters.zeroinfl.md)
  : Find names of model parameters from zero-inflated models

- [`find_parameters(`*`<averaging>`*`)`](https://easystats.github.io/insight/reference/find_parameters.averaging.md)
  : Find model parameters from models with special components

- [`get_datagrid(`*`<emmGrid>`*`)`](https://easystats.github.io/insight/reference/get_datagrid.emmGrid.md)
  :

  Extract a reference grid from objects created by
  [emmeans](https://rvlenth.github.io/emmeans/) and
  [marginaleffects](https://marginaleffects.com/)

- [`get_parameters(`*`<BGGM>`*`)`](https://easystats.github.io/insight/reference/get_parameters.BGGM.md)
  [`get_parameters(`*`<BFBayesFactor>`*`)`](https://easystats.github.io/insight/reference/get_parameters.BGGM.md)
  [`get_parameters(`*`<brmsfit>`*`)`](https://easystats.github.io/insight/reference/get_parameters.BGGM.md)
  : Get model parameters from Bayesian models

- [`get_parameters(`*`<emmGrid>`*`)`](https://easystats.github.io/insight/reference/get_parameters.emmGrid.md)
  : Get model parameters from estimated marginal means objects

- [`get_parameters(`*`<gamm>`*`)`](https://easystats.github.io/insight/reference/get_parameters.gamm.md)
  : Get model parameters from generalized additive models

- [`get_parameters(`*`<betamfx>`*`)`](https://easystats.github.io/insight/reference/get_parameters.betamfx.md)
  : Get model parameters from marginal effects models

- [`get_parameters(`*`<glmmTMB>`*`)`](https://easystats.github.io/insight/reference/get_parameters.glmmTMB.md)
  : Get model parameters from mixed models

- [`get_parameters(`*`<zeroinfl>`*`)`](https://easystats.github.io/insight/reference/get_parameters.zeroinfl.md)
  : Get model parameters from zero-inflated and hurdle models

- [`get_parameters(`*`<betareg>`*`)`](https://easystats.github.io/insight/reference/get_parameters.betareg.md)
  : Get model parameters from models with special components

- [`get_parameters(`*`<htest>`*`)`](https://easystats.github.io/insight/reference/get_parameters.htest.md)
  : Get model parameters from htest-objects
