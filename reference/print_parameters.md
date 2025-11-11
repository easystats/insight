# Prepare summary statistics of model parameters for printing

This function takes a data frame, typically a data frame with
information on summaries of model parameters like
[`bayestestR::describe_posterior()`](https://easystats.github.io/bayestestR/reference/describe_posterior.html),
[`bayestestR::hdi()`](https://easystats.github.io/bayestestR/reference/hdi.html)
or
[`parameters::model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.html),
as input and splits this information into several parts, depending on
the model. See details below.

## Usage

``` r
print_parameters(
  x,
  ...,
  by = c("Effects", "Component", "Group", "Response"),
  format = "text",
  parameter_column = "Parameter",
  keep_parameter_column = TRUE,
  remove_empty_column = FALSE,
  titles = NULL,
  subtitles = NULL
)
```

## Arguments

- x:

  A fitted model, or a data frame returned by
  [`clean_parameters()`](https://easystats.github.io/insight/reference/clean_parameters.md).

- ...:

  One or more objects (data frames), which contain information about the
  model parameters and related statistics (like confidence intervals,
  HDI, ROPE, ...).

- by:

  `by` should be a character vector with one or more of the following
  elements: `"Effects"`, `"Component"`, `"Response"` and `"Group"`.
  These are the column names returned by
  [`clean_parameters()`](https://easystats.github.io/insight/reference/clean_parameters.md),
  which is used to extract the information from which the group or
  component model parameters belong. If `NULL`, the merged data frame is
  returned. Else, the data frame is split into a list, split by the
  values from those columns defined in `by`.

- format:

  Name of output-format, as string. If `NULL` (or `"text"`), assumed use
  for output is basic printing. If `"markdown"`, markdown-format is
  assumed. This only affects the style of title- and table-caption
  attributes, which are used in
  [`export_table()`](https://easystats.github.io/insight/reference/export_table.md).

- parameter_column:

  String, name of the column that contains the parameter names. Usually,
  for data frames returned by functions the easystats-packages, this
  will be `"Parameter"`.

- keep_parameter_column:

  Logical, if `TRUE`, the data frames in the returned list have both a
  `"Cleaned_Parameter"` and `"Parameter"` column. If `FALSE`, the
  (unformatted) `"Parameter"` is removed, and the column with cleaned
  parameter names (`"Cleaned_Parameter"`) is renamed into `"Parameter"`.

- remove_empty_column:

  Logical, if `TRUE`, columns with completely empty character values
  will be removed.

- titles, subtitles:

  By default, the names of the model components (like fixed or random
  effects, count or zero-inflated model part) are added as attributes
  `"table_title"` and `"table_subtitle"` to each list element returned
  by `print_parameters()`. These attributes are then extracted and used
  as table (sub) titles in
  [`export_table()`](https://easystats.github.io/insight/reference/export_table.md).
  Use `titles` and `subtitles` to override the default attribute values
  for `"table_title"` and `"table_subtitle"`. `titles` and `subtitles`
  may be any length from 1 to same length as returned list elements. If
  `titles` and `subtitles` are shorter than existing elements, only the
  first default attributes are overwritten.

## Value

A data frame or a list of data frames (if `by` is not `NULL`). If a list
is returned, the element names reflect the model components where the
extracted information in the data frames belong to, e.g.
`random.zero_inflated.Intercept: persons`. This is the data frame that
contains the parameters for the random effects from group-level
"persons" from the zero-inflated model component.

## Details

This function prepares data frames that contain information about model
parameters for clear printing.

First, `x` is required, which should either be a model object or a
prepared data frame as returned by
[`clean_parameters()`](https://easystats.github.io/insight/reference/clean_parameters.md).
If `x` is a model,
[`clean_parameters()`](https://easystats.github.io/insight/reference/clean_parameters.md)
is called on that model object to get information with which model
components the parameters are associated.

Then, `...` take one or more data frames that also contain information
about parameters from the same model, but also have additional
information provided by other methods. For instance, a data frame in
`...` might be the result of, for instance,
[`bayestestR::describe_posterior()`](https://easystats.github.io/bayestestR/reference/describe_posterior.html),
or
[`parameters::model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.html),
where we have a) a `Parameter` column and b) columns with other
parameter values (like CI, HDI, test statistic, etc.).

Now we have a data frame with model parameters and information about the
association to the different model components, a data frame with model
parameters, and some summary statistics. `print_parameters()` then
merges these data frames, so the parameters or statistics of interest
are also associated with the different model components. The data frame
is split into a list, so for a clear printing. Users can loop over this
list and print each component for a better overview. Further, parameter
names are "cleaned", if necessary, also for a cleaner print. See also
'Examples'.

## Examples

``` r
# \donttest{
library(bayestestR)
model <- download_model("brms_zi_2")
if (!is.null(model)) {
  x <- hdi(model, effects = "all", component = "all")

  # hdi() returns a data frame; here we use only the
  # information on parameter names and HDI values
  tmp <- as.data.frame(x)[, 1:4]
  tmp

  # Based on the "by" argument, we get a list of data frames that
  # is split into several parts that reflect the model components.
  print_parameters(model, tmp)

  # This is the standard print()-method for "bayestestR::hdi"-objects.
  # For printing methods, it is easy to print complex summary statistics
  # in a clean way to the console by splitting the information into
  # different model components.
  x
}
#> Highest Density Interval 
#> 
#> Parameter   |        95% HDI
#> ----------------------------
#> (Intercept) | [-1.58, -0.19]
#> persons     | [ 0.62,  1.07]
#> child       | [-1.32, -0.96]
#> camper      | [ 0.55,  0.93]
#> 
#> # Fixed effects (zero-inflated) 
#> 
#> Parameter   |        95% HDI
#> ----------------------------
#> (Intercept) | [-2.14,  0.82]
#> child       | [ 1.24,  2.56]
#> camper      | [-1.56, -0.14]
#> 
#> # Random effects (conditional) (SD/Cor: persons)
#> 
#> Parameter   |        95% HDI
#> ----------------------------
#> (Intercept) | [ 0.00,  0.54]
#> 
#> # Random effects (zero-inflated) (SD/Cor: persons)
#> 
#> Parameter   |        95% HDI
#> ----------------------------
#> (Intercept) | [ 0.37,  3.07]
# }
```
