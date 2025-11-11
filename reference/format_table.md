# Parameter table formatting

This functions takes a data frame (usually with model parameters) as
input and formats certain columns into a more readable layout (like
collapsing separate columns for lower and upper confidence interval
values). Furthermore, column names are formatted as well. Note that
`format_table()` converts all columns into character vectors!

## Usage

``` r
format_table(
  x,
  pretty_names = TRUE,
  stars = FALSE,
  stars_only = FALSE,
  digits = 2,
  ci_width = "auto",
  ci_brackets = TRUE,
  ci_digits = digits,
  p_digits = 3,
  rope_digits = digits,
  ic_digits = 1,
  zap_small = FALSE,
  preserve_attributes = FALSE,
  exact = TRUE,
  use_symbols = getOption("insight_use_symbols", FALSE),
  select = NULL,
  verbose = TRUE,
  ...
)
```

## Arguments

- x:

  A data frame of model's parameters, as returned by various functions
  of the **easystats**-packages. May also be a result from
  [`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html).

- pretty_names:

  Return "pretty" (i.e. more human readable) parameter names.

- stars:

  If `TRUE`, add significance stars (e.g., `p < .001***`). Can also be a
  character vector, naming the columns that should include stars for
  significant values. This is especially useful for Bayesian models,
  where we might have multiple columns with significant values, e.g.
  `BF` for the Bayes factor or `pd` for the probability of direction. In
  such cases, use `stars = c("pd", "BF")` to add stars to both columns,
  or `stars = "BF"` to only add stars to the Bayes factor and exclude
  the `pd` column. Currently, following columns are recognized: `"BF"`,
  `"pd"` and `"p"`.

- stars_only:

  If `TRUE`, return significant stars only (and no p-values).

- digits, ci_digits, p_digits, rope_digits, ic_digits:

  Number of digits for rounding or significant figures. May also be
  `"signif"` to return significant figures or `"scientific"` to return
  scientific notation. Control the number of digits by adding the value
  as suffix, e.g. `digits = "scientific4"` to have scientific notation
  with 4 decimal places, or `digits = "signif5"` for 5 significant
  figures (see also [`signif()`](https://rdrr.io/r/base/Round.html)).

- ci_width:

  Minimum width of the returned string for confidence intervals. If not
  `NULL` and width is larger than the string's length, leading
  whitespaces are added to the string. If `width="auto"`, width will be
  set to the length of the longest string.

- ci_brackets:

  Logical, if `TRUE` (default), CI-values are encompassed in square
  brackets (else in parentheses).

- zap_small:

  Logical, if `TRUE`, small values are rounded after `digits` decimal
  places. If `FALSE`, values with more decimal places than `digits` are
  printed in scientific notation.

- preserve_attributes:

  Logical, if `TRUE`, preserves all attributes from the input data
  frame.

- exact:

  Formatting for Bayes factor columns, in case the provided data frame
  contains such a column (i.e. columns named `"BF"` or `"log_BF"`). For
  `exact = TRUE`, very large or very small values are then either
  reported with a scientific format (e.g., 4.24e5), else as truncated
  values (as "\> 1000" and "\< 1/1000").

- use_symbols:

  Logical, if `TRUE`, column names that refer to particular effectsizes
  (like Phi, Omega or Epsilon) include the related unicode-character
  instead of the written name. This only works on Windows for R \>= 4.2,
  and on OS X or Linux for R \>= 4.0. It is possible to define a global
  option for this setting, see 'Note'.

- select:

  Determines which columns are printed and the table layout. There are
  two options for this argument:

  - **A string expression with layout pattern**

    `select` is a string with "tokens" enclosed in braces. These tokens
    will be replaced by their associated columns, where the selected
    columns will be collapsed into one column. Following tokens are
    replaced by the related coefficients or statistics: `{estimate}`,
    `{se}`, `{ci}` (or `{ci_low}` and `{ci_high}`), `{p}`, `{pd}` and
    `{stars}`. The token `{ci}` will be replaced by
    `{ci_low}, {ci_high}`. Example:
    `select = "{estimate}{stars} ({ci})"`

    It is possible to create multiple columns as well. A `|` separates
    values into new cells/columns. Example:
    `select = "{estimate} ({ci})|{p}"`.

  - **A string indicating a pre-defined layout**

    `select` can be one of the following string values, to create one of
    the following pre-defined column layouts:

    - `"minimal"`: Estimates, confidence intervals and numeric p-values,
      in two columns. This is equivalent to
      `select = "{estimate} ({ci})|{p}"`.

    - `"short"`: Estimate, standard errors and numeric p-values, in two
      columns. This is equivalent to `select = "{estimate} ({se})|{p}"`.

    - `"ci"`: Estimates and confidence intervals, no asterisks for
      p-values. This is equivalent to `select = "{estimate} ({ci})"`.

    - `"se"`: Estimates and standard errors, no asterisks for p-values.
      This is equivalent to `select = "{estimate} ({se})"`.

    - `"ci_p"`: Estimates, confidence intervals and asterisks for
      p-values. This is equivalent to
      `select = "{estimate}{stars} ({ci})"`.

    - `"se_p"`: Estimates, standard errors and asterisks for p-values.
      This is equivalent to `select = "{estimate}{stars} ({se})"`..

  Using `select` to define columns will re-order columns and remove all
  columns related to uncertainty (standard errors, confidence
  intervals), test statistics, and p-values (and similar, like `pd` or
  `BF` for Bayesian models), because these are assumed to be included or
  intentionally excluded when using `select`. The new column order will
  be: Parameter columns first, followed by the "glue" columns, followed
  by all remaining columns. If further columns should also be placed
  first, add those as `focal_terms` attributes to `x`. I.e., following
  columns are considers as "parameter columns" and placed first:
  `c(easystats_columns("parameter"), attributes(x)$focal_terms)`.

  **Note:** glue-like syntax is still experimental in the case of more
  complex models (like mixed models) and may not return expected
  results.

- verbose:

  Toggle messages and warnings.

- ...:

  Arguments passed to or from other methods.

## Value

A data frame. Note that `format_table()` converts all columns into
character vectors!

## Note

`options(insight_use_symbols = TRUE)` overrides the `use_symbols`
argument and always displays symbols, if possible.

## See also

Vignettes [Formatting, printing and exporting
tables](https://easystats.github.io/insight/articles/display.html) and
[Formatting model
parameters](https://easystats.github.io/parameters/articles/model_parameters_formatting.html).

## Examples

``` r
format_table(head(iris), digits = 1)
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#> 1          5.1         3.5          1.4         0.2  setosa
#> 2          4.9         3.0          1.4         0.2  setosa
#> 3          4.7         3.2          1.3         0.2  setosa
#> 4          4.6         3.1          1.5         0.2  setosa
#> 5          5.0         3.6          1.4         0.2  setosa
#> 6          5.4         3.9          1.7         0.4  setosa

m <- lm(Sepal.Length ~ Species * Sepal.Width, data = iris)
x <- parameters::model_parameters(m)
as.data.frame(format_table(x))
#>                            Parameter Coefficient   SE        95% CI t(144)
#> 1                        (Intercept)        2.64 0.57 [ 1.51, 3.77]   4.62
#> 2               Species [versicolor]        0.90 0.80 [-0.68, 2.48]   1.13
#> 3                Species [virginica]        1.27 0.82 [-0.35, 2.88]   1.55
#> 4                        Sepal Width        0.69 0.17 [ 0.36, 1.02]   4.17
#> 5 Species [versicolor] × Sepal Width        0.17 0.26 [-0.34, 0.69]   0.67
#> 6  Species [virginica] × Sepal Width        0.21 0.26 [-0.29, 0.72]   0.83
#>        p
#> 1 < .001
#> 2 0.261 
#> 3 0.123 
#> 4 < .001
#> 5 0.503 
#> 6 0.411 
as.data.frame(format_table(x, p_digits = "scientific"))
#>                            Parameter Coefficient   SE        95% CI t(144)
#> 1                        (Intercept)        2.64 0.57 [ 1.51, 3.77]   4.62
#> 2               Species [versicolor]        0.90 0.80 [-0.68, 2.48]   1.13
#> 3                Species [virginica]        1.27 0.82 [-0.35, 2.88]   1.55
#> 4                        Sepal Width        0.69 0.17 [ 0.36, 1.02]   4.17
#> 5 Species [versicolor] × Sepal Width        0.17 0.26 [-0.34, 0.69]   0.67
#> 6  Species [virginica] × Sepal Width        0.21 0.26 [-0.29, 0.72]   0.83
#>             p
#> 1 8.52612e-06
#> 2 2.61332e-01
#> 3 1.22515e-01
#> 4 5.31104e-05
#> 5 5.02805e-01
#> 6 4.10634e-01
# "glue" columns
as.data.frame(format_table(x, select = "minimal"))
#>                            Parameter   Coefficient (CI)      p
#> 1                        (Intercept) 2.64 ( 1.51, 3.77) <0.001
#> 2               Species [versicolor] 0.90 (-0.68, 2.48)  0.261
#> 3                Species [virginica] 1.27 (-0.35, 2.88)  0.123
#> 4                        Sepal Width 0.69 ( 0.36, 1.02) <0.001
#> 5 Species [versicolor] × Sepal Width 0.17 (-0.34, 0.69)  0.503
#> 6  Species [virginica] × Sepal Width 0.21 (-0.29, 0.72)  0.411
as.data.frame(format_table(x, select = "{estimate}{stars}|{p}"))
#>                            Parameter Coefficient      p
#> 1                        (Intercept)     2.64*** <0.001
#> 2               Species [versicolor]        0.90  0.261
#> 3                Species [virginica]        1.27  0.123
#> 4                        Sepal Width     0.69*** <0.001
#> 5 Species [versicolor] × Sepal Width        0.17  0.503
#> 6  Species [virginica] × Sepal Width        0.21  0.411

# \donttest{
model <- rstanarm::stan_glm(
  Sepal.Length ~ Species,
  data = iris,
  refresh = 0,
  seed = 123
)
x <- parameters::model_parameters(model, ci = c(0.69, 0.89, 0.95))
as.data.frame(format_table(x))
#>           Parameter Median       69% CI       89% CI       95% CI   pd  Rhat
#> 1       (Intercept)   5.01 [4.93, 5.08] [4.86, 5.15] [4.88, 5.12] 100% 1.000
#> 2 Speciesversicolor   0.93 [0.82, 1.04] [0.73, 1.14] [0.76, 1.11] 100% 1.000
#> 3  Speciesvirginica   1.58 [1.48, 1.69] [1.39, 1.79] [1.42, 1.75] 100% 1.000
#>    ESS                 Prior
#> 1 3279 Normal (5.84 +- 2.07)
#> 2 3458 Normal (0.00 +- 4.38)
#> 3 3201 Normal (0.00 +- 4.38)
# }
```
