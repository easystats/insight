# Find model formula

Returns the formula(s) for the different parts of a model (like fixed or
random effects, zero-inflated component, ...). `formula_ok()` checks if
a model formula has valid syntax regarding writing `TRUE` instead of `T`
inside [`poly()`](https://rdrr.io/r/stats/poly.html) and that no data
names are used (i.e. no `data$variable`, but rather `variable`).

## Usage

``` r
find_formula(x, ...)

formula_ok(
  x,
  checks = "all",
  action = "warning",
  prefix_msg = NULL,
  verbose = TRUE,
  ...
)

# Default S3 method
find_formula(x, verbose = TRUE, ...)

# S3 method for class 'nestedLogit'
find_formula(x, dichotomies = FALSE, verbose = TRUE, ...)
```

## Arguments

- x:

  A fitted model.

- ...:

  Currently not used.

- checks:

  Indicates what kind of checks are conducted when checking the formula
  notation. Currently, four different formula specification that can
  result in unexpected behaviour of downstream-functions are checked.
  `checks` can be one or more of:

  - `"dollar"`: Check if formula contains data name with "\$", e.g.
    `mtcars$am`.

  - `"T"`: Check if formula contains poly-term with "raw=T", e.g.
    `poly(x, 2, raw=T)`. In this case,
    [`all.vars()`](https://rdrr.io/r/base/allnames.html) returns `T` as
    variable, which is not intended.

  - `"index"`: Check if formula contains indexed data frames as response
    variable (e.g., `df[, 5] ~ x`).

  - `"name"`: Check if syntactically invalid variable names were used
    and quoted in backticks.

  - `"all"`: Checks all of the above mentioned options.

- action:

  Should a message, warning or error be given for an invalid formula?
  Must be one of `"message"`, `"warning"` (default) or `"error"`.

- prefix_msg:

  Optional string that will be added to the warning/error message. This
  can be used to add additional information, e.g. about the specific
  function that was calling `formula_ok()` and failed.

- verbose:

  Toggle warnings.

- dichotomies:

  Logical, if model is a `nestedLogit` objects, returns the formulas for
  the dichotomies.

## Value

A list of formulas that describe the model. For simple models, only one
list-element, `conditional`, is returned. For more complex models, the
returned list may have following elements:

- `conditional`, the "fixed effects" part from the model (in the context
  of fixed-effects or instrumental variable regression, also called
  *regressors*) . One exception are `DirichletRegModel` models from
  **DirichletReg**, which has two or three components, depending on
  `model`.

- `random`, the "random effects" part from the model (or the `id` for
  gee-models and similar)

- `zero_inflated`, the "fixed effects" part from the zero-inflation
  component of the model. for models from *brms*, this component is
  named `zi`.

- `zero_inflated_random`, the "random effects" part from the
  zero-inflation component of the model; for models from *brms*, this
  component is named `zi_random`.

- `dispersion`, the dispersion formula

- `instruments`, for fixed-effects or instrumental variable regressions
  like
  [`ivreg::ivreg()`](https://zeileis.github.io/ivreg/reference/ivreg.html),
  [`lfe::felm()`](https://rdrr.io/pkg/lfe/man/felm.html) or
  [`plm::plm()`](https://rdrr.io/pkg/plm/man/plm.html), the instrumental
  variables

- `cluster`, for fixed-effects regressions like
  [`lfe::felm()`](https://rdrr.io/pkg/lfe/man/felm.html), the cluster
  specification

- `correlation`, for models with correlation-component like
  [`nlme::gls()`](https://rdrr.io/pkg/nlme/man/gls.html), the formula
  that describes the correlation structure

- `scale`, for distributional models such as
  [`mgcv::gaulss()`](https://rdrr.io/pkg/mgcv/man/gaulss.html) family
  fitted with [`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html),
  the formula that describes the scale parameter

- `slopes`, for fixed-effects individual-slope models like
  [`feisr::feis()`](https://rdrr.io/pkg/feisr/man/feis.html), the
  formula for the slope parameters

- `precision`, for `DirichletRegModel` models from **DirichletReg**,
  when parametrization (i.e. `model`) is `"alternative"`.

- `bidrange`, for models of class `oohbchoice` (from package
  **DCchoice**), which indicates the right-hand side of the bar (the
  bid-range).

For models from package **brms**, distributional parameters are also
included.

## Note

For models of class `lme` or `gls` the correlation-component is only
returned, when it is explicitly defined as named argument (`form`), e.g.
`corAR1(form = ~1 | Mare)`

## Examples

``` r
data(mtcars)
m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
find_formula(m)
#> $conditional
#> mpg ~ wt + cyl + vs
#> <environment: 0x55e245268b38>
#> 
#> attr(,"class")
#> [1] "insight_formula" "list"           

m <- lme4::lmer(Sepal.Length ~ Sepal.Width + (1 | Species), data = iris)
f <- find_formula(m)
f
#> $conditional
#> Sepal.Length ~ Sepal.Width
#> <environment: 0x55e245268b38>
#> 
#> $random
#> ~1 | Species
#> <environment: 0x55e250c4d7e0>
#> 
#> attr(,"class")
#> [1] "insight_formula" "list"           
format(f)
#> [1] "Sepal.Length ~ Sepal.Width + (~1 | Species)"
```
