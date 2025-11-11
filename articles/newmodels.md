# Adding support for new model classes

## Adding (almost) full support to the easystats ecosystem

To add support for models in the *easystats* ecosystem, especially for
the packages [*parameters*](https://easystats.github.io/parameters/), or
to support most / all features from *easystats*, following methods
should be added to the *insight* package:

- [`insight::find_formula()`](https://easystats.github.io/insight/reference/find_formula.md)
- [`insight::model_info()`](https://easystats.github.io/insight/reference/model_info.md)
- [`insight::get_parameters()`](https://easystats.github.io/insight/reference/get_parameters.md)
- [`insight::get_statistic()`](https://easystats.github.io/insight/reference/get_statistic.md)
- [`insight::find_statistic()`](https://easystats.github.io/insight/reference/find_statistic.md)
- [`insight::get_varcov()`](https://easystats.github.io/insight/reference/get_varcov.md)
- [`insight::get_df()`](https://easystats.github.io/insight/reference/get_df.md)

Nice to have (though some of the following functions could already work
if the above methods are implemented):

- [`insight::find_parameters()`](https://easystats.github.io/insight/reference/find_parameters.md)
- [`insight::get_modelmatrix()`](https://easystats.github.io/insight/reference/get_modelmatrix.md)
- [`insight::get_predicted()`](https://easystats.github.io/insight/reference/get_predicted.md)
- [`insight::link_inverse()`](https://easystats.github.io/insight/reference/link_inverse.md)
  and
  [`insight::link_function()`](https://easystats.github.io/insight/reference/link_function.md)
- [`insight::get_residuals()`](https://easystats.github.io/insight/reference/get_residuals.md)

## Adding basic support

If the purpose is just “tidy” output, it is enough to add following
methods to the [*parameters*](https://easystats.github.io/parameters/)
package:

- [`parameters::ci()`](https://easystats.github.io/bayestestR/reference/ci.html)
- [`parameters::p_value()`](https://easystats.github.io/parameters/reference/p_value.html)
- [`parameters::standard_error()`](https://easystats.github.io/parameters/reference/standard_error.html)
  **or**
  [`insight::get_varcov()`](https://easystats.github.io/insight/reference/get_varcov.md)

The *simplest* way is just adding a method for
[`parameters::model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.html),
however, than this model-class is not supported by *all* functions we
offer in the *easystats* ecosystem.

See also
<https://easystats.github.io/effectsize/articles/effectsize_API.html>
