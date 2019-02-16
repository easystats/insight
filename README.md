
# insight <img src='man/figures/logo.png' align="right" height="139" />

[![Build
Status](https://travis-ci.org/easystats/insight.svg?branch=master)](https://travis-ci.org/easystats/insight)
[![codecov](https://codecov.io/gh/easystats/insight/branch/master/graph/badge.svg)](https://codecov.io/gh/easystats/insight)
[![HitCount](http://hits.dwyl.io/easystats/insight.svg)](http://hits.dwyl.io/easystats/insight)
[![Documentation](https://img.shields.io/badge/documentation-insight-orange.svg?colorB=E91E63)](https://easystats.github.io/insight/)

***Gain insight into your models\!***

The goal of *insight* is to provide tools to help an **easy**,
**intuitive** and **consistent** accesss to information contained in
various models. Although there are generic functions to get information
and data from models, many modelling-functions from different packages
do not provide such methods to access these information. The *insight*
package aims at closing this gap by providing functions that work for
(almost) any model.

## Installation

Run the following:

``` r
install.packages("devtools")
devtools::install_github("easystats/insight")
```

``` r
library("insight")
```

## Functions

The syntax of `insight` mainly revolves around two types of functions.
One is to find the names of the *things* (`find_*`), and the second is
to actually get the *things* (`get_`). The *things* can be the
following:

  - [find\_formula()](https://easystats.github.io/insight/reference/find_formula.html)
  - [find\_terms()](https://easystats.github.io/insight/reference/find_terms.html)
  - [get\_data()](https://easystats.github.io/insight/reference/get_data.html)
  - [find\_/](https://easystats.github.io/insight/reference/find_parameters.html)
    [get\_parameters()](https://easystats.github.io/insight/reference/get_parameters.html)
  - [find\_/](https://easystats.github.io/insight/reference/find_predictors.html)
    [get\_predictors()](https://easystats.github.io/insight/reference/get_predictors.html)
  - [find\_/](https://easystats.github.io/insight/reference/find_random.html)
    [get\_random()](https://easystats.github.io/insight/reference/get_random.html)
  - [find\_/](https://easystats.github.io/insight/reference/find_response.html)
    [get\_response()](https://easystats.github.io/insight/reference/get_response.html)

On top of that, the
[`model_info()`](https://easystats.github.io/insight/reference/model_info.html)
function runs many checks to help you classify and understand the nature
of your model.

## List of Supported Models

  - base (*lm, glm, htest*)

  - lme4 (*lmer, glmer*)

  - rstanarm (*stanreg, stanmvreg*)

  - brms (*brmsfit*)

  - glmmTMB (*glmmTMB*)

  - MASS (*glmmPQL*)

  - GLMMadaptive (*MixMod*)

  - betareg (*betareg*)

  - truncreg (*truncreg*)

  - gam (*gam*)

  - AER

  - coxme

  - gee

  - gmnl

  - mgcv

  - nnet

  - nlme

  - ordinal

  - plm

  - pscl

  - survey

  - survival

  - **Didn’t find a model?** [Check this vignette
    out](https://easystats.github.io/insight/articles/support_new_models.html)
    to easily add support for a new model\!

## Credits

If this package helped you, please consider citing as follows:

  - Lüdecke, D. (2019). *insight: Easy Access to Model Information for
    Various Model Objects*. R package.
    <https://easystats.github.io/insight/>.
