# insight 0.1.2

## New functions

* `find_variables()`, a kind of combination of `find_parameters()` and `find_formula()`. It returns the names of all variables "as is", i.e. transformations like `log()` or `as.factor()` are preserved (as in `find_parameters()`), however, it returns information for all model components including response variable (like `find_formula()`).

## New supported objects

* _insight_ now supports `aov()`-models with error terms (i.e. objects of class `aovlist()`).

## Changes to functions

* `get_random()` now returns only the data of the random effects terms from **lme**-objects.
* `clean_names()` now also removes the `sqrt()`-pattern.

## Bug fixes

* Fixed bug for random effect models, where random component only contained a `~1` as formula.

# insight 0.1.1

## General

* Initial release.