# Small helper functions

Collection of small helper functions. `trim_ws()` is an efficient
function to trim leading and trailing whitespaces from character vectors
or strings. `n_unique()` returns the number of unique values in a
vector. `has_single_value()` is equivalent to `n_unique() == 1` but is
faster (note the different default for the `remove_na` argument).
`safe_deparse()` is comparable to
[`deparse1()`](https://rdrr.io/r/base/deparse.html), i.e. it can safely
deparse very long expressions into a single string.
`safe_deparse_symbol()` only deparses a substituted expressions when
possible, which can be much faster than `deparse(substitute())` for
those cases where
[`substitute()`](https://rdrr.io/r/base/substitute.html) returns no
valid object name.

## Usage

``` r
trim_ws(x, ...)

# S3 method for class 'data.frame'
trim_ws(x, character_only = TRUE, ...)

n_unique(x, ...)

# Default S3 method
n_unique(x, remove_na = TRUE, ...)

safe_deparse(x, ...)

safe_deparse_symbol(x)

has_single_value(x, remove_na = FALSE, ...)
```

## Arguments

- x:

  A (character) vector, or for some functions may also be a data frame.

- ...:

  Currently not used.

- character_only:

  Logical, if `TRUE` and `x` is a data frame or list, only processes
  character vectors.

- remove_na:

  Logical, if missing values should be removed from the input.

## Value

- `n_unique()`: For a vector, `n_unique` always returns an integer
  value, even if the input is `NULL` (the return value will be `0`
  then). For data frames or lists, `n_unique()` returns a named numeric
  vector, with the number of unique values for each element.

- `has_single_value()`: `TRUE` if `x` has only one unique value, `FALSE`
  otherwise.

- `trim_ws()`: A character vector, where trailing and leading white
  spaces are removed.

- `safe_deparse()`: A character string of the unevaluated expression or
  symbol.

- `safe_deparse_symbol()`: A character string of the unevaluated
  expression or symbol, if `x` was a symbol. If `x` is no symbol (i.e.
  if `is.name(x)` would return `FALSE`), `NULL` is returned.

## Examples

``` r
trim_ws("  no space!  ")
#> [1] "no space!"
n_unique(iris$Species)
#> [1] 3
has_single_value(c(1, 1, 2))
#> [1] FALSE

# safe_deparse_symbol() compared to deparse(substitute())
safe_deparse_symbol(as.name("test"))
#> [1] "test"
deparse(substitute(as.name("test")))
#> [1] "as.name(\"test\")"
```
