# Check names and rownames

`object_has_names()` checks if specified names are present in the given
object. `object_has_rownames()` checks if rownames are present in a
dataframe.

## Usage

``` r
object_has_names(x, names)

object_has_rownames(x)
```

## Arguments

- x:

  A named object (an atomic vector, a list, a dataframe, etc.).

- names:

  A single character or a vector of characters.

## Value

A logical or a vector of logicals.

## Examples

``` r

# check if specified names are present in the given object
object_has_names(mtcars, "am")
#> [1] TRUE
object_has_names(anscombe, c("x1", "z1", "y1"))
#> [1]  TRUE FALSE  TRUE
object_has_names(list("x" = 1, "y" = 2), c("x", "a"))
#> [1]  TRUE FALSE

# check if a dataframe has rownames
object_has_rownames(mtcars)
#> [1] TRUE
```
