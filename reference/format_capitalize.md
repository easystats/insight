# Capitalizes the first letter in a string

This function converts the first letter in a string into upper case.

## Usage

``` r
format_capitalize(x, verbose = TRUE)
```

## Arguments

- x:

  A character vector or a factor. The latter is coerced to character.
  All other objects are returned unchanged.

- verbose:

  Toggle warnings.

## Value

`x`, with first letter capitalized.

## Examples

``` r
format_capitalize("hello")
#> [1] "Hello"
format_capitalize(c("hello", "world"))
#> [1] "Hello" "World"
unique(format_capitalize(iris$Species))
#> [1] "Setosa"     "Versicolor" "Virginica" 
```
