# Color-formatting for data columns based on condition

Convenient function that formats columns in data frames with color
codes, where the color is chosen based on certain conditions. Columns
are then printed in color in the console.

## Usage

``` r
color_if(
  x,
  columns,
  predicate = `>`,
  value = 0,
  color_if = "green",
  color_else = "red",
  digits = 2
)

colour_if(
  x,
  columns,
  predicate = `>`,
  value = 0,
  colour_if = "green",
  colour_else = "red",
  digits = 2
)
```

## Arguments

- x:

  A data frame

- columns:

  Character vector with column names of `x` that should be formatted.

- predicate:

  A function that takes `columns` and `value` as input and which should
  return `TRUE` or `FALSE`, based on if the condition (in comparison
  with `value`) is met.

- value:

  The comparator. May be used in conjunction with `predicate` to quickly
  set up a function which compares elements in `colums` to `value`. May
  be ignored when `predicate` is a function that internally computes
  other comparisons. See 'Examples'.

- color_if, colour_if:

  Character vector, indicating the color code used to format values in
  `x` that meet the condition of `predicate` and `value`. May be one of
  `"red"`, `"yellow"`, `"green"`, `"blue"`, `"violet"`, `"cyan"` or
  `"grey"`. Formatting is also possible with `"bold"` or `"italic"`.

- color_else, colour_else:

  See `color_if`, but only for conditions that are *not* met.

- digits:

  Digits for rounded values.

## Value

`x`, where columns matched by `predicate` are wrapped into color codes.

## Details

The predicate-function simply works like this:
`which(predicate(x[, columns], value))`

## Examples

``` r
# all values in Sepal.Length larger than 5 in green, all remaining in red
x <- color_if(iris[1:10, ], columns = "Sepal.Length", predicate = `>`, value = 5)
x
#>                    Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#> 1  \033[32m        5.10\033[39m         3.5          1.4         0.2  setosa
#> 2  \033[31m        4.90\033[39m         3.0          1.4         0.2  setosa
#> 3  \033[31m        4.70\033[39m         3.2          1.3         0.2  setosa
#> 4  \033[31m        4.60\033[39m         3.1          1.5         0.2  setosa
#> 5  \033[31m        5.00\033[39m         3.6          1.4         0.2  setosa
#> 6  \033[32m        5.40\033[39m         3.9          1.7         0.4  setosa
#> 7  \033[31m        4.60\033[39m         3.4          1.4         0.3  setosa
#> 8  \033[31m        5.00\033[39m         3.4          1.5         0.2  setosa
#> 9  \033[31m        4.40\033[39m         2.9          1.4         0.2  setosa
#> 10 \033[31m        4.90\033[39m         3.1          1.5         0.1  setosa
cat(x$Sepal.Length)
#>         5.10         4.90         4.70         4.60         5.00         5.40         4.60         5.00         4.40         4.90

# all levels "setosa" in Species in green, all remaining in red
x <- color_if(iris, columns = "Species", predicate = `==`, value = "setosa")
cat(x$Species)
#>     setosa     setosa     setosa     setosa     setosa     setosa     setosa     setosa     setosa     setosa     setosa     setosa     setosa     setosa     setosa     setosa     setosa     setosa     setosa     setosa     setosa     setosa     setosa     setosa     setosa     setosa     setosa     setosa     setosa     setosa     setosa     setosa     setosa     setosa     setosa     setosa     setosa     setosa     setosa     setosa     setosa     setosa     setosa     setosa     setosa     setosa     setosa     setosa     setosa     setosa versicolor versicolor versicolor versicolor versicolor versicolor versicolor versicolor versicolor versicolor versicolor versicolor versicolor versicolor versicolor versicolor versicolor versicolor versicolor versicolor versicolor versicolor versicolor versicolor versicolor versicolor versicolor versicolor versicolor versicolor versicolor versicolor versicolor versicolor versicolor versicolor versicolor versicolor versicolor versicolor versicolor versicolor versicolor versicolor versicolor versicolor versicolor versicolor versicolor versicolor  virginica  virginica  virginica  virginica  virginica  virginica  virginica  virginica  virginica  virginica  virginica  virginica  virginica  virginica  virginica  virginica  virginica  virginica  virginica  virginica  virginica  virginica  virginica  virginica  virginica  virginica  virginica  virginica  virginica  virginica  virginica  virginica  virginica  virginica  virginica  virginica  virginica  virginica  virginica  virginica  virginica  virginica  virginica  virginica  virginica  virginica  virginica  virginica  virginica  virginica

# own function, argument "value" not needed here
p <- function(x, y) {
  x >= 4.9 & x <= 5.1
}
# all values in Sepal.Length between 4.9 and 5.1 in green, all remaining in red
x <- color_if(iris[1:10, ], columns = "Sepal.Length", predicate = p)
cat(x$Sepal.Length)
#>         5.10         4.90         4.70         4.60         5.00         5.40         4.60         5.00         4.40         4.90
```
