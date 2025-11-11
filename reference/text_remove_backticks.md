# Remove backticks from a string

This function removes backticks from a string.

## Usage

``` r
text_remove_backticks(x, ...)

# S3 method for class 'data.frame'
text_remove_backticks(x, column = "Parameter", verbose = FALSE, ...)
```

## Arguments

- x:

  A character vector, a data frame or a matrix. If a matrix, backticks
  are removed from the column and row names, not from values of a
  character vector.

- ...:

  Currently not used.

- column:

  If `x` is a data frame, specify the column of character vectors, where
  backticks should be removed. If `NULL`, all character vectors are
  processed.

- verbose:

  Toggle warnings.

## Value

`x`, where all backticks are removed.

## Note

If `x` is a character vector or data frame, backticks are removed from
the elements of that character vector (or character vectors from the
data frame.) If `x` is a matrix, the behaviour slightly differs: in this
case, backticks are removed from the column and row names. The reason
for this behaviour is that this function mainly serves formatting
coefficient names. For [`vcov()`](https://rdrr.io/r/stats/vcov.html) (a
matrix), row and column names equal the coefficient names and therefore
are manipulated then.

## Examples

``` r
# example model
data(iris)
iris$`a m` <- iris$Species
iris$`Sepal Width` <- iris$Sepal.Width
model <- lm(`Sepal Width` ~ Petal.Length + `a m`, data = iris)

# remove backticks from string
names(coef(model))
#> [1] "(Intercept)"     "Petal.Length"    "`a m`versicolor" "`a m`virginica" 
text_remove_backticks(names(coef(model)))
#> [1] "(Intercept)"   "Petal.Length"  "a mversicolor" "a mvirginica" 

# remove backticks from character variable in a data frame
# column defaults to "Parameter".
d <- data.frame(
  Parameter = names(coef(model)),
  Estimate = unname(coef(model))
)
d
#>         Parameter  Estimate
#> 1     (Intercept)  2.991869
#> 2    Petal.Length  0.298311
#> 3 `a m`versicolor -1.492674
#> 4  `a m`virginica -1.674092
text_remove_backticks(d)
#>       Parameter  Estimate
#> 1   (Intercept)  2.991869
#> 2  Petal.Length  0.298311
#> 3 a mversicolor -1.492674
#> 4  a mvirginica -1.674092
```
