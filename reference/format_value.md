# Numeric Values Formatting

`format_value()` converts numeric values into formatted string values,
where formatting can be something like rounding digits, scientific
notation etc. `format_percent()` is a short-cut for
`format_value(as_percent = TRUE)`.

## Usage

``` r
format_value(x, ...)

# S3 method for class 'data.frame'
format_value(
  x,
  digits = 2,
  protect_integers = FALSE,
  missing = "",
  width = NULL,
  as_percent = FALSE,
  zap_small = FALSE,
  lead_zero = TRUE,
  style_positive = "none",
  style_negative = "hyphen",
  decimal_point = getOption("OutDec"),
  big_mark = NULL,
  ...
)

# S3 method for class 'numeric'
format_value(
  x,
  digits = 2,
  protect_integers = FALSE,
  missing = "",
  width = NULL,
  as_percent = FALSE,
  zap_small = FALSE,
  lead_zero = TRUE,
  style_positive = "none",
  style_negative = "hyphen",
  decimal_point = getOption("OutDec"),
  big_mark = NULL,
  ...
)

format_percent(x, ...)
```

## Arguments

- x:

  Numeric value.

- ...:

  Arguments passed to or from other methods.

- digits:

  Number of digits for rounding or significant figures. May also be
  `"signif"` to return significant figures or `"scientific"` to return
  scientific notation. Control the number of digits by adding the value
  as suffix, e.g. `digits = "scientific4"` to have scientific notation
  with 4 decimal places, or `digits = "signif5"` for 5 significant
  figures (see also [`signif()`](https://rdrr.io/r/base/Round.html)).

- protect_integers:

  Should integers be kept as integers (i.e., without decimals)?

- missing:

  Value by which `NA` values are replaced. By default, an empty string
  (i.e. `""`) is returned for `NA`.

- width:

  Minimum width of the returned string. If not `NULL` and `width` is
  larger than the string's length, leading whitespaces are added to the
  string.

- as_percent:

  Logical, if `TRUE`, value is formatted as percentage value.

- zap_small:

  Logical, if `TRUE`, small values are rounded after `digits` decimal
  places. If `FALSE`, values with more decimal places than `digits` are
  printed in scientific notation.

- lead_zero:

  Logical, if `TRUE` (default), includes leading zeros, else leading
  zeros are dropped.

- style_positive:

  A string that determines the style of positive numbers. May be
  `"none"` (default), `"plus"` to add a plus-sign or `"space"` to
  precede the string by a Unicode "figure space", i.e., a space equally
  as wide as a number or `+`.

- style_negative:

  A string that determines the style of negative numbers. May be
  `"hyphen"` (default), `"minus"` for a proper Unicode minus symbol or
  `"parens"` to wrap the number in parentheses.

- decimal_point:

  Character string containing a single character that is used as decimal
  point in output conversions.

- big_mark:

  Character used as thousands separator. If `NULL` (default), no
  thousands separator is used. Use `","` for comma separator or `" "`
  for space separator.

## Value

A formatted string.

## Examples

``` r
format_value(1.20)
#> [1] "1.20"
format_value(1.2)
#> [1] "1.20"
format_value(1.2012313)
#> [1] "1.20"
format_value(c(0.0045, 234, -23))
#> [1] "4.50e-03" "234.00"   "-23.00"  
format_value(c(0.0045, 0.12, 0.34))
#> [1] "4.50e-03" "0.12"     "0.34"    
format_value(c(0.0045, 0.12, 0.34), as_percent = TRUE)
#> [1] "0.45%"  "12.00%" "34.00%"
format_value(c(0.0045, 0.12, 0.34), digits = "scientific")
#> [1] "4.50000e-03" "1.20000e-01" "3.40000e-01"
format_value(c(0.0045, 0.12, 0.34), digits = "scientific2")
#> [1] "4.50e-03" "1.20e-01" "3.40e-01"
format_value(c(0.045, 0.12, 0.34), lead_zero = FALSE)
#> [1] ".04" ".12" ".34"
format_value(c(0.0045, 0.12, 0.34), decimal_point = ",")
#> [1] "4,50e-03" "0,12"     "0,34"    
format_value(c(1234567.89, 1234.56), big_mark = ",")
#> [1] "1,234,567.89" "1,234.56"    
format_value(c(1234567.89, 1234.56), big_mark = " ")
#> [1] "1 234 567.89" "1 234.56"    

# default
format_value(c(0.0045, 0.123, 0.345))
#> [1] "4.50e-03" "0.12"     "0.34"    
# significant figures
format_value(c(0.0045, 0.123, 0.345), digits = "signif")
#> [1] "0.0045" "0.123"  "0.345" 

format_value(as.factor(c("A", "B", "A")))
#> [1] A B A
#> Levels: A B
format_value(iris$Species)
#>   [1] setosa     setosa     setosa     setosa     setosa     setosa    
#>   [7] setosa     setosa     setosa     setosa     setosa     setosa    
#>  [13] setosa     setosa     setosa     setosa     setosa     setosa    
#>  [19] setosa     setosa     setosa     setosa     setosa     setosa    
#>  [25] setosa     setosa     setosa     setosa     setosa     setosa    
#>  [31] setosa     setosa     setosa     setosa     setosa     setosa    
#>  [37] setosa     setosa     setosa     setosa     setosa     setosa    
#>  [43] setosa     setosa     setosa     setosa     setosa     setosa    
#>  [49] setosa     setosa     versicolor versicolor versicolor versicolor
#>  [55] versicolor versicolor versicolor versicolor versicolor versicolor
#>  [61] versicolor versicolor versicolor versicolor versicolor versicolor
#>  [67] versicolor versicolor versicolor versicolor versicolor versicolor
#>  [73] versicolor versicolor versicolor versicolor versicolor versicolor
#>  [79] versicolor versicolor versicolor versicolor versicolor versicolor
#>  [85] versicolor versicolor versicolor versicolor versicolor versicolor
#>  [91] versicolor versicolor versicolor versicolor versicolor versicolor
#>  [97] versicolor versicolor versicolor versicolor virginica  virginica 
#> [103] virginica  virginica  virginica  virginica  virginica  virginica 
#> [109] virginica  virginica  virginica  virginica  virginica  virginica 
#> [115] virginica  virginica  virginica  virginica  virginica  virginica 
#> [121] virginica  virginica  virginica  virginica  virginica  virginica 
#> [127] virginica  virginica  virginica  virginica  virginica  virginica 
#> [133] virginica  virginica  virginica  virginica  virginica  virginica 
#> [139] virginica  virginica  virginica  virginica  virginica  virginica 
#> [145] virginica  virginica  virginica  virginica  virginica  virginica 
#> Levels: setosa versicolor virginica

format_value(3)
#> [1] "3.00"
format_value(3, protect_integers = TRUE)
#> [1] "3"

format_value(head(iris))
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#> 1         5.10        3.50         1.40        0.20  setosa
#> 2         4.90        3.00         1.40        0.20  setosa
#> 3         4.70        3.20         1.30        0.20  setosa
#> 4         4.60        3.10         1.50        0.20  setosa
#> 5         5.00        3.60         1.40        0.20  setosa
#> 6         5.40        3.90         1.70        0.40  setosa
```
