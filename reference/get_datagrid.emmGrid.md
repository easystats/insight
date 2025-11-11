# Extract a reference grid from objects created by `{emmeans}` and `{marginaleffects}`

Extract a reference grid from objects created by `{emmeans}` and
`{marginaleffects}`

## Usage

``` r
# S3 method for class 'emmGrid'
get_datagrid(x, ...)
```

## Arguments

- x:

  An object created by a function such as
  [`emmeans::emmeans()`](https://rvlenth.github.io/emmeans/reference/emmeans.html),
  [`marginaleffects::slopes()`](https://marginaleffects.com/man/r/slopes.html),
  etc.

- ...:

  Currently not used

## Value

A `data.frame` with key columns that identify the rows in `x`.

## Details

Note that for `{emmeans}` inputs the results is a proper grid (all
combinations of values are represented), except when a nesting structure
is detected. Additionally, when the input is an `emm_list` object, the
function will [`rbind()`](https://rdrr.io/r/base/cbind.html) the
data-grids of all the elements in the input.

For `{marginaleffects}` inputs, the output may very well be a non-grid
result. See examples.

## Examples

``` r
data("mtcars")
mtcars$cyl <- factor(mtcars$cyl)

mod <- glm(am ~ cyl + hp + wt,
  family = binomial("logit"),
  data = mtcars
)

em1 <- emmeans::emmeans(mod, ~ cyl + hp, at = list(hp = c(100, 150)))
get_datagrid(em1)
#>   cyl  hp
#> 1   4 100
#> 2   6 100
#> 3   8 100
#> 4   4 150
#> 5   6 150
#> 6   8 150

contr1 <- emmeans::contrast(em1, method = "consec", by = "hp")
get_datagrid(contr1)
#>      contrast  hp
#> 1 cyl6 - cyl4 100
#> 2 cyl8 - cyl6 100
#> 3 cyl6 - cyl4 150
#> 4 cyl8 - cyl6 150

eml1 <- emmeans::emmeans(mod, pairwise ~ cyl | hp, at = list(hp = c(100, 150)))
get_datagrid(eml1) # not a "true" grid
#>     hp  cyl    contrast
#> 1  100    4        <NA>
#> 2  100    6        <NA>
#> 3  100    8        <NA>
#> 4  150    4        <NA>
#> 5  150    6        <NA>
#> 6  150    8        <NA>
#> 7  100 <NA> cyl4 - cyl6
#> 8  100 <NA> cyl4 - cyl8
#> 9  100 <NA> cyl6 - cyl8
#> 10 150 <NA> cyl4 - cyl6
#> 11 150 <NA> cyl4 - cyl8
#> 12 150 <NA> cyl6 - cyl8
mfx1 <- marginaleffects::slopes(mod, variables = "hp")
get_datagrid(mfx1) # not a "true" grid
#>    term contrast cyl  hp    wt
#> 1    hp    dY/dX   6 110 2.620
#> 2    hp    dY/dX   6 110 2.875
#> 3    hp    dY/dX   4  93 2.320
#> 4    hp    dY/dX   6 110 3.215
#> 5    hp    dY/dX   8 175 3.440
#> 6    hp    dY/dX   6 105 3.460
#> 7    hp    dY/dX   8 245 3.570
#> 8    hp    dY/dX   4  62 3.190
#> 9    hp    dY/dX   4  95 3.150
#> 10   hp    dY/dX   6 123 3.440
#> 11   hp    dY/dX   6 123 3.440
#> 12   hp    dY/dX   8 180 4.070
#> 13   hp    dY/dX   8 180 3.730
#> 14   hp    dY/dX   8 180 3.780
#> 15   hp    dY/dX   8 205 5.250
#> 16   hp    dY/dX   8 215 5.424
#> 17   hp    dY/dX   8 230 5.345
#> 18   hp    dY/dX   4  66 2.200
#> 19   hp    dY/dX   4  52 1.615
#> 20   hp    dY/dX   4  65 1.835
#> 21   hp    dY/dX   4  97 2.465
#> 22   hp    dY/dX   8 150 3.520
#> 23   hp    dY/dX   8 150 3.435
#> 24   hp    dY/dX   8 245 3.840
#> 25   hp    dY/dX   8 175 3.845
#> 26   hp    dY/dX   4  66 1.935
#> 27   hp    dY/dX   4  91 2.140
#> 28   hp    dY/dX   4 113 1.513
#> 29   hp    dY/dX   8 264 3.170
#> 30   hp    dY/dX   6 175 2.770
#> 31   hp    dY/dX   8 335 3.570
#> 32   hp    dY/dX   4 109 2.780

mfx2 <- marginaleffects::slopes(mod, variables = c("hp", "wt"), by = "am")
get_datagrid(mfx2)
#>   term contrast am
#> 1   hp    dY/dX  0
#> 2   hp    dY/dX  1
#> 3   wt    dY/dX  0
#> 4   wt    dY/dX  1

contr2 <- marginaleffects::avg_comparisons(mod)
get_datagrid(contr2) # not a "true" grid
#>   term contrast
#> 1  cyl    6 - 4
#> 2  cyl    8 - 4
#> 3   hp       +1
#> 4   wt       +1
```
