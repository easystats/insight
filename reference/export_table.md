# Data frame and Tables Pretty Formatting

Function to export data frames into tables, which can be printed to the
console, or displayed in markdown or HTML format (and thereby, exported
to other formats like Word or PDF). The table width is automatically
adjusted to fit into the width of the display device (e.g., width of
console). Use the `table_width` argument to control this behaviour.

## Usage

``` r
export_table(
  x,
  sep = " | ",
  header = "-",
  cross = NULL,
  empty_line = NULL,
  digits = 2,
  protect_integers = TRUE,
  missing = "",
  width = NULL,
  format = NULL,
  title = NULL,
  caption = title,
  subtitle = NULL,
  footer = NULL,
  column_names = NULL,
  align = NULL,
  by = NULL,
  zap_small = FALSE,
  big_mark = NULL,
  table_width = "auto",
  remove_duplicates = FALSE,
  row_groups = NULL,
  column_groups = NULL,
  verbose = TRUE,
  ...
)
```

## Arguments

- x:

  A data frame. May also be a list of data frames, to export multiple
  data frames into multiple tables.

- sep:

  Column separator.

- header:

  Header separator. Can be `NULL`.

- cross:

  Character that is used where separator and header lines cross.

- empty_line:

  Separator used for empty lines. If `NULL`, line remains empty (i.e.
  filled with whitespaces).

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

  Refers to the width of columns (with numeric values). Can be either
  `NULL`, a number or a named numeric vector. If `NULL`, the width for
  each column is adjusted to the minimum required width. If a number,
  columns with numeric values will have the minimum width specified in
  `width`. If a named numeric vector, value names are matched against
  column names, and for each match, the specified width is used (see
  'Examples'). Only applies to text-format (see `format`).

- format:

  Name of output-format, as string. If `NULL` (or `"text"`), returned
  output is used for basic printing. Can be one of `NULL` (the default)
  resp. `"text"` for plain text, `"markdown"` (or `"md"`) for markdown
  and `"html"` for HTML output. A special option is `"tt"`, which
  creates a
  [`tinytable::tt()`](https://vincentarelbundock.github.io/tinytable/man/tt.html)
  object, where the output format is dependent on the context where the
  table is used, i.e. it can be markdown format when `export_table()` is
  used in markdown files, or LaTeX format when creating PDFs etc.

- title, caption, subtitle:

  Table title (same as caption) and subtitle, as strings. If `NULL`, no
  title or subtitle is printed, unless it is stored as attributes
  (`table_title`, or its alias `table_caption`, and `table_subtitle`).
  If you want to force that no title is printed, even if present as
  attribute, use `""`, which will never print titles. If `x` is a list
  of data frames, `caption` may be a list of table captions, one for
  each table.

- footer:

  Table footer, as string. For markdown-formatted tables, table footers,
  due to the limitation in markdown rendering, are actually just a new
  text line under the table. If `x` is a list of data frames, `footer`
  may be a list of table captions, one for each table. If `NULL`, no
  footer is printed, unless it is stored as attributes (`table_footer`).
  If you want to force that no footer is printed, even if present as
  attribute, use `""`, which will never print footers.

- column_names:

  Character vector of names that will be used as column names in the
  table. Must either be of same length as columns in the table, or a
  named vector, where names (LHS) indicate old column names, and values
  (RHS) are used as new column names.

- align:

  Column alignment. For markdown-formatted tables, the default
  `align = NULL` will right-align numeric columns, while all other
  columns will be left-aligned. If `format = "html"`, the default is
  left-align first column and center all remaining. May be a string to
  indicate alignment rules for the complete table, like `"left"`,
  `"right"`, `"center"` or `"firstleft"` (to left-align first column,
  center remaining); or a string with abbreviated alignment characters,
  where the length of the string must equal the number of columns. For
  instance, `align = "lccrl"` would left-align the first column, center
  the second and third, right-align column four and left-align the fifth
  column.

- by:

  Name of column(s) in `x` that indicates grouping for tables. When
  `format = "html"`, `by` is passed down to
  `gt::gt(groupname_col = by)`. Likewise, for `format = "tt"`, `by`
  indicates the name of the variable in the data frame, which is then
  used to create row headers in the table. For markdown and text format,
  `x` is internally split into a list of data frames. See also
  `row_groups` to group rows in the printed output.

- zap_small:

  Logical, if `TRUE`, small values are rounded after `digits` decimal
  places. If `FALSE`, values with more decimal places than `digits` are
  printed in scientific notation.

- big_mark:

  Character used as thousands separator. If `NULL` (default), no
  thousands separator is used. Use `","` for comma separator or `" "`
  for space separator.

- table_width:

  Numeric,`"auto"`, `NULL` or `Inf`, indicating the width of the
  complete table.

  - If `table_width = "auto"` (default) and the table is wider than the
    current width (i.e. line length) of the console (or any other source
    for textual output, like markdown files), the table is split into
    multiple parts.

  - Else, if `table_width` is numeric and table rows are larger than
    `table_width`, the table is split into multiple parts. For each new
    table, the first column is repeated for better orientation.

  - Use `NULL` or `Inf` to turn off automatic splitting of the table.

  - `options(easystats_table_width = <value>)` can be used to set a
    default width for tables.

- remove_duplicates:

  Logical, if `TRUE` and table is split into multiple parts, duplicated
  ("empty") rows will be removed. If `FALSE`, empty rows will be
  preserved. Only applies when `table_width` is *not* `NULL` (or `Inf`)
  *and* table is split into multiple parts.

- row_groups:

  Named list, can be used as alternative to `by` to group rows in the
  printed output, but in a more flexible way. List elements may either
  be character vectors that match the names of values in the first
  column of the data frame that belong to one group, or list elements
  can be row numbers of those value rows that should belong to one
  group. The names of the list elements will be used as group names,
  which will be inserted as "header row". Rows will be re-ordered
  according to the order used in `row_groups`, while all rows with
  non-matching values will be added to the end.

- column_groups:

  Named list, can be used to group columns in the printed output. List
  elements must indicate column indices for columns that should belong
  to one group. The names of the list elements will be used as group
  names, which will be inserted as "column header row". Currently only
  works for `format = "tt"` or `format = "html"`.

- verbose:

  Toggle messages and warnings.

- ...:

  Arguments passed to
  [`tinytable::tt()`](https://vincentarelbundock.github.io/tinytable/man/tt.html)
  and
  [`tinytable::style_tt()`](https://vincentarelbundock.github.io/tinytable/man/style_tt.html)
  when `format = "tt"`.

## Value

If `format = "text"` (or `NULL`), a formatted character string is
returned. `format = "markdown"` (or `"md"`) returns a character string
of class `knitr_kable`, which renders nicely in markdown files.
`format = "html"` returns an `gt` object (created by the **gt**
package), which - by default - is displayed in the IDE's viewer pane or
default browser. This object can be further modified with the various
gt-functions. `format = "tt"` returns a
[`tinytable::tt()`](https://vincentarelbundock.github.io/tinytable/man/tt.html)
object, which is a lightweight table format that can be used in
markdown, LaTeX, HTML and other formats, depending on the context where
the table is used.

## Note

The values for `caption`, `subtitle` and `footer` can also be provided
as attributes of `x`, e.g. if `caption = NULL` and `x` has attribute
`table_caption`, the value for this attribute will be used as table
caption. `table_subtitle` is the attribute for `subtitle`, and
`table_footer` for `footer`.

## See also

Vignettes [Formatting, printing and exporting
tables](https://easystats.github.io/insight/articles/display.html) and
[Formatting model
parameters](https://easystats.github.io/parameters/articles/model_parameters_formatting.html).

## Examples

``` r
export_table(head(iris))
#> Sepal.Length | Sepal.Width | Petal.Length | Petal.Width | Species
#> -----------------------------------------------------------------
#>         5.10 |        3.50 |         1.40 |        0.20 |  setosa
#>         4.90 |        3.00 |         1.40 |        0.20 |  setosa
#>         4.70 |        3.20 |         1.30 |        0.20 |  setosa
#>         4.60 |        3.10 |         1.50 |        0.20 |  setosa
#>         5.00 |        3.60 |         1.40 |        0.20 |  setosa
#>         5.40 |        3.90 |         1.70 |        0.40 |  setosa
export_table(head(iris), cross = "+")
#> Sepal.Length | Sepal.Width | Petal.Length | Petal.Width | Species
#> -------------+-------------+--------------+-------------+--------
#>         5.10 |        3.50 |         1.40 |        0.20 |  setosa
#>         4.90 |        3.00 |         1.40 |        0.20 |  setosa
#>         4.70 |        3.20 |         1.30 |        0.20 |  setosa
#>         4.60 |        3.10 |         1.50 |        0.20 |  setosa
#>         5.00 |        3.60 |         1.40 |        0.20 |  setosa
#>         5.40 |        3.90 |         1.70 |        0.40 |  setosa
export_table(head(iris), sep = " ", header = "*", digits = 1)
#> Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#> *********************************************************
#>          5.1         3.5          1.4         0.2  setosa
#>          4.9         3.0          1.4         0.2  setosa
#>          4.7         3.2          1.3         0.2  setosa
#>          4.6         3.1          1.5         0.2  setosa
#>          5.0         3.6          1.4         0.2  setosa
#>          5.4         3.9          1.7         0.4  setosa

# split longer tables
export_table(head(iris), table_width = 30)
#> Sepal.Length | Sepal.Width
#> --------------------------
#>         5.10 |        3.50
#>         4.90 |        3.00
#>         4.70 |        3.20
#>         4.60 |        3.10
#>         5.00 |        3.60
#>         5.40 |        3.90
#> 
#> Sepal.Length | Petal.Length
#> ---------------------------
#>         5.10 |         1.40
#>         4.90 |         1.40
#>         4.70 |         1.30
#>         4.60 |         1.50
#>         5.00 |         1.40
#>         5.40 |         1.70
#> 
#> Sepal.Length | Petal.Width | Species
#> ------------------------------------
#>         5.10 |        0.20 |  setosa
#>         4.90 |        0.20 |  setosa
#>         4.70 |        0.20 |  setosa
#>         4.60 |        0.20 |  setosa
#>         5.00 |        0.20 |  setosa
#>         5.40 |        0.40 |  setosa

# group (split) tables by variables
export_table(head(mtcars, 8), by = "cyl")
#> Group: cyl=6
#> 
#>   mpg | disp |  hp | drat |   wt |  qsec | vs | am | gear | carb
#> ----------------------------------------------------------------
#> 21.00 |  160 | 110 | 3.90 | 2.62 | 16.46 |  0 |  1 |    4 |    4
#> 21.00 |  160 | 110 | 3.90 | 2.88 | 17.02 |  0 |  1 |    4 |    4
#> 21.40 |  258 | 110 | 3.08 | 3.21 | 19.44 |  1 |  0 |    3 |    1
#> 18.10 |  225 | 105 | 2.76 | 3.46 | 20.22 |  1 |  0 |    3 |    1
#> 
#> Group: cyl=4
#> 
#>   mpg |   disp | hp | drat |   wt |  qsec | vs | am | gear | carb
#> -----------------------------------------------------------------
#> 22.80 | 108.00 | 93 | 3.85 | 2.32 | 18.61 |  1 |  1 |    4 |    1
#> 24.40 | 146.70 | 62 | 3.69 | 3.19 | 20.00 |  1 |  0 |    4 |    2
#> 
#> Group: cyl=8
#> 
#>   mpg | disp |  hp | drat |   wt |  qsec | vs | am | gear | carb
#> ----------------------------------------------------------------
#> 18.70 |  360 | 175 | 3.15 | 3.44 | 17.02 |  0 |  0 |    3 |    2
#> 14.30 |  360 | 245 | 3.21 | 3.57 | 15.84 |  0 |  0 |    3 |    4

# \donttest{
# colored footers
data(iris)
x <- as.data.frame(iris[1:5, ])
attr(x, "table_footer") <- c("This is a yellow footer line.", "yellow")
export_table(x)
#> Sepal.Length | Sepal.Width | Petal.Length | Petal.Width | Species
#> -----------------------------------------------------------------
#>         5.10 |        3.50 |         1.40 |        0.20 |  setosa
#>         4.90 |        3.00 |         1.40 |        0.20 |  setosa
#>         4.70 |        3.20 |         1.30 |        0.20 |  setosa
#>         4.60 |        3.10 |         1.50 |        0.20 |  setosa
#>         5.00 |        3.60 |         1.40 |        0.20 |  setosa
#> This is a yellow footer line.

attr(x, "table_footer") <- list(
  c("\nA yellow line", "yellow"),
  c("\nAnd a red line", "red"),
  c("\nAnd a blue line", "blue")
)
export_table(x)
#> Sepal.Length | Sepal.Width | Petal.Length | Petal.Width | Species
#> -----------------------------------------------------------------
#>         5.10 |        3.50 |         1.40 |        0.20 |  setosa
#>         4.90 |        3.00 |         1.40 |        0.20 |  setosa
#>         4.70 |        3.20 |         1.30 |        0.20 |  setosa
#>         4.60 |        3.10 |         1.50 |        0.20 |  setosa
#>         5.00 |        3.60 |         1.40 |        0.20 |  setosa
#> 
#> A yellow line
#> And a red line
#> And a blue line

attr(x, "table_footer") <- list(
  c("Without the ", "yellow"),
  c("new-line character ", "red"),
  c("we can have multiple colors per line.", "blue")
)
export_table(x)
#> Sepal.Length | Sepal.Width | Petal.Length | Petal.Width | Species
#> -----------------------------------------------------------------
#>         5.10 |        3.50 |         1.40 |        0.20 |  setosa
#>         4.90 |        3.00 |         1.40 |        0.20 |  setosa
#>         4.70 |        3.20 |         1.30 |        0.20 |  setosa
#>         4.60 |        3.10 |         1.50 |        0.20 |  setosa
#>         5.00 |        3.60 |         1.40 |        0.20 |  setosa
#> Without the new-line character we can have multiple colors per line.

# rename column names
export_table(x, column_names = letters[1:5])
#>    a |    b |    c |    d |      e
#> ----------------------------------
#> 5.10 | 3.50 | 1.40 | 0.20 | setosa
#> 4.90 | 3.00 | 1.40 | 0.20 | setosa
#> 4.70 | 3.20 | 1.30 | 0.20 | setosa
#> 4.60 | 3.10 | 1.50 | 0.20 | setosa
#> 5.00 | 3.60 | 1.40 | 0.20 | setosa
#> Without the new-line character we can have multiple colors per line.
export_table(x, column_names = c(Species = "a"))
#> Sepal.Length | Sepal.Width | Petal.Length | Petal.Width |      a
#> ----------------------------------------------------------------
#>         5.10 |        3.50 |         1.40 |        0.20 | setosa
#>         4.90 |        3.00 |         1.40 |        0.20 | setosa
#>         4.70 |        3.20 |         1.30 |        0.20 | setosa
#>         4.60 |        3.10 |         1.50 |        0.20 | setosa
#>         5.00 |        3.60 |         1.40 |        0.20 | setosa
#> Without the new-line character we can have multiple colors per line.
# }

# column-width
d <- data.frame(
  x = c(1, 2, 3),
  y = c(100, 200, 300),
  z = c(10000, 20000, 30000)
)
export_table(d)
#> x |   y |     z
#> ---------------
#> 1 | 100 | 10000
#> 2 | 200 | 20000
#> 3 | 300 | 30000
export_table(d, width = 8)
#>        x |        y |        z
#> ------------------------------
#>        1 |      100 |    10000
#>        2 |      200 |    20000
#>        3 |      300 |    30000
export_table(d, width = c(x = 5, z = 10))
#>     x |   y |          z
#> ------------------------
#>     1 | 100 |      10000
#>     2 | 200 |      20000
#>     3 | 300 |      30000
export_table(d, width = c(x = 5, y = 5, z = 10), align = "lcr")
#> x     |   y   |          z
#> --------------------------
#> 1     |  100  |      10000
#> 2     |  200  |      20000
#> 3     |  300  |      30000

# group rows in the table
# \dontrun{
data(mtcars)

# fit model
mtcars$cyl <- as.factor(mtcars$cyl)
mtcars$gear <- as.factor(mtcars$gear)
model <- lm(mpg ~ hp + gear * vs + cyl + drat, data = mtcars)

# model summary, don't select "Intercept" parameter
mp <- as.data.frame(format(
  parameters::model_parameters(model, drop = "^\\(Intercept")
))

# define groups for the table
groups <- list(
  Engine = c("cyl [6]", "cyl [8]", "vs", "hp"),
  Interactions = c(8, 9),
  Controls = c(2, 3, 7)
)

# export table with groups, using tinytable format
export_table(mp, format = "tt", row_groups = groups)
#> 
#> +-----------------+-------------+------+--------------+-------+-------+
#> | Parameter       | Coefficient | SE   | 95% CI       | t(22) | p     |
#> +=================+=============+======+==============+=======+=======+
#> | Engine                                                              |
#> +-----------------+-------------+------+--------------+-------+-------+
#> |   cyl [6]       | -2.47       | 2.21 | -7.05, 2.12  | -1.12 | 0.276 |
#> +-----------------+-------------+------+--------------+-------+-------+
#> |   cyl [8]       | 1.97        | 5.11 | -8.63, 12.58 | 0.39  | 0.703 |
#> +-----------------+-------------+------+--------------+-------+-------+
#> |   vs            | 3.18        | 3.79 | -4.68, 11.04 | 0.84  | 0.410 |
#> +-----------------+-------------+------+--------------+-------+-------+
#> |   hp            | -0.06       | 0.02 | -0.11, -0.02 | -2.91 | 0.008 |
#> +-----------------+-------------+------+--------------+-------+-------+
#> | Interactions                                                        |
#> +-----------------+-------------+------+--------------+-------+-------+
#> |   gear [4] × vs | -2.90       | 4.67 | -12.57, 6.78 | -0.62 | 0.541 |
#> +-----------------+-------------+------+--------------+-------+-------+
#> |   gear [5] × vs | 2.59        | 4.54 | -6.82, 12.00 | 0.57  | 0.574 |
#> +-----------------+-------------+------+--------------+-------+-------+
#> | Controls                                                            |
#> +-----------------+-------------+------+--------------+-------+-------+
#> |   gear [4]      | 3.10        | 4.34 | -5.90, 12.10 | 0.71  | 0.482 |
#> +-----------------+-------------+------+--------------+-------+-------+
#> |   gear [5]      | 4.80        | 3.48 | -2.42, 12.01 | 1.38  | 0.182 |
#> +-----------------+-------------+------+--------------+-------+-------+
#> |   drat          | 2.70        | 2.03 | -1.52, 6.91  | 1.33  | 0.198 |
#> +-----------------+-------------+------+--------------+-------+-------+ 
# }
```
