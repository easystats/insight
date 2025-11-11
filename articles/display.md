# Formatting, printing and exporting tables

## The difference between a dataframe and its render

Most of objects encountered throughout the {easystats} packages are
“tables”, i.e., a 2D matrix with columns and rows. In R, these objects
are often, at their core, *data frames*. Let’s create one to use as an
example:

``` r

library(insight)

df <- data.frame(
  Variable = c(1, 3, 5, 3, 1),
  Group = c("A", "A", "A", "B", "B"),
  CI = c(0.95, 0.95, 0.95, 0.95, 0.95),
  CI_low = c(3.35, 2.425, 6.213, 12.1, 1.23),
  CI_high = c(4.23, 5.31, 7.123, 13.5, 3.61),
  p = c(0.001, 0.0456, 0.45, 0.0042, 0.34)
)

df
#>   Variable Group   CI CI_low CI_high      p
#> 1        1     A 0.95  3.350   4.230 0.0010
#> 2        3     A 0.95  2.425   5.310 0.0456
#> 3        5     A 0.95  6.213   7.123 0.4500
#> 4        3     B 0.95 12.100  13.500 0.0042
#> 5        1     B 0.95  1.230   3.610 0.3400
```

When I display in in the console (calling an object - e.g. `df` - is
actually equivalent to calling `print(df)`), the output looks alright,
but it could be improved. Some packages, such as {knitr}, have functions
to create a nicer output. For instance, in markdown, so that it can be
nicely rendered in markdown documents when copied:

``` r

knitr::kable(df, format = "markdown")
```

    | Variable|Group |   CI| CI_low| CI_high|      p|
    |--------:|:-----|----:|------:|-------:|------:|
    |        1|A     | 0.95|  3.350|   4.230| 0.0010|
    |        3|A     | 0.95|  2.425|   5.310| 0.0456|
    |        5|A     | 0.95|  6.213|   7.123| 0.4500|
    |        3|B     | 0.95| 12.100|  13.500| 0.0042|
    |        1|B     | 0.95|  1.230|   3.610| 0.3400|

Or HTML, which again makes it look great in HTML files (such as this
webpage you’re reading):

``` r

knitr::kable(df, format = "html")
```

| Variable | Group |   CI | CI_low | CI_high |      p |
|---------:|:------|-----:|-------:|--------:|-------:|
|        1 | A     | 0.95 |  3.350 |   4.230 | 0.0010 |
|        3 | A     | 0.95 |  2.425 |   5.310 | 0.0456 |
|        5 | A     | 0.95 |  6.213 |   7.123 | 0.4500 |
|        3 | B     | 0.95 | 12.100 |  13.500 | 0.0042 |
|        1 | B     | 0.95 |  1.230 |   3.610 | 0.3400 |

## The *insight* workflow

The {insight} package also contains function to improve the “printing”,
or rendering, of tables. Its design dissociates two separate and
independent steps: *formatting* and *exporting*.

### Formatting

The purpose of formatting is to improve a given table, while still
keeping it as a regular R data frame, so that it can be for instance
further modified by the user.

``` r

format_table(df)
#>   Variable Group         95% CI     p
#> 1        1     A [ 3.35,  4.23] 0.001
#> 2        3     A [ 2.42,  5.31] 0.046
#> 3        5     A [ 6.21,  7.12] 0.450
#> 4        3     B [12.10, 13.50] 0.004
#> 5        1     B [ 1.23,  3.61] 0.340
```

As you can see,
[`format_table()`](https://easystats.github.io/insight/reference/format_table.md)
modifies columns, turning number into characters (so that it has the
same amount of digits), and detecting confidence intervals. This is
usually combined with column-specific formatting functions, like
[`format_p()`](https://easystats.github.io/insight/reference/format_p.md):

``` r

library(datawizard) # for data_modify()
df |>
  data_modify(p = format_p(p, stars = TRUE)) |>
  format_table()
#>   Variable Group         95% CI           p
#> 1        1     A [ 3.35,  4.23] p = 0.001**
#> 2        3     A [ 2.42,  5.31] p = 0.046* 
#> 3        5     A [ 6.21,  7.12] p = 0.450  
#> 4        3     B [12.10, 13.50] p = 0.004**
#> 5        1     B [ 1.23,  3.61] p = 0.340
```

## Using unicode symbols as effect size names

With `use_symbols = TRUE`, it is possible to render certain effect size
names as symbols, if these are used as column names. Note that this only
works on OS X or Linux, or on Windows from R 4.2 or higher.

``` r

x <- data.frame(
  phi_adjusted = 0.3,
  Glass_delta = 0.4,
  Epsilon2 = 0.7,
  R2 = 0.4
)

# standard output
format_table(x)

# column names of effect sizes as symbols
format_table(x, use_symbols = TRUE)
```

In combination with
[`export_table()`](https://easystats.github.io/insight/reference/export_table.md)
(see next section), this will give you nicely formatted tables.

``` r

export_table(format_table(x, use_symbols = TRUE))
```

### Exporting

The next step is *exporting*, which takes a data frame and renders it in
a given format, so that it looks good in the console, or in markdown,
HTML or latex.

``` r

export_table(df)
#> Variable | Group |   CI | CI_low | CI_high |        p
#> -----------------------------------------------------
#>        1 |     A | 0.95 |   3.35 |    4.23 | 1.00e-03
#>        3 |     A | 0.95 |   2.42 |    5.31 |     0.05
#>        5 |     A | 0.95 |   6.21 |    7.12 |     0.45
#>        3 |     B | 0.95 |  12.10 |   13.50 | 4.20e-03
#>        1 |     B | 0.95 |   1.23 |    3.61 |     0.34
```

For markdown or HTML, simply change the `format` argument to markdown
(“md”)…

``` r

export_table(df, format = "md")
```

| Variable | Group | CI   | CI_low | CI_high | p        |
|---------:|------:|:-----|-------:|--------:|:---------|
|        1 |     A | 0.95 |   3.35 |    4.23 | 1.00e-03 |
|        3 |     A | 0.95 |   2.42 |    5.31 | 0.05     |
|        5 |     A | 0.95 |   6.21 |    7.12 | 0.45     |
|        3 |     B | 0.95 |  12.10 |   13.50 | 4.20e-03 |
|        1 |     B | 0.95 |   1.23 |    3.61 | 0.34     |

…or HTML format.

``` r

export_table(df, format = "html")
```

| Variable | CI   | CI_low | CI_high | p        |
|----------|------|--------|---------|----------|
| A        |      |        |         |          |
| 1        | 0.95 | 3.35   | 4.23    | 1.00e-03 |
| 3        | 0.95 | 2.42   | 5.31    | 0.05     |
| 5        | 0.95 | 6.21   | 7.12    | 0.45     |
| B        |      |        |         |          |
| 3        | 0.95 | 12.10  | 13.50   | 4.20e-03 |
| 1        | 0.95 | 1.23   | 3.61    | 0.34     |

This can be combined with
[`format_table()`](https://easystats.github.io/insight/reference/format_table.md).

``` r

df |>
  format_table(ci_brackets = c("(", ")")) |>
  export_table(format = "html")
```

| Variable | 95% CI         | p     |
|----------|----------------|-------|
| A        |                |       |
| 1        | ( 3.35, 4.23)  | 0.001 |
| 3        | ( 2.42, 5.31)  | 0.046 |
| 5        | ( 6.21, 7.12)  | 0.450 |
| B        |                |       |
| 3        | (12.10, 13.50) | 0.004 |
| 1        | ( 1.23, 3.61)  | 0.340 |

## Displaying tables with `display()`

While
[`export_table()`](https://easystats.github.io/insight/reference/export_table.md)
gives you fine-grained control, the
[`display()`](https://easystats.github.io/insight/reference/display.md)
function serves as a high-level wrapper. It is designed to take an
object and “display” it in a user-friendly format. Most of the functions
in [easystats](https://easystats.github.io/easystats/) that produce
table-like outputs usually also have a
[`display()`](https://easystats.github.io/insight/reference/display.md)
method, which is a convenient way to visualize the results in different
formats, which integrates nicely in markdown or quarto documents.

Let’s see how it works with our example data frame:

``` r

# By default, display() creates a markdown table
display(df)
```

| Variable | Group | CI   | CI_low | CI_high | p        |
|---------:|------:|:-----|-------:|--------:|:---------|
|        1 |     A | 0.95 |   3.35 |    4.23 | 1.00e-03 |
|        3 |     A | 0.95 |   2.42 |    5.31 | 0.05     |
|        5 |     A | 0.95 |   6.21 |    7.12 | 0.45     |
|        3 |     B | 0.95 |  12.10 |   13.50 | 4.20e-03 |
|        1 |     B | 0.95 |   1.23 |    3.61 | 0.34     |

You can specify the output format using the `format` argument. For
instance, to create a rich HTML table (powered by the {gt} package), you
can use `format = "html"`:

``` r

display(df, format = "html")
```

| Variable | CI   | CI_low | CI_high | p        |
|----------|------|--------|---------|----------|
| A        |      |        |         |          |
| 1        | 0.95 | 3.35   | 4.23    | 1.00e-03 |
| 3        | 0.95 | 2.42   | 5.31    | 0.05     |
| 5        | 0.95 | 6.21   | 7.12    | 0.45     |
| B        |      |        |         |          |
| 3        | 0.95 | 12.10  | 13.50   | 4.20e-03 |
| 1        | 0.95 | 1.23   | 3.61    | 0.34     |

A special option is `format = "tt"`, which creates a table using the
{tinytable} package. This is a very flexible format, as the table can be
rendered to HTML, LaTeX, or other formats depending on the context.

``` r

display(df, format = "tt")
```

| Variable | Group | CI   | CI_low | CI_high | p        |
|----------|-------|------|--------|---------|----------|
| 1        | A     | 0.95 | 3.35   | 4.23    | 1.00e-03 |
| 3        | A     | 0.95 | 2.42   | 5.31    | 0.05     |
| 5        | A     | 0.95 | 6.21   | 7.12    | 0.45     |
| 3        | B     | 0.95 | 12.10  | 13.50   | 4.20e-03 |
| 1        | B     | 0.95 | 1.23   | 3.61    | 0.34     |

Since all arguments are passed to
[`export_table()`](https://easystats.github.io/insight/reference/export_table.md),
you can also use additional arguments like the `by` argument for
grouping.

``` r

display(df, format = "tt", by = "Group")
```

| Variable | CI   | CI_low | CI_high | p        |
|----------|------|--------|---------|----------|
| A        | A    | A      | A       | A        |
| 1        | 0.95 | 3.35   | 4.23    | 1.00e-03 |
| 3        | 0.95 | 2.42   | 5.31    | 0.05     |
| 5        | 0.95 | 6.21   | 7.12    | 0.45     |
| B        | B    | B      | B       | B        |
| 3        | 0.95 | 12.10  | 13.50   | 4.20e-03 |
| 1        | 0.95 | 1.23   | 3.61    | 0.34     |
