# Generic export of data frames into formatted tables

`display()` is a generic function to export data frames into various
table formats (like plain text, markdown, ...). `print_md()` usually is
a convenient wrapper for `display(format = "markdown")`. Similar,
`print_html()` is a shortcut for `display(format = "html")`. See the
documentation for the specific objects' classes.

## Usage

``` r
display(object, ...)

print_md(x, ...)

print_html(x, ...)

# S3 method for class 'data.frame'
display(object, format = "markdown", ...)

# S3 method for class 'data.frame'
print_md(x, ...)

# S3 method for class 'data.frame'
print_html(x, ...)
```

## Arguments

- object, x:

  A data frame.

- ...:

  Arguments passed to other methods.

- format:

  String, indicating the output format. Can be `"markdown"` or `"html"`.
  A special option is `"tt"`, which creates a
  [`tinytable::tt()`](https://vincentarelbundock.github.io/tinytable/man/tt.html)
  object, where the output format is dependent on the context where the
  table is used, i.e. it can be markdown format when
  [`export_table()`](https://easystats.github.io/insight/reference/export_table.md)
  is used in markdown files, or LaTex format when creating PDFs etc.

## Value

Depending on `format`, either an object of class `gt_tbl`, `tinytable`,
or a character vector of class `knitr_kable`.

## Global Options to Customize Output when Printing

- `easystats_display_format`:
  `options(easystats_display_format = <value>)` will set the default
  format for the `display()` methods. Can be one of `"markdown"`,
  `"html"`, or `"tt"`.

## Examples

``` r
display(iris[1:5, ], format = "html")


  

Sepal.Length
```
