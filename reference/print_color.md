# Coloured console output

Convenient function that allows coloured output in the console. Mainly
implemented to reduce package dependencies.

## Usage

``` r
print_color(text, color)

print_colour(text, colour)

color_text(text, color)

colour_text(text, colour)

color_theme()
```

## Arguments

- text:

  The text to print.

- color, colour:

  Character vector, indicating the colour for printing. May be one of
  `"white"`, `"black"`, `"red"`, `"yellow"`, `"green"`, `"blue"`,
  `"violet"`, `"cyan"` or `"grey"`. Bright variants of colors are
  available by adding the prefix `"b"` (or `"br_"` or `"bright_"`), e.g.
  `"bred"` (or `"br_red"` resp. `"bright_red"`). Background colors can
  be set by adding the prefix `"bg_"` (e.g. `"bg_red"`). Formatting is
  also possible with `"bold"` or `"italic"`. Note that `"bright_black"`
  is equivalent to `"grey"`, and `"bg_grey"` has no effect (it is
  equivalent to the IDE's default background).

## Value

Nothing.

## Details

This function prints `text` directly to the console using
[`cat()`](https://rdrr.io/r/base/cat.html), so no string is returned.
`color_text()`, however, returns only the formatted string, without
using [`cat()`](https://rdrr.io/r/base/cat.html). `color_theme()` either
returns `"dark"` when RStudio is used with dark color scheme, `"light"`
when it's used with light theme, and `NULL` if the theme could not be
detected.

## Examples

``` r
print_color("I'm blue dabedi dabedei", "blue")
#> I'm blue dabedi dabedei
```
