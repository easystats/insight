# Format messages and warnings

Inserts line breaks into a longer message or warning string. Line length
is adjusted to maximum length of the console, if the width can be
accessed. By default, new lines are indented by two spaces.

`format_alert()` is a wrapper that combines formatting a string with a
call to [`message()`](https://rdrr.io/r/base/message.html),
[`warning()`](https://rdrr.io/r/base/warning.html) or
[`stop()`](https://rdrr.io/r/base/stop.html). By default,
`format_alert()` creates a
[`message()`](https://rdrr.io/r/base/message.html). `format_warning()`
and `format_error()` change the default type of exception to
[`warning()`](https://rdrr.io/r/base/warning.html) and
[`stop()`](https://rdrr.io/r/base/stop.html), respectively.

## Usage

``` r
format_message(
  string,
  ...,
  line_length = 0.9 * getOption("width", 80),
  indent = "  "
)

format_alert(
  string,
  ...,
  line_length = 0.9 * getOption("width", 80),
  indent = "  ",
  type = "message",
  call = FALSE,
  immediate = FALSE
)

format_warning(..., immediate = FALSE)

format_error(...)
```

## Arguments

- string:

  A string.

- ...:

  Further strings that will be concatenated as indented new lines.

- line_length:

  Numeric, the maximum length of a line. The default is 90% of the width
  of the console window.

- indent:

  Character vector. If further lines are specified in `...`, a
  user-defined string can be specified to indent subsequent lines.
  Defaults to `" "` (two white spaces), hence for each start of the line
  after the first line, two white space characters are inserted.

- type:

  Type of exception alert to raise. Can be `"message"` for
  [`message()`](https://rdrr.io/r/base/message.html), `"warning"` for
  [`warning()`](https://rdrr.io/r/base/warning.html), or `"error"` for
  [`stop()`](https://rdrr.io/r/base/stop.html).

- call:

  Logical. Indicating if the call should be included in the the error
  message. This is usually confusing for users when the function
  producing the warning or error is deep within another function, so the
  default is `FALSE`.

- immediate:

  Logical. Indicating if the *warning* should be printed immediately.
  Only applies to `format_warning()` or `format_alert()` with
  `type = "warning"`. The default is `FALSE`.

## Value

For `format_message()`, a formatted string. For `format_alert()` and
related functions, the requested exception, with the exception formatted
using `format_message()`.

## Details

There is an experimental formatting feature implemented in this
function. You can use following tags:

- `{.b text}` for bold formatting

- `{.i text}` to use italic font style

- `{.url www.url.com}` formats the string as URL (i.e., enclosing URL in
  `<` and `>`, blue color and italic font style)

- `{.pkg packagename}` formats the text in blue color.

This features has some limitations: it's hard to detect the exact length
for each line when the string has multiple lines (after line breaks) and
the string contains formatting tags. Thus, it can happen that lines are
wrapped at an earlier length than expected. Furthermore, if you have
multiple words in a format tag (`{.b one two three}`), a line break
might occur inside this tag, and the formatting no longer works (messing
up the message-string).

## Examples

``` r
msg <- format_message("Much too long string for just one line, I guess!",
  line_length = 15
)
message(msg)
#> Much too long
#>   string for just
#>   one line, I
#>   guess!

msg <- format_message("Much too long string for just one line, I guess!",
  "First new line",
  "Second new line",
  "(both indented)",
  line_length = 30
)
message(msg)
#> Much too long string for just
#>   one line, I guess!
#>   First new line
#>   Second new line
#>   (both indented)

msg <- format_message("Much too long string for just one line, I guess!",
  "First new line",
  "Second new line",
  "(not indented)",
  line_length = 30,
  indent = ""
)
message(msg)
#> Much too long string for just
#>   one line, I guess!
#> First new line
#> Second new line
#> (not indented)

# Caution, experimental! See 'Details'
msg <- format_message(
  "This is {.i italic}, visit {.url easystats.github.io/easystats}",
  line_length = 30
)
message(msg)
#> This is italic, visit
#>   <easystats.github.io/easystats>

# message
format_alert("This is a message.")
#> This is a message.
format_alert("This is a warning.", type = "message")
#> This is a warning.

# error
try(format_error("This is an error."))
#> Error : This is an error.
# warning
format_warning("This is a warning.")
#> Warning: This is a warning.
```
