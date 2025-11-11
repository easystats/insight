# Download circus models

Downloads pre-compiled models from the *circus*-repository. The
*circus*-repository contains a variety of fitted models to help the
systematic testing of other packages

## Usage

``` r
download_model(
  name,
  url = "https://raw.github.com/easystats/circus/master/data/",
  extension = ".rda",
  verbose = TRUE
)
```

## Arguments

- name:

  Model name.

- url:

  String with the URL from where to download the model data. Optional,
  and should only be used in case the repository-URL is changing. By
  default, models are downloaded from
  `https://raw.github.com/easystats/circus/master/data/`.

- extension:

  File extension. Default is `.rda`.

- verbose:

  Toggle messages and warnings.

## Value

A model from the *circus*-repository, or `NULL` if model could not be
downloaded (e.g., due to server problems).

## Details

The code that generated the model is available at the
<https://easystats.github.io/circus/reference/index.html>.

## References

<https://easystats.github.io/circus/>

## Examples

``` r
if (FALSE) { # require("httr2", quietly = TRUE) && curl::has_internet() && interactive()
# \donttest{
download_model("aov_1")
try(download_model("non_existent_model"))
# }
}
```
