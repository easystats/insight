#' @title Download circus models
#' @name download_model
#'
#' @description Downloads pre-compiled models from the *circus*-repository.
#'   The *circus*-repository contains a variety of fitted models to help
#'   the systematic testing of other packages
#'
#' @param name Model name.
#' @param url String with the URL from where to download the model data.
#'   Optional, and should only be used in case the repository-URL is
#'   changing. By default, models are downloaded from
#'   `https://raw.github.com/easystats/circus/master/data/`.
#' @param extension File extension. Default is `.rda`.
#' @param verbose Toggle messages and warnings.
#'
#' @return A model from the *circus*-repository, or `NULL` if model could
#' not be downloaded (e.g., due to server problems).
#'
#' @details The code that generated the model is available at the
#'   <https://easystats.github.io/circus/reference/index.html>.
#'
#' @references <https://easystats.github.io/circus/>
#'
#' @examplesIf require("httr2", quietly = TRUE) && curl::has_internet() && interactive()
#' \donttest{
#' download_model("aov_1")
#' try(download_model("non_existent_model"))
#' }
#'
#' @export
download_model <- function(name,
                           url = "https://raw.github.com/easystats/circus/master/data/",
                           extension = ".rda",
                           verbose = TRUE) {
  check_if_installed("httr2", "to download models from the circus-repo")

  url <- paste0(url, name, extension)
  req <- httr2::request(url)

  temp_file <- tempfile()
  on.exit(unlink(temp_file))

  res <- tryCatch(
    {
      httr2::req_perform(req, verbosity = 0L)
    },
    error = function(e) {
      if (verbose) {
        format_alert(
          "Could not download model. Request failed with following error:",
          e$message
        )
      }
      NULL
    }
  )
  if (is.null(res)) {
    return(NULL)
  }

  writeBin(httr2::resp_body_raw(res), temp_file)

  x <- load(temp_file)
  model <- get(x)
  rm(x)

  model
}
