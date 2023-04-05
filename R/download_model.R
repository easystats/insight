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
#'
#' @return A model from the *circus*-repository, or `NULL` if model could
#' not be downloaded (e.g., due to server problems).
#'
#' @details The code that generated the model is available at the
#'   <https://easystats.github.io/circus/reference/index.html>.
#'
#' @references <https://easystats.github.io/circus/>
#'
#' @export
download_model <- function(name, url = NULL) {
  .download_data_github(name, url)
}


# Download rda files from github
.download_data_github <- function(name, url) {
  check_if_installed("httr", "to download models from the circus-repo")

  if (is.null(url)) {
    url <- "https://raw.github.com/easystats/circus/master/data/"
  }

  url <- paste0(url, name, ".rda")

  temp_file <- tempfile()
  on.exit(unlink(temp_file))

  result <- tryCatch(
    {
      request <- httr::GET(url)
      httr::stop_for_status(request)
    },
    error = function(e) {
      format_alert(
        "Could not download model. Request failed with following error:",
        e$message
      )
      NULL
    }
  )
  if (is.null(result)) {
    return(NULL)
  }

  writeBin(httr::content(request, type = "raw"), temp_file)

  x <- load(temp_file)
  model <- get(x)
  rm(x)

  model
}
