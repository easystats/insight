#' Download circus models
#'
#' @name download_model
#'
#' @description Downloads pre-compiled models from the \emph{circus}-repository.
#'   The \emph{circus}-repository contains a variety of fitted models to help
#'   the systematic testing of other packages
#'
#' @param name Model name.
#' @return A model from the \emph{circus}-repository.
#' @references \url{https://easystats.github.io/circus/}
#'
#' @export
download_model <- function(name) {
  .download_data_github(name)
}


#' Download rda files from github
#' @keywords internal
.download_data_github <- function(name) {
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("Package `httr` required to download models from the circus-repo.", call. = FALSE)
  }

  url <- paste0("https://raw.github.com/easystats/circus/master/data/", name, ".rda")

  temp_file <- tempfile()
  on.exit(unlink(temp_file))

  request <- httr::GET(url)
  httr::stop_for_status(request)
  writeBin(httr::content(request, type = "raw"), temp_file)


  x <- load(temp_file)
  model <- get(x)
  rm(x)

  model
}

