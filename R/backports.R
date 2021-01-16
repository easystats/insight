#' @keywords internal
#' @rawNamespace if (getRversion() < "3.6.0") export(str2lang)
str2lang <- function(s) {
  stopifnot(length(s) == 1L)
  ex <- parse(text = s, keep.source = FALSE)
  stopifnot(length(ex) == 1L)
  ex[[1L]]
}


#' @keywords internal
#' @rawNamespace if (getRversion() < "3.5.0") export(isTRUE)
isTRUE <- function(x) {
  is.logical(x) && length(x) == 1L && !is.na(x) && x
}


#' @keywords internal
#' @rawNamespace if (getRversion() < "3.5.0") export(isFALSE)
isFALSE <- function(x) {
  is.logical(x) && length(x) == 1L && !is.na(x) && !x
}
