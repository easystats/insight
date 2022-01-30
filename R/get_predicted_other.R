# Other models ----------------------------------------------------------
# =======================================================================


#' @export
get_predicted.crr <- function(x, verbose = TRUE, ...) {
  out <- as.data.frame(unclass(stats::predict(x, ...)))
  class(out) <- c("get_predicted", class(out))
  out
}



# FA / PCA -------------------------------------------------------------
# ======================================================================


#' @export
get_predicted.principal <- function(x, data = NULL, ...) {
  if (is.null(data)) {
    out <- as.data.frame(x$scores)
  } else {
    out <- as.data.frame(stats::predict(x, data, ...))
  }
  class(out) <- c("get_predicted", class(out))
  out
}


#' @export
get_predicted.fa <- get_predicted.principal


#' @export
get_predicted.prcomp <- function(x, data = NULL, ...) {
  if (is.null(data)) {
    out <- as.data.frame(x$x)
  } else {
    out <- as.data.frame(stats::predict(x, data, ...))
  }
  class(out) <- c("get_predicted", class(out))
  out
}


#' @export
get_predicted.faMain <- function(x, data = NULL, ...) {
  check_if_installed("fungible")

  if (is.null(data)) {
    stop("A dataframe (either the original of a new one) must be provided (`get_predicted(fa_results, data = df`).")
  } else {
    out <- as.data.frame(fungible::faScores(X = data, faMainObject = x)$fscores)
  }
  class(out) <- c("get_predicted", class(out))
  out
}
