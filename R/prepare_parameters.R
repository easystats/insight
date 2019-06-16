#' @export
prepare_parameters <- function(x, ...) {
  obj <- list(...)
  obj <- lapply(obj, function(i) {
    if (!"Parameter" %in% colnames(i)) colnames(i)[1] <- "Parameter"
    i
  })

  obj <- list(clean_parameters(x), obj)

  obj <- Reduce(function(x, y) merge(x, y, all = TRUE, by = "Parameter", sort = FALSE), obj)
}