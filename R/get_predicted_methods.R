# Printing -----------------------------------------------------------------

#' @export
print.get_predicted <- function(x, ...) {
  print_colour("Predicted values:\n\n", "blue")
  # vectors have NULL columns; 1-dimensional arrays have NA columns (e.g., mgcv::gam predict() output)
  if (is.null(ncol(x)) || is.na(ncol(x))) {
    print.default(as.vector(x))
  } else {
    print.data.frame(x)
  }
  print_colour("\nNOTE: Confidence intervals, if available, are stored as attributes and can be accessed using `as.data.frame()` on this output.", "yellow")
}



.print_bigdata <- function(x, nrows = 3, ncols = 3, ...) {
  out <- x[1:nrows, 1:ncols]

  # Add row
  row <- out[1, ]
  row[1, ] <- "..."
  out <- rbind(out, row)

  # Add col
  out[[paste0("...x", ncol(x) - ncols)]] <- "..."

  row.names(out)[nrows + 1] <- paste0("...x", nrow(x) - nrows)

  class(out) <- "data.frame"
  out
}



# As data frame -----------------------------------------------------------

#' @export
as.data.frame.get_predicted <- function(x, ..., keep_iterations = TRUE) {
  # a regular data.frame (e.g., from PCA/FA)
  if (inherits(x, "data.frame") && 
      !"iterations" %in% names(attributes(x)) &&
      !"Response" %in% colnames(x)) {
    out <- as.data.frame.data.frame(x)
  # grouped response level (e.g., polr or multinom)
  } else if (inherits(x, "data.frame") && "Response" %in% colnames(x)) {
    out <- as.data.frame.data.frame(x)
    if ("ci_data" %in% names(attributes(x))) {
      out <- merge(out, attributes(x)$ci_data, by = c("Row", "Response"), sort = FALSE)
    }
  } else {
    # Then it must be predictions from a regression model
    out <- data.frame("Predicted" = as.vector(x))
    if ("ci_data" %in% names(attributes(x))) {
      out <- cbind(out, attributes(x)$ci_data)
    }
    if ("iterations" %in% names(attributes(x)) && keep_iterations == TRUE) {
      out <- cbind(out, attributes(x)$iterations)
    }
  }

  out
}



#' @export
summary.get_predicted <- function(object, ...) {
  as.data.frame(object, keep_iterations = FALSE, ...)
}



#' @export
as.matrix.get_predicted <- function(x, ...) {
  class(x) <- class(x)[class(x) != "get_predicted"]
  as.matrix(x)
}
