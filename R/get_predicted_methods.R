# Printing -----------------------------------------------------------------

#' @importFrom utils head
#' @export
print.get_predicted <- function(x, ...) {
  insight::print_colour("Predicted values:\n\n", "blue")
  if (inherits(x, "data.frame") && "iter_1" %in% names(x)) {
    print.data.frame(.print_bigdata(x, ...))
    insight::print_colour("\nNOTE: You can get CIs by running `bayestestR::describe_posterior()` on this output, and reshape it to a long format with `bayestestR::reshape_iterations()`.", "yellow")
  } else {
    print(as.numeric(x))
    insight::print_colour("\nNOTE: Confidence intervals, if available, are stored as attributes and can be acccessed using `as.data.frame()` on this output.", "yellow")
  }
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
as.data.frame.get_predicted <- function(x, ...) {

  # In the case it's already a dataframe
  if (inherits(x, "data.frame")) {
    class(x) <- class(x)[class(x) != "get_predicted"]
    return(x)
  }

  out <- data.frame("Predicted" = as.numeric(x))
  if (all(c("SE") %in% names(attributes(x)))) {
    out$SE <- attributes(x)$SE
  }
  if (all(c("CI_low", "CI_high") %in% names(attributes(x)))) {
    out$CI <- attributes(x)$ci
    out$CI_low <- attributes(x)$CI_low
    out$CI_high <- attributes(x)$CI_high
  }
  out
}



#' @export
summary.get_predicted <- function(object, ...) {
  if (!inherits(object, "data.frame")) {
    return(as.data.frame(object))
  }
  out <- data.frame(Predicted = attributes(object)$Predicted)

  if (all(c("SE") %in% names(attributes(object)))) {
    out$SE <- attributes(object)$SE
  }
  if (all(c("CI_low", "CI_high") %in% names(attributes(object)))) {
    out$CI <- attributes(object)$ci
    out$CI_low <- attributes(object)$CI_low
    out$CI_high <- attributes(object)$CI_high
  }
  out
}





#' @export
as.matrix.get_predicted <- function(x, ...) {
  class(x) <- class(x)[class(x) != "get_predicted"]
  as.matrix(x)
}
