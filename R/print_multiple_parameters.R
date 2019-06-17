#' @title Get clean names of model paramters
#' @name print_multiple_parameters
#'
#' @description This function
#'
#' @param x A fitted model.
#' @param ... One or more objects (data frames), which contain information about
#'   the model parameters and related statistics (like confidence intervals, HDI,
#'   ROPE, ...).
#' @param split_by If \code{NULL}, the merged data frame is returned. Else, the
#'   data frame is split into a list, split by the values from those columns defined
#'   in \code{split_by}. \code{split_by} should be a character vector with one or
#'   more of the following elements: \code{"Effects"}, \code{"Component"},
#'   \code{"Response"} and \code{"Group"}.
#'
#' @return A data frame or a list.
#'
#'
#' @examples
#' library(bayestestR)
#' model <- download_model("brms_zi_2")
#' x <- hdi(model, effects = "all", component = "all")
#' x
#' print_multiple_parameters(model, x)
#'
#' @importFrom stats na.omit
#' @export
print_multiple_parameters <- function(x, ..., split_by = c("Effects", "Component", "Group", "Response")) {
  obj <- list(...)

  cp <- if (!inherits(x, "clean_parameters"))
    clean_parameters(x)
  else
    x
  cn1 <- colnames(cp)

  obj <- lapply(obj, function(i) {
    # make sure we have a Parameter column
    if (!"Parameter" %in% colnames(i)) colnames(i)[1] <- "Parameter"
    # remove all other common columns that might produce duplicates
    cn2 <- colnames(i)
    dupes <- stats::na.omit(match(cn1, cn2)[-1])
    if (length(dupes) > 0) i <- i[, -dupes, drop = FALSE]
    i
  })

  # merge all objects together
  obj <- Reduce(function(x, y) merge(x, y, all.x = FALSE, by = "Parameter", sort = FALSE), c(list(cp), obj))

  # return merged data frame if no splitting requested
  if (.is_empty_object(split_by)) return(obj)

  # determine where to split data frames
  split_by <- split_by[split_by %in% colnames(obj)]
  f <- lapply(split_by, function(i) {
    if (i %in% colnames(obj)) obj[[i]]
  })
  names(f) <- split_by

  # split into groups, remove empty elements
  out <- split(obj, f)
  out <- .compact_list(lapply(out, function(i) { if (nrow(i) > 0) i }))

  # remove trailing dots
  names(out) <- list_names <- gsub("(.*)\\.$", "\\1", names(out))

  has_zeroinf <- any(grepl("zero_inflated", names(out), fixed = TRUE))

  # create title attributes
  out <- lapply(names(out), function(i) {
    title1 <- title2 <- ""
    element <- out[[i]]
    parts <- unlist(strsplit(i, ".", fixed = TRUE))
    for (j in 1:length(parts)) {
      if (parts[j] %in% c("fixed", "random") || (has_zeroinf && parts[j] %in% c("conditional", "zero_inflated"))) {
        tmp <- switch(
          parts[j],
          "fixed" = "Fixed effects",
          "random" = "Random effects",
          "conditional" = "(conditional)",
          "zero_inflated" = "(zero-inflated)"
        )
        title1 <- paste0(title1, " ", tmp)
      } else if (!parts[j] %in% c("conditional", "zero_inflated")) {
        title2 <- paste0(title2, " ", parts[j])
      }
    }
    attr(element, "main_title") <- .trim(title1)
    attr(element, "sub_title") <- .trim(title2)
    element
  })

  names(out) <- list_names
  out
}