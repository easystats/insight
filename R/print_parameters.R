#' @title Prepare summary statistics of model parameters for printing
#' @name print_parameters
#'
#' @description This function takes a data frame, typically a data frame with
#' information on summaries of model parameters like \code{\link[bayestestR]{describe_posterior}},
#' \code{\link[bayestestR]{hdi}} or \code{\link[parameters]{model_parameters}},
#' as input and splits this information into several parts, depending on the
#' model. See details below.
#'
#' @param x A fitted model, or a data frame returned by \code{\link{clean_parameters}}.
#' @param ... One or more objects (data frames), which contain information about
#'   the model parameters and related statistics (like confidence intervals, HDI,
#'   ROPE, ...).
#' @param split_by \code{split_by} should be a character vector with one or
#'   more of the following elements: \code{"Effects"}, \code{"Component"},
#'   \code{"Response"} and \code{"Group"}. These are the column names returned
#'   by \code{\link{clean_parameters}}, which is used to extract the information
#'   from which the group or component model parameters belong. If \code{NULL}, the
#'   merged data frame is returned. Else, the data frame is split into a list,
#'   split by the values from those columns defined in \code{split_by}.
#' @param format Name of output-format, as string. If \code{NULL} (or \code{"text"}),
#'   assumed use for output is basic printing. If \code{"markdown"}, markdown-format
#'   is assumed. This only affects the style of title- and table-caption attributes,
#'   which are used in \code{\link{export_table}}.
#' @param parameter_column String, name of the column that contains the
#'   parameter names. Usually, for data frames returned by functions the
#'   easystats-packages, this will be \code{"Parameter"}.
#' @param keep_parameter_column Logical, if \code{TRUE}, the data frames in the
#'   returned list have both a \code{"Cleaned_Parameter"} and \code{"Parameter"}
#'   column. If \code{FALSE}, the (unformatted) \code{"Parameter"} is removed,
#'   and the column with cleaned parameter names (\code{"Cleaned_Parameter"}) is
#'   renamed into \code{"Parameter"}.
#' @param remove_empty_column Logical, if \code{TRUE}, columns with completely
#'   empty character values will be removed.
#' @param titles,subtitles By default, the names of the model components (like
#'   fixed or random effects, count or zero-inflated model part) are added as
#'   attributes \code{"table_title"} and \code{"table_subtitle"} to each list
#'   element returned by \code{print_parameters()}. These attributes are then
#'   extracted and used as table (sub) titles in \code{\link{export_table}}.
#'   Use \code{titles} and \code{subtitles} to override the default attribute
#'   values for \code{"table_title"} and \code{"table_subtitle"}. \code{titles}
#'   and \code{subtitles} may be any length from 1 to same length as returned
#'   list elements. If \code{titles} and \code{subtitles} are shorter than
#'   existing elements, only the first default attributes are overwritten.
#'
#' @return A data frame or a list of data frames (if \code{split_by} is not \code{NULL}).
#' If a list is returned, the element names reflect the model components where the
#' extracted information in the data frames belong to, e.g. \code{`random.zero_inflated.Intercept: persons`}.
#' This is the data frame that contains the parameters for the random effects from
#' group-level "persons" from the zero-inflated model component.
#'
#' @details This function prepares data frames that contain information
#' about model parameters for clear printing.
#' \cr \cr
#' First, \code{x} is required, which should either be a model object or a
#' prepared data frame as returned by \code{\link{clean_parameters}}. If
#' \code{x} is a model, \code{clean_parameters()} is called on that model
#' object to get information with which model components the parameters
#' are associated.
#' \cr \cr
#' Then, \code{...} take one or more data frames that also contain information
#' about parameters from the same model, but also have additional information
#' provided by other methods. For instance, a data frame in \code{...} might
#' be the result of, for instance, \code{\link[bayestestR]{describe_posterior}},
#' or \code{\link[parameters]{model_parameters}}, where we have a) a
#' \code{Parameter} column and b) columns with other parameter values (like
#' CI, HDI, test statistic, etc.).
#' \cr \cr
#' Now we have a data frame with model parameters and information about the
#' association to the different model components, a data frame with model
#' parameters, and some summary statistics. \code{print_parameters()}
#' then merges these data frames, so the parameters or statistics of interest
#' are also associated with the different model components. The data
#' frame is split into a list, so for a clear printing. Users can loop over this
#' list and print each component for a better overview. Further, parameter
#' names are "cleaned", if necessary, also for a cleaner print. See also 'Examples'.
#'
#' @examples
#' \dontrun{
#' library(bayestestR)
#' model <- download_model("brms_zi_2")
#' x <- hdi(model, effects = "all", component = "all")
#'
#' # hdi() returns a data frame; here we use only the
#' # information on parameter names and HDI values
#' tmp <- as.data.frame(x)[, 1:4]
#' tmp
#'
#' # Based on the "split_by" argument, we get a list of data frames that
#' # is split into several parts that reflect the model components.
#' print_parameters(model, tmp)
#'
#' # This is the standard print()-method for "bayestestR::hdi"-objects.
#' # For printing methods, it is easy to print complex summary statistics
#' # in a clean way to the console by splitting the information into
#' # different model components.
#' x
#' }
#' @importFrom stats na.omit
#' @export
print_parameters <- function(x,
                             ...,
                             split_by = c("Effects", "Component", "Group", "Response"),
                             format = "text",
                             parameter_column = "Parameter",
                             keep_parameter_column = TRUE,
                             remove_empty_column = FALSE,
                             titles = NULL,
                             subtitles = NULL) {
  obj <- list(...)

  # save attributes of original object
  att <- do.call(c, .compact_list(lapply(obj, function(i) {
    a <- attributes(i)
    a$names <- a$class <- a$row.names <- NULL
    a
  })))
  att <- att[!duplicated(names(att))]

  # get cleaned parameters
  cp <- if (!inherits(x, "clean_parameters")) {
    clean_parameters(x)
  } else {
    x
  }

  # merge all objects together
  obj <- Reduce(
    function(x, y) {
      # check for valid column name
      if (parameter_column != "Parameter" && parameter_column %in% colnames(y) && !"Parameter" %in% colnames(y)) {
        colnames(y)[colnames(y) == parameter_column] <- "Parameter"
      }
      merge_by <- unique(c("Parameter", intersect(colnames(y), intersect(c("Effects", "Component", "Group", "Response"), colnames(x)))))
      merge(x, y, all.x = FALSE, by = merge_by, sort = FALSE)
    },
    c(list(cp), obj)
  )

  # return merged data frame if no splitting requested
  if (.is_empty_object(split_by)) {
    return(obj)
  }

  # determine where to split data frames
  split_by <- split_by[split_by %in% colnames(obj)]
  f <- lapply(split_by, function(i) {
    if (i %in% colnames(obj)) obj[[i]]
  })
  names(f) <- split_by

  # split into groups, remove empty elements
  out <- split(obj, f)
  out <- .compact_list(lapply(out, function(i) {
    if (nrow(i) > 0) i
  }))

  # remove trailing dots
  names(out) <- list_names <- gsub("(.*)\\.$", "\\1", names(out))

  has_zeroinf <- any(grepl("zero_inflated", names(out), fixed = TRUE))

  # create title attributes, and remove unnecessary columns from output
  out <- lapply(names(out), function(i) {

    # init title variables
    title1 <- title2 <- ""

    # get data frame
    element <- out[[i]]

    # split name at ".", so we have all components the data frame refers to (i.e.
    # fixed/random, conditional/zero-inflated, group-lvl or random slope etc.)
    # as character vector
    parts <- unlist(strsplit(i, ".", fixed = TRUE))

    # iterate all parts of the component names, to create title attribute
    for (j in 1:length(parts)) {

      # Rename "fixed", "random" etc. into proper titles. Here we have the
      # "Main title" of a subcomponent (like "Random effects")
      if (parts[j] %in% c("fixed", "random") || (has_zeroinf && parts[j] %in% c("conditional", "zero_inflated"))) {
        tmp <- switch(parts[j],
          "fixed" = "Fixed effects",
          "random" = "Random effects",
          "dispersion" = "Dispersion",
          "conditional" = "(conditional)",
          "zero_inflated" = "(zero-inflated)"
        )
        title1 <- paste0(title1, " ", tmp)
      } else if (!parts[j] %in% c("conditional", "zero_inflated")) {
        # here we have the "subtitles" of a subcomponent
        # (like "Intercept: Group-Level 1")
        tmp <- switch(parts[j],
          "simplex" = "(monotonic effects)",
          parts[j]
        )
        title2 <- paste0(title2, " ", tmp)
      }
    }

    .effects <- unique(element$Effects)
    .component <- unique(element$Component)
    .group <- unique(element$Group)

    # we don't need "Effects" and "Component" column any more, and probably
    # also no longer the "Group" column
    columns_to_remove <- c("Effects", "Component", "Cleaned_Parameter")
    if (.n_unique(.group) == 1) {
      columns_to_remove <- c(columns_to_remove, "Group")
    } else {
      .group <- NULL
    }
    keep <- setdiff(colnames(element), columns_to_remove)
    element <- element[, c("Cleaned_Parameter", keep)]

    # if we had a pretty_names attributes in the original object,
    # match parameters of pretty names here, and add this attributes
    # to each element here...
    if ("pretty_names" %in% names(att)) {
      attr(element, "pretty_names") <- setNames(att$pretty_names[element$Parameter], element$Cleaned_Parameter)
    }

    # keep or remove old parameter column?
    if (!isTRUE(keep_parameter_column)) {
      element$Parameter <- NULL
      colnames(element)[colnames(element) == "Cleaned_Parameter"] <- "Parameter"
    }

    # remove empty columns
    if (isTRUE(remove_empty_column)) {
      for (j in colnames(element)) {
        if (all(is.na(element[[j]])) || (is.character(element[[j]]) && all(element[[j]] == ""))) {
          element[[j]] <- NULL
        }
      }
    }

    # for sub-table titles
    if (is.null(format) || format == "text") {
      title_prefix <- "# "
    } else {
      title_prefix <- ""
    }

    title1 <- .capitalize(title1)
    title2 <- .capitalize(title2)

    # add attributes
    attr(element, "main_title") <- .trim(title1)
    attr(element, "sub_title") <- .trim(title2)
    if (is.null(format) || format == "text") {
      attr(element, "table_caption") <- c(paste0(title_prefix, .trim(title1)), "blue")
      attr(element, "table_subtitle") <- c(.trim(title2), "blue")
    } else {
      attr(element, "table_caption") <- .trim(title1)
      attr(element, "table_subtitle") <- .trim(title2)
    }
    attr(element, "Effects") <- .effects
    attr(element, "Component") <- .component
    attr(element, "Group") <- .group

    element
  })

  # override titles?
  if (!is.null(titles) && length(titles) <= length(out)) {
    for (i in 1:length(titles)) {
      attr(out[[i]], "table_caption") <- c(titles[i], "blue")
    }
  }

  if (!is.null(subtitles) && length(subtitles) <= length(out)) {
    for (i in 1:length(subtitles)) {
      attr(out[[i]], "table_subtitle") <- c(subtitles[i], "blue")
    }
  }

  att$pretty_names <- NULL
  attr(out, "additional_attributes") <- att
  names(out) <- list_names
  out
}
