#' Count number of random effect levels in a mixed model
#'
#' Returns the number of group levels of random effects from mixed models.
#'
#' @param x A mixed model.
#' @param ... Additional arguments that can be passed to the function. Currently,
#'   you can use `data` to provide the model data, if available, to avoid
#'   retrieving model data multiple times.
#'
#' @return The number of group levels in the model.
#'
#' @examplesIf require("lme4")
#' data(sleepstudy, package = "lme4")
#' set.seed(12345)
#' sleepstudy$grp <- sample(1:5, size = 180, replace = TRUE)
#' sleepstudy$subgrp <- NA
#' for (i in 1:5) {
#'   filter_group <- sleepstudy$grp == i
#'   sleepstudy$subgrp[filter_group] <-
#'     sample(1:30, size = sum(filter_group), replace = TRUE)
#' }
#' model <- lme4::lmer(
#'   Reaction ~ Days + (1 | grp / subgrp) + (1 | Subject),
#'   data = sleepstudy
#' )
#' n_grouplevels(model)
#' @export
n_grouplevels <- function(x, ...) {
  if (!is_mixed_model(x)) {
    format_error("`x` must be a mixed model.")
  }

  # try to extract random effects
  ran_eff <- .safe(find_random(x, split_nested = TRUE, flatten = TRUE))

  # retrieve model data - may be passed via "..."
  dot_args <- list(...)
  if ("data" %in% names(dot_args)) {
    re_data <- dot_args$data
  } else {
    re_data <- .safe(get_data(x, verbose = FALSE))
  }

  # validation check - did we successfully retrieve data and random effects?
  if (is.null(re_data) || is.null(ran_eff)) {
    return(NULL)
  }

  # make sure we only have valid columns
  re_data <- re_data[intersect(colnames(re_data), ran_eff)]

  # extract group levels
  re_levels <- vapply(re_data, n_unique, 1L)

  # retrieve names - needs checking for names attribute for R < 4.0
  group_names <- names(re_levels)
  if (is.null(group_names)) {
    group_names <- colnames(re_data)
  }

  out <- .safe(
    data.frame(
      Group = group_names,
      N_levels = unname(re_levels),
      stringsAsFactors = FALSE
    )
  )

  # validation check
  if (is.null(out)) {
    return(NULL)
  }

  # add interactions, if any
  ran_eff_int <- find_random(x, split_nested = FALSE, flatten = TRUE)
  re_int <- grep(":", ran_eff_int, fixed = TRUE, value = TRUE)
  if (length(re_int)) {
    tmp <- do.call(rbind, lapply(re_int, function(i) {
      pars <- unlist(strsplit(i, ":", fixed = TRUE))
      data.frame(
        Group = i,
        N_levels = nrow(unique(re_data[pars])),
        stringsAsFactors = FALSE
      )
    }))
    out <- rbind(out, tmp)
  }

  class(out) <- c("n_grouplevels", "data.frame")
  out
}

# methods ----------------

#' @export
format.n_grouplevels <- function(x, ...) {
  x$N_levels <- format_value(x$N_levels, protect_integers = TRUE)
  format_table(x, ...)
}

#' @export
print.n_grouplevels <- function(x, ...) {
  cat(export_table(format(x, ...), ...))
}
