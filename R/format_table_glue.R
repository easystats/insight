# this function does the main composition of columns for the output.
.format_glue_table <- function(x,
                               style,
                               coef_column = NULL,
                               p_stars = NULL,
                               ...) {
  # evaluate dots
  dots <- list(...)
  original_x <- x

  # save model information
  info <- attributes(x)$model_info

  # default format
  if (!is.null(dots$format)) {
    format <- dots$format
  } else {
    format <- "text"
  }

  # column name for "pasted" columns
  if (!is.null(dots$new_column_name)) {
    new_column_name <- dots$new_column_name
  } else {
    new_column_name <- NULL
  }

  # line separator
  if (is.null(dots$line_separator)) {
    linesep <- " "
  } else {
    linesep <- dots$line_separator
  }

  # init significant stars
  if ((!is.null(style) && style %in% c("se", "ci")) || is.null(p_stars)) {
    x$p_stars <- ""
  } else {
    x$p_stars <- p_stars
  }

  # potential names for the coefficient column
  coefficient_names <- unique(c(
    easystats_columns("estimate"),
    broom_columns("estimate")
  ))

  # find columns
  if (is.null(coef_column) || !length(coef_column) || !coef_column %in% colnames(x)) {
    coef_column <- intersect(colnames(x), coefficient_names)[1]
  }
  ci_column <- colnames(x)[endsWith(colnames(x), " CI") | colnames(x) == "CI" | colnames(x) == "conf.int"] # nolint
  stat_colum <- colnames(x)[colnames(x) %in% c("t", "z", "Chi2", "Statistic", "statistic") | grepl("^(t\\(|Chi2\\()", colnames(x))] # nolint
  # modelbased
  focal_term_column <- c(
    attributes(original_x)$focal_terms,
    attributes(original_x)$trend,
    attributes(original_x)$contrast
  )

  # make sure we have a glue-like syntax
  style <- .convert_to_glue_syntax(style, linesep, info)

  # we need special handling for ROPE columns. These are sometimes pre-formatted,
  # have the name "% in ROPE" and then won't be removed at the end. So we check
  # if we have a {rope} token, and if not, we also remove "% in ROPE" column later
  special_rope <- any(grepl("{rope}", style, fixed = TRUE))

  # "|" indicates cell split
  style <- unlist(strsplit(style, split = "|", fixed = TRUE))

  # define column names
  if (length(style) == 1 && !is.null(new_column_name)) {
    column_names <- new_column_name
  } else {
    column_names <- .style_pattern_to_name(style, coef_column)
  }

  # paste glue together
  formatted_columns <- compact_list(lapply(seq_along(style), function(i) {
    result <- .format_glue_output(x, coef_column, ci_column, style[i], format, column_names[i])
    # fix invalid columns
    if (all(nzchar(colnames(result)))) {
      result
    } else {
      NULL
    }
  }))
  out <- do.call(cbind, formatted_columns)

  # add new_column_name to column names; for single column layout per model, we just
  # need the column name. If the layout contains more than one column per model,
  # add new_column_name in parenthesis.
  if (!is.null(new_column_name) && nzchar(new_column_name, keepNA = TRUE)) {
    if (ncol(out) > 1) {
      colnames(out) <- paste0(colnames(out), " (", new_column_name, ")")
    } else {
      colnames(out) <- new_column_name
    }
  }

  # remove empty parenthesis
  out[] <- lapply(out, function(i) {
    # here we either have "<br>" or " " as line breaks, followed by empty "()"
    i <- gsub("<br>()", "", i, fixed = TRUE)
    i <- gsub(" ()", "", i, fixed = TRUE)
    i <- gsub("<br>(, )", "", i, fixed = TRUE)
    i <- gsub(" (, )", "", i, fixed = TRUE)
    i[i == "()"] <- ""
    i[i == "(, )"] <- ""
    # remove other non-matched patterns
    i <- gsub("{stars}", "", i, fixed = TRUE)
    i <- gsub("{rhat}", "", i, fixed = TRUE)
    i <- gsub("{ess}", "", i, fixed = TRUE)
    i <- gsub("{pd}", "", i, fixed = TRUE)
    i <- gsub("{rope}", "", i, fixed = TRUE)
    i
  })

  # define columns that should be removed
  p_column <- unique(c(easystats_columns("p"), broom_columns("p")))
  df_column <- unique(c(easystats_columns("df"), broom_columns("df")))
  uncertainty_column <- unique(c(
    easystats_columns("uncertainty"),
    broom_columns("uncertainty"),
    ci_column
  ))
  # add special rope column?
  if (!special_rope) {
    p_column <- unique(c(p_column, "ROPE", "% in ROPE"))
  }

  # bind glue-columns to original data, but remove former columns first
  original_x[c(coefficient_names, uncertainty_column, stat_colum, p_column, df_column)] <- NULL # nolint

  # reorder
  original_x <- standardize_column_order(original_x)

  # we want: first parameter, then glue-columns, then remaining columns
  parameter_column <- intersect(
    c(
      easystats_columns("parameter"),
      broom_columns("parameter"),
      focal_term_column
    ),
    colnames(original_x)
  )
  non_parameter_column <- setdiff(colnames(original_x), parameter_column)

  cbind(original_x[parameter_column], out, original_x[non_parameter_column])
}


# this function handles short cuts for "select" and creates the related
# glue patterns
.convert_to_glue_syntax <- function(style, linesep = NULL, info = NULL) {
  # set default
  if (is.null(linesep)) {
    linesep <- " "
  }

  # for Bayesian models, we have pd instead p
  p_column <- ifelse(isTRUE(info$is_bayesian), "{pd}", "{p}")

  # default
  if (is.null(style)) {
    style <- paste0("{estimate}", linesep, "({ci})|", p_column)

    # style: estimate and CI, p-value in separate column (currently identical to "ci_p2")
  } else if (style %in% c("minimal", "ci_p2")) {
    style <- paste0("{estimate}", linesep, "({ci})|", p_column)

    # style: estimate and CI, no p
  } else if (style == "ci") {
    style <- paste0("{estimate}", linesep, "({ci})")

    # style: estimate, p-stars and CI
  } else if (style == "ci_p") {
    style <- paste0("{estimate}{stars}", linesep, "({ci})")

    # style: estimate and SE, no p
  } else if (style == "se") {
    style <- paste0("{estimate}", linesep, "({se})")

    # style: estimate, p-stars and SE
  } else if (style == "se_p") {
    style <- paste0("{estimate}{stars}", linesep, "({se})")

    # style: estimate and SE, p-value in separate column
  } else if (style %in% c("short", "se_p2")) {
    style <- paste0("{estimate}", linesep, "({se})|", p_column)

    # style: only estimate
  } else if (style %in% c("est", "coef")) {
    style <- "{estimate}"
  }

  # replace \n for now with default line-separators
  gsub("\n", linesep, style, fixed = TRUE)
}


.format_glue_output <- function(x, coef_column, ci_column, style, format, column_names) {
  # check whether we have broom styled or easystats styled columns
  se_column <- ifelse("SE" %in% colnames(x), "SE", "std.error")
  p_column <- ifelse("p" %in% colnames(x), "p", "p.value")
  rope_column <- ifelse("ROPE_Percentage" %in% colnames(x), "ROPE_Percentage", "rope.percentage")
  ess_column <- ifelse("ESS" %in% colnames(x), "ESS", "ess")
  rhat_column <- ifelse("Rhat" %in% colnames(x), "Rhat", "rhat")

  # separate CI columns, for custom layout
  ci <- ci_low <- ci_high <- NULL
  if (!insight::is_empty_object(ci_column)) {
    ci <- x[[ci_column[1]]]
    ci_low <- insight::trim_ws(gsub("(\\(|\\[)(.*),(.*)(\\)|\\])", "\\2", ci))
    ci_high <- insight::trim_ws(gsub("(\\(|\\[)(.*),(.*)(\\)|\\])", "\\3", ci))
  }

  # fix p-layout
  if (p_column %in% colnames(x)) {
    x[[p_column]] <- insight::trim_ws(x[[p_column]])
    x[[p_column]] <- gsub("< .", "<0.", x[[p_column]], fixed = TRUE)
  }

  # handle aliases
  style <- tolower(style)
  style <- gsub("{coef}", "{estimate}", style, fixed = TRUE)
  style <- gsub("{coefficient}", "{estimate}", style, fixed = TRUE)
  style <- gsub("{std.error}", "{se}", style, fixed = TRUE)
  style <- gsub("{standard error}", "{se}", style, fixed = TRUE)
  style <- gsub("{pval}", "{p}", style, fixed = TRUE)
  style <- gsub("{p.value}", "{p}", style, fixed = TRUE)
  style <- gsub("{ci}", "{ci_low}, {ci_high}", style, fixed = TRUE)

  # we put all elements (coefficient, SE, CI, p, ...) in one column.
  # for text format, where columns are not center aligned, this can result in
  # misaligned columns, which looks ugly. So we try to ensure that each element
  # is formatted and justified to the same width
  if (identical(format, "text") || is.null(format)) {
    x[[coef_column]] <- .align_values(x[[coef_column]])
    x[[se_column]] <- .align_values(x[[se_column]])
    x[[p_column]] <- .align_values(x[[p_column]])
    x$p_stars <- .align_values(x$p_stars)
    ci_low <- .align_values(ci_low)
    ci_high <- .align_values(ci_high)
    x$pd <- .align_values(x$pd)
    x[[rhat_column]] <- .align_values(x[[rhat_column]])
    x[[ess_column]] <- .align_values(x[[ess_column]])
    x[[rope_column]] <- .align_values(x[[rope_column]])
  }

  # remove non-existent patterns from style
  style <- .clean_style_pattern(
    x,
    style,
    ci_low,
    ci_high,
    se_column,
    p_column,
    rhat_column,
    ess_column,
    rope_column
  )

  # replace glue-tokens with columns
  final <- .replace_style_with_column(
    x,
    style,
    ci_low,
    ci_high,
    coef_column,
    se_column,
    p_column,
    rhat_column,
    ess_column,
    rope_column
  )

  # final output
  x <- data.frame(final)
  colnames(x) <- column_names

  # sanity check - does column exist?
  non_existent <- vapply(x, function(i) all(i == style), logical(1))
  x[non_existent] <- NULL

  x
}


# align columns width for text format
.align_values <- function(i) {
  if (!is.null(i)) {
    non_empty <- !is.na(i) & nzchar(i, keepNA = TRUE)
    i[non_empty] <- format(insight::trim_ws(i[non_empty]), justify = "right")
  }
  i
}


# this function checks if the glue-columns actually exist in the data
# and if not, these are being removed from the glue-pattern. we need
# this to avoid columns that contain glue-tokens instead of values
.clean_style_pattern <- function(x,
                                 style,
                                 ci_low,
                                 ci_high,
                                 se_column,
                                 p_column,
                                 rhat_column,
                                 ess_column,
                                 rope_column) {
  if (!p_column %in% colnames(x) && any(grepl("(\\{p\\}|\\{stars\\})", style))) {
    style <- gsub("(\\{p\\}|\\{stars\\})", "", style)
  }
  if (!se_column %in% colnames(x) && any(grepl("{se}", style, fixed = TRUE))) {
    style <- gsub("{se}", "", style, fixed = TRUE)
  }
  if (!rhat_column %in% colnames(x) && any(grepl("{rhat}", style, fixed = TRUE))) {
    style <- gsub("{rhat}", "", style, fixed = TRUE)
  }
  if (!rope_column %in% colnames(x) && any(grepl("{rope}", style, fixed = TRUE))) {
    style <- gsub("{rope}", "", style, fixed = TRUE)
  }
  if (!ess_column %in% colnames(x) && any(grepl("{ess}", style, fixed = TRUE))) {
    style <- gsub("{ess}", "", style, fixed = TRUE)
  }
  if (!"pd" %in% colnames(x) && any(grepl("{pd}", style, fixed = TRUE))) {
    style <- gsub("{pd}", "", style, fixed = TRUE)
  }
  if (is.null(ci_low) && is.null(ci_high) && any(grepl("{ci_low}, {ci_high}", style, fixed = TRUE))) {
    style <- gsub("{ci_low}, {ci_high}", "", style, fixed = TRUE)
  }

  compact_character(style)
}


# we create a data frame with dummy-content, where each column
# contains the style pattern. here, we replace the style pattern
# with the related content from the original data frame.
.replace_style_with_column <- function(x,
                                       style,
                                       ci_low,
                                       ci_high,
                                       coef_column,
                                       se_column,
                                       p_column,
                                       rhat_column,
                                       ess_column,
                                       rope_column) {
  # create new string
  table_row <- rep(style, times = nrow(x))
  for (r in seq_along(table_row)) {
    table_row[r] <- gsub("{estimate}", x[[coef_column]][r], table_row[r], fixed = TRUE)
    if (!is.null(ci_low) && !is.null(ci_high)) {
      table_row[r] <- gsub("{ci_low}", ci_low[r], table_row[r], fixed = TRUE)
      table_row[r] <- gsub("{ci_high}", ci_high[r], table_row[r], fixed = TRUE)
    }
    if (se_column %in% colnames(x)) {
      table_row[r] <- gsub("{se}", x[[se_column]][r], table_row[r], fixed = TRUE)
    }
    if (p_column %in% colnames(x)) {
      table_row[r] <- gsub("{p}", x[[p_column]][r], table_row[r], fixed = TRUE)
    }
    if ("p_stars" %in% colnames(x)) {
      table_row[r] <- gsub("{stars}", x[["p_stars"]][r], table_row[r], fixed = TRUE)
    }
    if ("pd" %in% colnames(x)) {
      table_row[r] <- gsub("{pd}", x[["pd"]][r], table_row[r], fixed = TRUE)
    }
    if (rhat_column %in% colnames(x)) {
      table_row[r] <- gsub("{rhat}", x[[rhat_column]][r], table_row[r], fixed = TRUE)
    }
    if (ess_column %in% colnames(x)) {
      table_row[r] <- gsub("{ess}", x[[ess_column]][r], table_row[r], fixed = TRUE)
    }
    if (rope_column %in% colnames(x)) {
      table_row[r] <- gsub("{rope}", x[[rope_column]][r], table_row[r], fixed = TRUE)
    }
  }

  # some cleaning: columns w/o coefficient are empty
  table_row[x[[coef_column]] == "" | is.na(x[[coef_column]])] <- "" # nolint

  # fix some p-value stuff, e.g. if pattern is "p={p]}",
  # we may have "p= <0.001", which we want to be "p<0.001"
  table_row <- gsub("=<", "<", table_row, fixed = TRUE)
  table_row <- gsub("= <", "<", table_row, fixed = TRUE)
  table_row <- gsub("= ", "=", table_row, fixed = TRUE)

  table_row
}


.style_pattern_to_name <- function(style, coef_column) {
  if (is.null(coef_column)) {
    coef_column <- "Estimate"
  }
  column_names <- tolower(style)
  # completely remove these patterns
  column_names <- gsub("{stars}", "", column_names, fixed = TRUE)
  # remove curlys
  column_names <- gsub("{", "", column_names, fixed = TRUE)
  column_names <- gsub("}", "", column_names, fixed = TRUE)
  # manual renaming
  column_names <- gsub("\\Qrope\\E", "% in ROPE", column_names)
  column_names <- gsub("(estimate|coefficient|coef)", coef_column, column_names)
  column_names <- gsub("\\Qse\\E", "SE", column_names)
  column_names <- gsub("\\<ci\\>", "CI", column_names)
  column_names <- gsub("<br>", "", column_names, fixed = TRUE)
  column_names
}
