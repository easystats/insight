#' @keywords internal
.degrees_of_freedom_ml1 <- function(x, ...) {
  UseMethod(".degrees_of_freedom_ml1")
}

#' @keywords internal
.degrees_of_freedom_ml1.default <- function(x, ...) {
  if (!is_mixed_model(x)) {
    format_error("Model must be a mixed model.")
  }

  re_groups <- get_random(x)

  parameters <- find_parameters(x, effects = "fixed")[["conditional"]]
  predictors <- find_predictors(x, effects = "fixed", component = "conditional", flatten = TRUE)
  predictors <- setdiff(predictors, names(re_groups))

  model_data <- get_data(x, verbose = FALSE)[predictors]
  has_intcp <- has_intercept(x)

  term_assignment <- .find_term_assignment(model_data, predictors, parameters)

  ddf <- sapply(model_data, function(.x) {
    min(sapply(re_groups, .get_df_ml1_approx, x = .x))
  })

  ltab <- table(ddf)
  ltab <- list(m = as.integer(names(ltab)), l = as.vector(ltab))

  ltab$ddf <- ltab$m - ltab$l
  if (has_intcp) ltab$ddf <- ltab$ddf - 1

  ii <- match(ddf, ltab$m)
  ddf[] <- ltab$ddf[ii]

  out <- numeric(length = length(parameters))
  ## TODO number of items to replace is not a multiple of replacement length
  suppressWarnings({
    out[which("(Intercept)" != parameters)] <- ddf[term_assignment]
  })
  if (has_intcp) out[which("(Intercept)" == parameters)] <- min(ddf)

  stats::setNames(out, parameters)
}


.get_df_ml1_approx <- function(x, g) {
  if (!is.factor(g)) {
    g <- as.factor(g)
  }
  m <- nlevels(g)
  n <- length(x)
  if (is.character(x)) {
    x <- as.numeric(as.factor(x))
  } else {
    x <- as.numeric(x)
  }
  x.bar <- stats::ave(x, g)
  var.within <- stats::var(x - x.bar)
  var.between <- stats::var(x.bar)
  if (var.within >= var.between) {
    n
  } else {
    m
  }
}


.find_term_assignment <- function(model_data, predictors, parameters) {
  parms <- unlist(lapply(seq_along(predictors), function(i) {
    p <- predictors[i]
    if (is.factor(model_data[[p]])) {
      ps <- paste0(p, levels(model_data[[p]]))
      names(ps)[seq_along(ps)] <- i
      ps
    } else {
      names(p) <- i
      p
    }
  }))
  stats::na.omit(as.numeric(names(parms)[match(clean_names(parameters), parms)]))
}
