#' @keywords internal
.degrees_of_freedom_betwithin <- function(x, ...) {
  UseMethod(".degrees_of_freedom_betwithin")
}

#' @keywords internal
.degrees_of_freedom_betwithin.default <- function(x, ...) {
  if (!is_mixed_model(x)) {
    format_error("Model must be a mixed model.")
  }

  ngrps <- sum(.n_randomeffects(x))
  parameters <- find_parameters(x, effects = "fixed")[["conditional"]]
  within_effects <- unlist(find_random_slopes(x))
  has_intcp <- has_intercept(x)

  ddf_within <- ngrps - n_parameters(x)
  ddf_between <- n_obs(x, disaggregate = TRUE) - ngrps - n_parameters(x)

  if (has_intcp) {
    ddf_between <- ddf_between - 1
    ddf_within <- ddf_within - 1
  }

  within_index <- match(within_effects, parameters)
  ddf <- stats::setNames(seq_along(parameters), parameters)

  if (length(within_index) > 0) {
    ddf[match(within_effects, parameters)] <- ddf_within
    ddf[-match(within_effects, parameters)] <- ddf_between
  } else {
    ddf <- ddf_between
  }

  ddf
}


.n_randomeffects <- function(x) {
  vapply(
    get_data(x, verbose = FALSE)[find_random(x, split_nested = TRUE, flatten = TRUE)],
    n_unique,
    numeric(1)
  )
}
