#' @title Get statistic associated with estimates
#' @description Returns the statistic (\emph{t}, \code{z}, ...) for model estimates.
#'   In most cases, this is the related column from \code{coef(summary())}.
#' @name get_statistic
#'
#' @description Small helper that checks if a model is a regression model
#'   object and return the statistic used.
#'
#' @param x A model.
#' @param column_index For model objects that have no defined \code{get_statistic()}
#'   method yet, the default method is called. This method tries to extract the
#'   statistic column from \code{coef(summary())}, where the index of the column
#'   that is being pulled is \code{column_index}. Defaults to 3, which is the
#'   default statistic column for most models' summary-output.
#' @param component Should all parameters, parameters for the conditional model,
#'   or for the zero-inflated part of the model be returned? Applies to models
#'   with zero-inflated component. \code{component} may be one of
#'   \code{"conditional"}, \code{"zi"}, \code{"zero-inflated"} or \code{"all"}
#'   (default). May be abbreviated.
#' @param ... Currently not used.
#'
#' @return
#'
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
#' get_statistic(m)
#' @export
get_statistic <- function(x, ...) {
  UseMethod("get_statistic")
}


#' @rdname get_statistic
#' @export
get_statistic.default <- function(x, column_index = 3, ...) {
  cs <- stats::coef(summary(x))
  cs_names <- dimnames(cs)[[2]]

  out <- data.frame(
    Parameter = gsub("`", "", rownames(cs), fixed = TRUE),
    Statistic = as.vector(cs[, column_index]),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  if (any(c("t val.", "t", "t-value", "t.value", "tvalue") %in% cs_names))
    attr(out, "statistic") <- "t"
  else if (any(c("z val.", "z", "z-value", "z.value", "z value") %in% cs_names))
    attr(out, "statistic") <- "z"
  else
    attr(out, "statistic") <- "statistic"

  out
}



#' @importFrom stats coef
#' @rdname get_statistic
#' @export
get_statistic.glmmTMB <- function(model, component = c("all", "conditional", "zi", "zero_inflated"), ...) {
  component <- match.arg(component)

  cs <- compact_list(stats::coef(summary(model)))
  x <- lapply(names(cs), function(i) {
    data.frame(
      Parameter = find_parameters(model, effects = "fixed", component = i, flatten = TRUE),
      Statistic = as.vector(cs[[i]][, 3]),
      Component = i,
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  })

  stat <- do.call(rbind, x)
  stat$Component <- .rename_values(stat$Component, "cond", "conditional")
  stat$Component <- .rename_values(stat$Component, "zi", "zero_inflated")

  stat <- .filter_component(stat, component)
  attr(stat, "statistic") <- "z"

  stat
}



.get_statistic.zeroinfl <- function(model, component = c("all", "conditional", "zi", "zero_inflated"), ...) {
  component <- match.arg(component)

  cs <- .compact_list(stats::coef(summary(model)))
  x <- lapply(names(cs), function(i) {
    comp <- ifelse(i == "count", "conditional", "zi")
    data_frame(
      Parameter = insight::find_parameters(model, effects = "fixed", component = comp, flatten = TRUE),
      Statistic = as.vector(cs[[i]][, 3]),
      Component = comp
    )
  })

  stat <- do.call(rbind, x)
  stat$Component <- .rename_values(stat$Component, "cond", "conditional")
  stat$Component <- .rename_values(stat$Component, "zi", "zero_inflated")

  stat <- .filter_component(stat, component)
  attr(stat, "statistic") <- "z"

  stat
}

.get_statistic.hurdle <- .get_statistic.zeroinfl

.get_statistic.zerocount <- .get_statistic.zeroinfl



.get_statistic.MixMod <- function(model, component = c("all", "conditional", "zi", "zero_inflated"), ...) {
  component <- match.arg(component)
  s <- summary(model)
  cs <- list(s$coef_table, s$coef_table_zi)
  names(cs) <- c("conditional", "zero_inflated")
  cs <- .compact_list(cs)
  x <- lapply(names(cs), function(i) {
    data_frame(
      Parameter = insight::find_parameters(model, effects = "fixed", component = i, flatten = TRUE),
      Statistic = as.vector(cs[[i]][, 3]),
      Component = i
    )
  })

  stat <- do.call(rbind, x)
  .filter_component(stat, component)
  attr(stat, "statistic") <- "z"

  stat
}



.get_statistic.gam <- function(model, statistic_column = 3, ...) {
  cs <- summary(model)$p.table
  cs.smooth <- summary(model)$s.table

  out <- data_frame(
    Parameter = c(rownames(cs), rownames(cs.smooth)),
    Statistic = c(as.vector(cs[, statistic_column]), as.vector(cs.smooth[, statistic_column])),
    Component = c(rep("conditional", nrow(cs)), rep("smooth_terms", nrow(cs.smooth)))
  )

  attr(out, "statistic") <- "t / F"
  out
}



.get_statistic.gamlss <- function(model, statistic_column = 3, ...) {
  parms <- insight::get_parameters(model)
  utils::capture.output(cs <- summary(model))

  out <- data_frame(
    Parameter = parms$parameter,
    Statistic = as.vector(cs[, statistic_column]),
    Component = parms$component
  )

  attr(out, "statistic") <- "t"
  out
}



.get_statistic.lme <- function(model, ...) {
  .get_statistic.default(model, statistic_column = 4)
}


.get_statistic.plm <- .get_statistic.default

.get_statistic.feis <- .get_statistic.default


.get_statistic.coxph <- function(model, ...) {
  .get_statistic.default(model, statistic_column = 4)
}


.get_statistic.svyglm.nb <- function(model, ...) {
  parms <- insight::get_parameters(model)
  se <- standard_error(model)

  out <- data_frame(
    Parameter = parms$parameter,
    Statistic = parms$estimate / se$SE
  )

  attr(out, "statistic") <- "t"
  out
}


.get_statistic.svyglm.zip <- .get_statistic.svyglm.nb