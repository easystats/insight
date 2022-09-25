#' @keywords internal
.degrees_of_freedom_satterthwaite <- function(x, ...) {
  UseMethod(".degrees_of_freedom_satterthwaite")
}

#' @keywords internal
.degrees_of_freedom_satterthwaite.lmerMod <- function(x, ...) {
  check_if_installed("lmerTest")

  parameters <- find_parameters(x, effects = "fixed", flatten = TRUE)
  lmerTest_model <- lmerTest::as_lmerModLmerTest(x)
  s <- summary(lmerTest_model)

  stats::setNames(as.vector(s$coefficients[, 3]), parameters)
}

#' @keywords internal
.degrees_of_freedom_satterthwaite.lme <- function(x, ...) {
  check_if_installed("lavaSearch2")

  parameters <- find_parameters(x, effects = "fixed", flatten = TRUE)
  lavaSearch2::sCorrect(x) <- TRUE
  s <- lavaSearch2::summary2(x)
  stats::setNames(as.vector(s$tTable[, "df"]), parameters)
}
