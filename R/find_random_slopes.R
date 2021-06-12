#' @title Find names of random slopes
#' @name find_random_slopes
#'
#' @description Return the name of the random slopes from mixed effects models.
#'
#' @param x A fitted mixed model.
#'
#' @return A list of character vectors with the name(s) of the random slopes, or
#'    \code{NULL} if model has no random slopes. Depending on the model, the
#'    returned list has following elements:
#'    \itemize{
#'      \item \code{random}, the random slopes from the conditional part of model
#'      \item \code{zero_inflated_random}, the random slopes from the
#'      zero-inflation component of the model
#'    }
#'
#' @examples
#' if (require("lme4")) {
#'   data(sleepstudy)
#'   m <- lmer(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)
#'   find_random_slopes(m)
#' }
#' @export
find_random_slopes <- function(x) {
  random_slopes <- vector(mode = "list")
  forms <- find_formula(x, verbose = FALSE)

  random_slopes$random <- .extract_random_slopes(forms$random)
  random_slopes$zero_inflated_random <- .extract_random_slopes(forms$zero_inflated_random)

  random_slopes <- .compact_list(random_slopes)

  if (.is_empty_object(random_slopes)) {
    NULL
  } else {
    random_slopes
  }
}


.extract_random_slopes <- function(fr) {
  if (is.null(fr)) {
    return(NULL)
  }

  if (!is.list(fr)) fr <- list(fr)

  random_slope <- lapply(fr, function(forms) {
    if (grepl("(.*)\\|(.*)\\|(.*)", .safe_deparse(forms))) {
      pattern <- "(.*)\\|(.*)\\|(.*)"
    } else {
      pattern <- "(.*)\\|(.*)"
    }
    pattern <- gsub(pattern, "\\1", .safe_deparse(forms))
    re <- all.vars(forms)
    re[sapply(re, function(x) {
      grepl(x, pattern, fixed = TRUE)
    })]
  })

  unique(unlist(.compact_list(random_slope)))
}
