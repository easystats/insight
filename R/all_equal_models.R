#' @title Checks if all objects are models of same class
#' @name all_models_equal
#'
#' @description Small helper that checks if all objects are \emph{supported}
#'  (regression) model objects and of same class.
#'
#' @param ... A list of objects.
#' @inheritParams get_variance
#'
#' @return A logical, \code{TRUE} if \code{x} are all supported model objects
#'   of same class.
#'
#' @examples
#' library(lme4)
#' data(mtcars)
#' data(sleepstudy)
#'
#' m1 <- lm(mpg ~ wt + cyl + vs, data = mtcars)
#' m2 <- lm(mpg ~ wt + cyl, data = mtcars)
#' m3 <- lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy)
#' m4 <- glm(formula = vs ~ wt, family = binomial(), data = mtcars)
#'
#' all_models_equal(m1, m2)
#' all_models_equal(m1, m2, m3)
#' all_models_equal(m1, m4, m2, m3, verbose = TRUE)
#'
#' @export
all_models_equal <- function(..., verbose = FALSE) {
  objects <- list(...)
  objects.names <- match.call(expand.dots = FALSE)$`...`

  all_supported <- sapply(objects, is_model)
  all_classes <- sapply(objects, class)
  all_equal <- Reduce(identical, all_classes)

  if (!all_equal && verbose) {
    differ <- which(!duplicated(all_classes))

    m1 <- sprintf(
      "Following objects are not identical with %s (of class \"%s\"): ",
      objects.names[1],
      all_classes[[1]]
    )

    m2 <- paste0(
      sprintf(
        "%s (\"%s\")",
        objects.names[differ[-1]],
        sapply(all_classes[differ[-1]], function(x) as.vector(x[[1]]))
      ),
      collapse = ", "
    )

    message(paste(m1, m2, collapse = " "))
  }

  all_supported && all_equal
}
