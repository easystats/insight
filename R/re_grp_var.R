#' @importFrom lme4 findbars
#' @importFrom stats formula
re_terms <- function(x) {
  tryCatch({
    if (inherits(x, "brmsfit"))
      f <- stats::formula(x)[[1]]
    if (inherits(x, "MixMod"))
      return(x$id_name)
    else
      f <- stats::formula(x)

    re <- sapply(lme4::findbars(f), deparse)
    trim(substring(re, regexpr(pattern = "\\|", re) + 1))
  },
  error = function(x) { NULL }
  )
}
