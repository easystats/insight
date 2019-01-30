#' @rdname pred_vars
#' @importFrom nlme getResponse
#' @export
resp_val <- function(x) {

  rn <- resp_var(x, combine = FALSE)

  if (inherits(x, c("lme", "gls")))
    as.vector(nlme::getResponse(x))
  else if (inherits(x, "brmsfit")) {
    rv <- model_frame(x)[, clean_names(resp_var(x))]
    rc <- ncol(rv)
    if (!is.null(stats::formula(x)$responses) || !is.null(rc))
      as.vector(rv)
    else
      rv
  } else if (inherits(x, "stanmvreg"))
    as.vector(model_frame(x)[, clean_names(resp_var(x))])
  else if (length(rn) > 1) {
    rv <- as.data.frame(model_frame(x)[[clean_names(resp_var(x))]])
    colnames(rv) <- rn
    rv
  } else
    as.vector(model_frame(x)[[clean_names(resp_var(x))]])
}
