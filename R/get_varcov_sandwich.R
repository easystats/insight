# Get Variance-covariance Matrix ---------------------------------------------------

.get_varcov_sandwich <- function(x,
                                 vcov_fun = NULL,
                                 vcov_args = NULL,
                                 verbose = TRUE,
                                 ...) {
  dots <- list(...)

  # deprecated
  if (isTRUE(verbose) && "vcov_type" %in% names(dots)) {
    warning(format_message("The `vcov_type` argument is superseded by the `vcov_args` argument."), call. = FALSE)
  }
  if (isTRUE(verbose) && "robust" %in% names(dots)) {
    warning(format_message("The `robust` argument is superseded by the `vcov` argument."), call. = FALSE)
  }

  if (is.null(vcov_args)) {
    vcov_args <- list()
  }

  # deprecated: `vcov_estimation`
  if (is.null(vcov_fun) && "vcov_estimation" %in% names(dots)) {
    vcov_fun <- dots[["vcov_estimation"]]
  }

  # deprecated: `robust`
  if (isTRUE(dots[["robust"]]) && is.null(vcov_fun)) {
    dots[["robust"]] <- NULL
    vcov_fun <- "HC3"
  }

  # deprecated: `vcov_type`
  if ("vcov_type" %in% names(dots)) {
    if (!"type" %in% names(vcov_args)) {
      vcov_args[["type"]] <- dots[["vcov_type"]]
    }
  }

  # vcov_fun is a matrix
  if (is.matrix(vcov_fun)) {
    return(vcov_fun)
  }

  # vcov_fun is a function
  if (is.function(vcov_fun)) {
    if (is.null(vcov_args) || !is.list(vcov_args)) {
      args <- list(x)
    } else {
      args <- c(list(x), vcov_args)
    }
    .vcov <- do.call("vcov_fun", args)
    return(.vcov)
  }

  # type shortcuts: overwrite only if not supplied explicitly by the user
  if (!"type" %in% names(vcov_args)) {
    if (isTRUE(vcov_fun %in% c(
      "HC0", "HC1", "HC2", "HC3", "HC4", "HC4m", "HC5",
      "CR0", "CR1", "CR1p", "CR1S", "CR2", "CR3", "xy",
      "residual", "wild", "mammen", "webb"
    ))) {
      vcov_args[["type"]] <- vcov_fun
    }
  }

  # default vcov matrix
  if (is.null(vcov_fun)) {
    .vcov <- get_varcov(x, ...)
    return(.vcov)
  }

  if (!grepl("^(vcov|kernHAC|NeweyWest)", vcov_fun)) {
    vcov_fun <- switch(vcov_fun,
      "HC0" = ,
      "HC1" = ,
      "HC2" = ,
      "HC3" = ,
      "HC4" = ,
      "HC4m" = ,
      "HC5" = ,
      "HC" = "vcovHC",
      "CR0" = ,
      "CR1" = ,
      "CR1p" = ,
      "CR1S" = ,
      "CR2" = ,
      "CR3" = ,
      "CR" = "vcovCR",
      "xy" = ,
      "residual" = ,
      "wild" = ,
      "mammen" = ,
      "webb" = ,
      "BS" = "vcovBS",
      "OPG" = "vcovOPG",
      "HAC" = "vcovHAC",
      "PC" = "vcovPC",
      "CL" = "vcovCL",
      "PL" = "vcovPL"
    )
  }

  # check if required package is available
  if (vcov_fun == "vcovCR") {
    check_if_installed("clubSandwich", reason = "to get cluster-robust standard errors")
    fun <- try(get(vcov_fun, asNamespace("clubSandwich")), silent = TRUE)
  } else {
    check_if_installed("sandwich", reason = "to get robust standard errors")
    fun <- try(get(vcov_fun, asNamespace("sandwich")), silent = TRUE)
    if (!is.function(fun)) {
      stop(sprintf("`%s` is not a function exported by the `sandwich` package.", vcov_fun))
    }
  }

  .vcov <- try(do.call(fun, c(list(x), vcov_args)), silent = TRUE)

  # clubSandwich output has a weird class
  if (inherits(.vcov, c("vcovCR", "clubSandwich"))) {
    .vcov <- as.matrix(.vcov)
  }

  # extract variance-covariance matrix
  if (!inherits(.vcov, "matrix")) {
    msg <- sprintf("Unable to extract a variance-covariance matrix for model object of class `%s`. Different values of the `vcov` argument trigger calls to the `sandwich` or `clubSandwich` packages in order to extract the matrix (see `?insight::get_varcov`). Your model or the requested estimation type may not be supported by one or both of those packages.", class(x)[1])
    stop(format_message(msg), call. = FALSE)
  }

  .vcov
}


