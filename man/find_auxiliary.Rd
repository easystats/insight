% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/find_auxiliary.R
\name{find_auxiliary}
\alias{find_auxiliary}
\title{Find auxiliary (distributional) parameters from models}
\usage{
find_auxiliary(x, ...)
}
\arguments{
\item{x}{A model of class \code{brmsfit}.}

\item{...}{Currently not used.}
}
\value{
The names of all available auxiliary parameters used in the model.
}
\description{
Returns the names of all auxiliary / distributional parameters
from brms-models, like dispersion, sigma, kappa, phi, or beta...
}
