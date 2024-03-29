% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hdi.R
\name{hdi_of_icdf}
\alias{hdi_of_icdf}
\title{Bind the weighted log odds to a tidy dataset}
\source{
\url{https://bookdown.org/content/3686/goals-power-and-sample-size.html#computing-power-and-sample-size/}
}
\usage{
hdi_of_icdf(name, width = 0.95, tol = 1e-08, ...)
}
\arguments{
\item{name}{Distribution name}

\item{width}{Width of HDI}

\item{tol}{Tolerance to pass to \code{optimize}}

\item{...}{parameters to pass for particular distribution}
}
\description{
Calculate HDI range from any common distribution
}
\details{
Function was derived by Solomon Kurz's bookdown
converting Kruschke's book into \link{brms} and \link{tidyverse}
}
