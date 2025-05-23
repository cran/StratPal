% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/discrete_gradient.R
\name{discrete_gradient}
\alias{discrete_gradient}
\title{construct discretized gradient}
\usage{
discrete_gradient(vals, bounds, outval = "")
}
\arguments{
\item{vals}{vector, values of the gradient}

\item{bounds}{vector of strictly increasing values, e.g. times or stratigraphic heights.}

\item{outval}{value, gradient value assigned outside of values covered by \code{bounds}}
}
\value{
a functions assigning continuous values (e.g., times or heights) discrete niches
}
\description{
Constructs a discretized gradient along time/height. The gradient value between
\code{bounds[i]} and \code{bounds[i+1]} (not including) is \code{vals[i]}, values above/below the largest/smallest value of \code{bounds} are assigned \code{outval}.
Helper function for usage with \code{discrete_niche}
}
\examples{
# see examples in `discrete_niche` for a use case
# and examples in `apply_niche` for the general application to different data types
# or the vignette on event data for more context

}
\seealso{
\itemize{
\item \code{\link[=discrete_niche]{discrete_niche()}} to construct niches based on discretized gradients
\item \code{\link[=apply_niche]{apply_niche()}} to combine \code{\link[=discrete_niche]{discrete_niche()}} and \code{\link[=discrete_gradient]{discrete_gradient()}} to model the effects of niches
\item \code{vignette("advanced_functionality")} for details how to create user-defined niche models
}
}
