\name{plot.lpda}
\alias{plot.lpda}

\title{Plot method for lpda classification}

\description{
\code{plot.lpda} is applied to an \code{lpda} class object. It shows a plot
in two dimensions with the distances to the computed hyperplane of each individual coloring each case with the real class.
}

\usage{
\method{plot}{lpda}(x, PCscores = FALSE, xlim = NULL, main = NULL,
legend.pos = "topright", ...)
}

\arguments{
\item{x}{Object of class inheriting from "lpda"}

\item{PCscores}{Logical to show the first 2 PCscores. Only possible when PCA is applied.}

\item{xlim}{An optional vector with two values with the x-axis range. If omitted, it will be computed.}

\item{main}{An optional title for the plot.}

\item{legend.pos}{The position for the legend. By default it is topright. NULL when no legend is required.}

\item{...}{Other arguments passed.}
}

\author{Maria Jose Nueda, \email{mj.nueda@ua.es}}


\seealso{ \code{\link{lpda}}}

\keyword{classification}
