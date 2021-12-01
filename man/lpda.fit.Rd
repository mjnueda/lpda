\name{lpda.fit}
\alias{lpda.fit}

\title{lpda.fit computes the discriminating hyperplane for two groups}

\description{
\code{lpda.fit} computes the discriminating hyperplane for two groups, giving as a result the coefficients of the hyperplane.
}

\usage{
lpda.fit(data, group, f1 = NULL, f2 = NULL)
}

\arguments{
\item{data}{Matrix containing data. Individuals in rows and variables in columns}

\item{group}{Vector with the variable group}

\item{f1}{Vector with weights for individuals of the first group}

\item{f2}{Vector with weights for individuals of the second group}
}

\author{Maria Jose Nueda, \email{mj.nueda@ua.es}}

\seealso{ \code{\link{lpda}}}

\keyword{classification}


