\name{CVloo}
\alias{CVloo}

\title{CVloo evaluates the error rate classification with leave one out procedure}

\description{
\code{CVloo} evaluates the error rate classification with leave one out procedure.
}

\usage{
CVloo(data, group, scale = FALSE, pca = FALSE, PC = 2,
                  Variability = NULL, f1 = NULL, f2 = NULL)
}

\arguments{
\item{data}{Matrix containing data. Individuals in rows and variables in columns.}

\item{group}{Vector with the variable group.}

\item{scale}{Logical indicating if data is standarised.}

\item{pca}{Logical indicating if a reduction of dimension is required.}

\item{PC}{Number of Principal Components (PC) for PCA. By default it is 2. When the number of PC is not decided, it can be determined choosing the desired proportion of explained variability (Variability parameter) or choosing the maximum number of errors allowed in the training set (Error.max).}

\item{Variability}{Parameter for Principal Components (PC) selection. This is the desired
    proportion of variability explained for the PC of the variables.}

\item{f1}{Vector with weights for individuals of the first group. If NULL they are equally weighted.}

\item{f2}{Vector with weights for individuals of the second group. If NULL they are equally weighted.}
}

\value{

\code{CVloo} The prediction error rate.
  }

\author{Maria Jose Nueda, \email{mj.nueda@ua.es}}


\seealso{ \code{\link{lpdaCV}} }

