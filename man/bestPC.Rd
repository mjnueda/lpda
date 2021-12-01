\name{bestPC}
\alias{bestPC}

\title{Choosing the best number of Principal Components (PCs) for lpda-pca model. }

\description{
\code{bestPC} computes the classification error for lpda.pca models applied with the number of components specified in PCs argument. The result is the average classification error rate from the R models computed for each number of PCs.
}

\usage{
bestPC(data, group, ntest = 10, R = 10, PCs = c(10,15,20), f1 = NULL, f2 = NULL)
}

\arguments{
\item{data}{Matrix containing data. Individuals in rows and variables in columns}

\item{group}{Vector with the variable group}

\item{ntest}{Number of samples to evaluate in the test-set.}

\item{R}{Times the model is evaluated with each Variability indicated in Vars vector.}

\item{PCs}{The PCs to check.}

\item{f1}{Vector with weights for individuals of the first group. If NULL they are equally weighted.}

\item{f2}{Vector with weights for individuals of the second group. If NULL they are equally weighted.}
}

\value{

\code{bestPC} returns a vector with the average prediction error rate obtained from the R models for each PC specified in PCs input.
  }

\author{Maria Jose Nueda, \email{mj.nueda@ua.es}}

\examples{
  data(RNAseq)
  group = as.factor(rep(c("G1","G2"), each = 30))
  bestPC(RNAseq, group, ntest = 10, R = 5, PCs = c(2, 10))
  }

\seealso{ \code{\link{lpda}}}

\keyword{classification}
