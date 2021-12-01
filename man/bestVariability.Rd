\name{bestVariability}
\alias{bestVariability}

\title{Choosing the best explained variability for lpda-pca model. }

\description{
\code{bestVariability} computes the classification error for lpda.pca models obtained with the number of components needed to reach the  explained variability specified in `Vars` argument. The result is the average classification error rate from the R models computed for each explained variability.
}


\usage{
bestVariability(data, group, ntest = 10, R = 10, Vars = c(0.5,0.7), f1 = NULL, f2 = NULL)
}

\arguments{
\item{data}{Matrix containing data. Individuals in rows and variables in columns}

\item{group}{Vector with the variable group}

\item{ntest}{Number of samples to evaluate in the test-set.}

\item{R}{Times the model is evaluated with each Variability indicated in Vars vector.}

\item{Vars}{The different variabilities to check from which the best variability parameter will be chosen for lpdapca model.}

\item{f1}{Vector with weights for individuals of the first group. If NULL they are equally weighted.}

\item{f2}{Vector with weights for individuals of the second group. If NULL they are equally weighted.}
}

\value{

\code{bestVar} returns a vector with the average prediction error rate obtained from the R models for each variability specified in Vars input.
  }

\author{Maria Jose Nueda, \email{mj.nueda@ua.es}}

\examples{
  data(RNAseq)
  group = as.factor(rep(c("G1","G2"),each=30))
  bestVariability(RNAseq, group, ntest = 10, R = 5, Vars = c(0.1,0.9))
  }

\seealso{ \code{\link{lpda}}}

\keyword{classification}
