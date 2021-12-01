\name{PCA}
\alias{PCA}

\title{Principal Component Analysis
}

\description{
Computes a Principal Component Analysis when both when p>n and when p<=n.
}

\usage{
PCA(X)
}

\arguments{
  \item{X}{Matrix or data.frame with variables in columns and observations in rows.}
}

\value{
  \item{eigen}{A eigen class object with eigenvalues and eigenvectors of the analysis.}
  \item{var.exp}{A matrix containing the explained variance for each component and the cumulative variance.}
  \item{scores}{Scores of the PCA analysis.}
  \item{loadings}{Loadings of the PCA analysis.}
  }

\author{Maria Jose Nueda, \email{mj.nueda@ua.es}}

\examples{
## Simulate data matrix with 500 variables and 10 observations
datasim = matrix(sample(0:100, 5000, replace = TRUE), nrow = 10)
## PCA
myPCA = PCA(datasim)
## Extracting the variance explained by each principal component
myPCA$var.exp
}
