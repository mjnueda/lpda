PCA <- function(X)
{
  # PCA computes Principal Components Analysis both when p>n and when p<=n
  # R can not apply princomp when p>n and eigen(t(X)%*%X) can not be computed.
  # First X matrix is centered by columns (Xoff)
  # Then eigenvalues and eigenvectors of Xoff%*%t(Xoff) are computed
  # Equivalences between the loadings and scores are used to obtain the solution
  # scores1 & loadings1 is the output and scores2 & loadings2 intermediate results


  p <- ncol(X)
  n <- nrow(X)
  Xc <- center(X)

  if(p<n){
    eigen <- eigen(t(Xc)%*%Xc/n)
    var <- cbind(eigen$values/sum(eigen$values),cumsum(eigen$values/sum(eigen$values)))
    loadings1 <- eigen$vectors
    scores1 <- Xc%*%loadings1
  }else{
    eigen <- eigen(Xc%*%t(Xc)/n)
    var <- cbind(eigen$values/sum(eigen$values),cumsum(eigen$values/sum(eigen$values)))

    loadings2 <- eigen$vectors
    scores2 <- t(Xc)%*%loadings2

    normas2 <- sqrt(apply(scores2^2,2,sum))

    scores1<-loadings2%*%diag(normas2)
    loadings1<-scores2%*%diag(1/normas2)
  }

  output<-list(eigen,var,scores1,loadings1)
  names(output)<-c("eigen","var.exp","scores","loadings")
  output
}


center <- function(X)
{
  X<-as.matrix(X)
  n<-nrow(X)
  medias<-apply(X,2,mean)
  Xc<-X-(matrix(1,n,1)%*%rbind(medias))
  Xc
}
