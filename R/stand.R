#----------------------------------------------------------
stand<-function(X)
{
  cnames=colnames(X)
  rnames=rownames(X)
  X = as.matrix(X)
  n = nrow(X)
  medias = apply(X, 2, mean)
  des = apply(X, 2, sd)
  des = sqrt(des^2*(n-1)/n)
  Xc = X - (matrix(1,n,1)%*%rbind(medias))
  Xs = Xc%*%diag(1/des)
  colnames(Xs) = cnames
  rownames(Xs) = rnames
  Xs
}

#----------------------------------------------------------
stand2 <- function(X, X2)
{
  #aquí no hacen falta rnames y cnames pq se usa para proyectar y se toman
  #los nombres de las componentes
  X = as.matrix(X)
  X2 = as.matrix(X2)
  n = nrow(X)
  medias = apply(X, 2, mean)
  des = apply(X, 2, sd)
  des = sqrt(des^2*(n-1)/n)
  # usamos datos de X para estandarizar X2
  n2 = nrow(X2)
  if(is.null(n2)) {
    Xc2 = X2 - medias
    Xs2 = Xc2/des
  }
  else{
    Xc2 = X2-(matrix(1,n2,1)%*%rbind(medias))
    Xs2 = Xc2%*%diag(1/des)
  }
  Xs2
}
