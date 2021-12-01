lpda <- function(data, group, scale = FALSE, pca = FALSE, PC = 2, Variability = NULL,
                 f1 = NULL, f2 = NULL)
{
#-------------------Pretreatment-----------------------------------
  data0 = data
  if(scale) data = stand(data)
  if(pca){
    res.pca = lpda.pca(data, group, PC, Variability)
    data = res.pca$scores
  }

#------------------------------------------------------
  group = as.factor(as.character(group))
  k = length(levels(group))
  compare = combn(levels(group),2)
  pares = ncol(compare)

  group=as.character(group)
  COEF = NULL
     for (i in 1:pares){
      cases = group%in%compare[,i]
      group.i = group[cases]
      group.i = as.factor(group.i)
      data.i = data[cases,]

      coef = lpda.fit(data.i, group.i, f1=NULL, f2=NULL)

      COEF = cbind(COEF, coef)
      NAME = paste(compare[1,i],compare[2,i], sep="-")
      colnames(COEF)[i] = NAME
     }
  group=as.factor(group)
  output <- list(COEF, data0, group, scale, pca)
  names(output)<-c("coef", "data", "group", "scale", "pca")
  if(pca){
    output[[6]] = res.pca$loadings
    output[[7]] = res.pca$scores
    output[[8]] = res.pca$var.exp
    output[[9]] = res.pca$PCs
    names(output)[6:9] = c("loadings", "scores","var.exp", "PCs")
  }

  class(output)<-"lpda"
  output
}

