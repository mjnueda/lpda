lpdaCV <- function(data, group, scale = FALSE, pca = FALSE, PC = 2, Variability = NULL,
                   CV = "loo", ntest = 10, R = 10, f1 = NULL, f2 = NULL)
  {
  group = as.factor(group)
  if(CV=="loo")
    output = CVloo(data, group, scale =scale, pca = pca, PC = PC,
                   Variability = Variability, f1 = f1, f2 = f2)
  else if(CV=="ktest")
    output =CVktest(data, group, scale =scale, pca = pca, PC = PC, Variability = Variability,
                    ntest =  ntest, R = R, f1 = f1, f2 = f2)
  else
    output=paste(CV,"is not a specified Cross-validation method")
  output
}
