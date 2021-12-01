CVloo <- function(data, group, scale = FALSE, pca = FALSE, PC = 2,
                  Variability = NULL, f1 = NULL, f2 = NULL)
{
  n = nrow(data)

  OUT.lpda=NULL
  for (i in 1:n){
    adpl = lpda(data[-i,], group[-i], scale =scale, pca = pca, PC = PC,
                Variability = Variability, f1 = f1, f2 = f2)
    predlpda = predict(adpl, data[i,], group[i])$fitted
    OUT.lpda = c(OUT.lpda, predlpda)
  }
  paste("Prediction error rate: ", mean(group!=OUT.lpda), sep="")
}
