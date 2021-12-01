CVktest <- function(data, group, scale = FALSE, pca = FALSE, PC = 2,
                    Variability = NULL, ntest = 10, R = 10, f1 = NULL, f2 = NULL)
{
  # ntest is the number of samples in the test-set
  # R is the times the model is evaluated with each PC indicated in PCs vector
  group = as.factor(group)
  n = nrow(data)
  OUT.lpda = NULL

  for (i in 1:R){
    test = sample(1:n, ntest)
    group.train = group[-test]
    group.test = group[test]
    data.train = data[-test, ]
    data.test = data[test, ]

    model = lpda(data = data.train , group = group.train, pca = pca,
                 PC = PC, Variability = Variability, f1 = f1, f2 = f2)
    predlpda = predict(model, data.test, group.test)$fitted
    error.i = sum(group.test!=predlpda)/ntest
    OUT.lpda = c(OUT.lpda, error.i)
  }
  paste("Prediction error rate: ", mean(OUT.lpda), sep="")

}
