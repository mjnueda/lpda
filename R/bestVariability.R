bestVariability <- function(data, group, ntest = 10, R = 10,
                             Vars = c(0.5,0.7), f1 = NULL, f2 = NULL)
{
  # ntest is the number of samples in the test-set
  # R is the times the model is evaluated with each Variability indicated in Vars vector
  group = as.factor(group)
  n = nrow(data)
  Res = NULL

  for (j in 1:R)
  {
    test = sample(1:n, ntest)
    group.train = group[-test]
    group.test = group[test]
    data.train = data[-test, ]
    data.test = data[test, ]
    Ej=NULL

    for (i in Vars)
    {
      model = lpda(data = data.train , group = group.train, pca = TRUE, Variability = i,f1 = f1, f2 = f2)
      pred = predict(model, data.test)
      Ej = c(Ej, sum(pred$fitted!=group.test))
    }
    Res = rbind(Res, Ej)
  }
  colnames(Res) = as.character(Vars)
  Res = apply(Res, 2, mean, na.rm=TRUE)
  Res/ntest
}

