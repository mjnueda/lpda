plot.lpda<-function(x, PCscores = FALSE, xlim = NULL,
                    main = NULL, legend.pos = "topright", ...)
{
  # x is an object of class inheriting from "lpda"
  if(!inherits(x, "lpda"))
    stop("x should be of class 'lpda' ")
  if(PCscores){
    if(!x$pca)  stop("PCscores only can be plotted when 'pca=TRUE' in 'lpda'")
    vars = round(100*x$var.exp[,1],2)
    xlab = paste("PC-1 ",vars[1],"%",sep="")
    ylab = paste("PC-2 ", " ",vars[2],"%",sep="")

    plot(x$scores[,c(1,2)], col = as.numeric(x$group)+1, pch=20, main = main,
         xlab=xlab, ylab=ylab)

    if(length(levels(x$group))==2)
    abline(a = x$coef[3]/x$coef[2], b=-x$coef[1]/x$coef[2],cex=2)

    if(!is.null(legend.pos))  {
      col = as.numeric(as.factor(levels(x$group)))+1
    legend(legend.pos, levels(x$group), col=col, pch=20) }
  }
 else{
   pred = predict(x, x$data)
   eval = pred$eval
   pares = ncol(eval)

   group = x$group
   compare = combn(levels(group),2)
   group=as.character(group)

   for (i in 1:pares){
     cases = group%in%compare[,i]
     group.i = group[cases]
     group.i = as.factor(group.i)
     data.i = x$data[cases,]
     class = as.numeric(as.factor(group.i))
     n1 = sum(class==1)
     n2 = sum(class==2)
     n = n1 + n2

     #if(is.null(xlim))
     xlim=c(min(eval[cases,i]),max(eval[cases,i]))
     ylim = c(1, max(n1, n2)*1.3)
     eje.y = NULL
     eje.y[class==1] = c(1:n1)
     eje.y[class==2] = c(1:n2)
     plot(eval[cases,i], eje.y, xlim=xlim, ylim=ylim,col=class+1,pch=20,
          xlab="Distance to the hyperplane", ylab="Index", main=colnames(eval)[i],...)
     abline(v=0,lty=2)

     legend(legend.pos, levels(group.i),col=c(2,3), pch=20)
} #final bucle

 }
  }
