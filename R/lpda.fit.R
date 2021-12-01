lpda.fit <- function(data, group, f1=NULL, f2=NULL)
{

p = ncol(data) #variables

group = as.factor(group)
data1 <- data[group==levels(group)[1],]
data2 <- data[group==levels(group)[2],]
n1 = nrow(data1) # size of first group
n2 = nrow(data2) # size of second group
n = n1+n2 # total size

# Objective function
  if(is.null(f1)) { f1 = rep(1/n1, n1) }
  if(is.null(f2)) { f2 = rep(1/n2, n2) }
  f3 = rep(0, p+1)
  f = c(f1,f2,f3)

# matriz de restricciones
  # primer bloque
   u1 = diag(rep(-1, n1))
   v1 = matrix(0, n1, n2)
   ab1 = cbind(-data1,rep(1, n1))
 B1=as.matrix(cbind(u1,v1,ab1))

  # segundo bloque
   u2 = u1
   v2 = v1
   ab2 = matrix(0, n1, p+1)
 B2=cbind(u2,v2,ab2)

  # tercer bloque
    u3 = matrix(0,n2,n1)
    v3 = diag(rep(-1,n2))
    ab3 = cbind(data2,rep(-1,n2))
 B3 = as.matrix( cbind(u3,v3,ab3))

  # cuarto bloque
   u4 = u3
   v4 = v3
   ab4 = matrix(0,n2,p+1)
 B4 = cbind(u4,v4,ab4)

colnames(B1) <- colnames(B2) <- colnames(B3) <-colnames(B4) <-NULL
A = rbind(B1,B2,B3,B4)
  # vector de segundos miembros
s1 = rep(-1,n1)
s2 = rep(0,n1)
s3 = rep(-1,n2)
s4 = rep(0,n2)
b = c(s1,s2,s3,s4)

nvar <- n+p+1
bounds = list(lower = list( ind=c(1:nvar), val=rep(-Inf,nvar) ),
              upper = list( ind=c(1:nvar), val=rep(Inf,nvar) ) )

sol<- Rglpk_solve_LP(f, A, rep("<=",2*n), b, bounds = bounds)
#infea <- sol$solution[1:n]
coef <- sol$solution[(n+1):nvar]
coef
}
