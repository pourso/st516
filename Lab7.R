library(ggplot2)
set.seed(1800)

#Rejection rates and uniform p-values

pval <- function(n=25){
  x <- rnorm(n,mean = 0,sd = 1)
  test <- t.test(x,mu=0)
  test$p.value
}

pval()

p_values <- replicate(100000,pval())
mean(p_values < 0.05)
mean(p_values < 0.75)

qplot(p_values,binwidth=0.05,alpha=I(0.2), boundary=0)
qplot(p_values,binwidth=0.05,alpha=I(0.2))

#Violating assumptions: non-normality
qplot(rbeta(100,0.5,0.5),binwidth=0.01)
?rbeta
x<-rbeta(10,shape1 = 0.5,shape2 = 0.5)
y<-runif(10,0,1)
t.test(x,y,mu=0,paired = FALSE,var.equal = FALSE)

pval2 <- function(n1=25,n2=25){
  x<-rbeta(n1,shape1 = 0.5,shape2 = 0.5)
  y<-runif(n2,min=0,max=1)
  test <- t.test(x,y,mu=0,paired = FALSE,var.equal = FALSE)
  test$p.value
}

set.seed(1770)
sim <- replicate(100000,pval2(5,5))
mean(sim<0.05)
mean(sim<0.03)
qplot(sim, binwidth=0.05,alpha=I(0.2),boundary=0) + xlab("P-values")

# n2 increases to 10x n1
sim <- replicate(100000,pval2(5,50))
qplot(sim, binwidth=0.05,alpha=I(0.2),boundary=0) + xlab("P-values")
mean(sim<0.05)

# n1 increases to 10x n2
sim <- replicate(100000,pval2(50,5))
qplot(sim, binwidth=0.05,alpha=I(0.2),boundary=0) + xlab("P-values")
mean(sim<0.05)

# n1 equals n2
sim <- replicate(100000,pval2())
qplot(sim, binwidth=0.05,alpha=I(0.2),boundary=0) + xlab("P-values")
mean(sim<0.05)

# t-test robust to non-normality!
