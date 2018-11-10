library(ggplot2)

### ST516 Homework 7

### 1. validity of t-based conf intervals

### a. plot of gamma

# x axis values
x <- seq(from=0,to=10,by=0.01)
# y value from PDF of Exp dist
y <- dgamma(x, shape = 2, rate = 2)
# plot
qplot(x,y,geom="line")


#qplot(rgamma(100, shape = 2, rate = 2),binwidth=0.01)
#?dgamma

### b. function

ci <- function(n = 25){
  a <- 2
  b <- 2
  m <- a/b
  x <- rgamma(n, shape = a, rate = b)
  t <- t.test(x,mu=m)
  (m > t$conf.int[1]) & (m < t$conf.int[2])
}

### c. n = 5
intervals <- replicate(100000,ci(5))
mean(intervals)

### d. n = 25, 50, 100
intervals <- replicate(100000,ci(25))
mean(intervals)

intervals <- replicate(100000,ci(50))
mean(intervals)

intervals <- replicate(100000,ci(100))
mean(intervals)

### e. confidence intervals as n increases
# As sample size n increases, the proportion of confidence 
# intervals containing the true mean of 1 approaches 95%.
# This is a result of the Central Limit Theorem, which 
# states that means based on large samples have approximately
# normal sampling distributions, regardless of population
# distribution. As sample size increases, the normality of
# the sample distribution increases, and the t-tools are more
# valid.
