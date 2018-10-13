library(ggplot2)


# Homework 3, Problem 1
source("https://gist.githubusercontent.com/cwickham/abe3b4c4ba5319e8e1dd5102541f2117/raw")
y<-replicate(100000,play(silent=TRUE))
qplot(y)

x <- replicate(10000, mean(replicate(100,play(silent=TRUE))) )
qplot(x, binwidth = 1, xlim=c(-55,20)) 

mean(x)

# Homework 3, Problem 2
#The Poisson distribution is popular for modelling the number of times an event occurs in an interval of time or space.
?rpois

# Sample size = 2 observations from Poisson dist; 
# 10,000 replications
x <- replicate(10000, mean(rpois(2, 5))) 
# Create a histogram of results
qplot(x, binwidth=.25, xlim=c(-5, 15)) 

# Sample size = 5; 10,000 replications
x <- replicate(10000, mean(rpois(5, 5))) 
# Create a histogram of results
qplot(x, binwidth=.25, xlim=c(-5, 15)) 

# Sample size = 10; 10,000 replications
x <- replicate(10000, mean(rpois(10, 5))) 
# Create a histogram of results
qplot(x, binwidth=.25, xlim=c(-5, 15)) 

# Sample size = 30; 10,000 replications
x <- replicate(10000, mean(rpois(30, 5))) 
# Create a histogram of results
qplot(x, binwidth=.25, xlim=c(-5, 15)) 

# Sample size = 100; 10,000 replications
x <- replicate(10000, mean(rpois(100, 5))) 
# Create a histogram of results
qplot(x, binwidth=.25, xlim=c(-5, 15)) 

#x1 is a sequence from 0 to 10, w/ 1000 values
#y1 is a sequence of random samples from a normal dist
#y1 vs x1 is overlaid on the sample mean dist 
x1 <- seq(0, 10, length = 1000); y1 <- dnorm(x1, 5)
qplot(x, binwidth = .05, xlim=c(-5,15)) +
  geom_line(aes(y = 10000*0.25*y1, x = x1), size = 1.5, color = "blue")
?seq
