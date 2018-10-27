?t.test

# test for pop mean == 3
# set seed value, generate 40 N(5,5) values
set.seed(1908)
x <- rnorm(40,5,5)
x
t.test(x,mu=3,conf.level = 0.95)

# test of proportions: normal approx to binomial
# p = 0.25?
?prop.test
prop.test(x=20,n=100,p=0.25,conf.level = 0.95,correct = FALSE)

# test of proportions: exact binomial
# p = 0.25?
b<-binom.test(3,20,p=0.25,conf.level = 0.95)
b
