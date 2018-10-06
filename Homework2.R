# ST 516 - Homework 2 ========

# Problem 1 ========

# Part (a)  ========

# Part (i)  ========
  h = runif(10000, min=0, max=1)

# Part (ii)  ========
  sum((0.5<h) & (h<0.75))/10000
  
# Part (b)  ========
  
# Part (i)  ========
  h = rbinom(n=10000,size=2,prob = 0.49)
  sum(h==2)/10000
  
# Part (ii) ========
  h = rbinom(n=10000,size=4,prob = 0.49)
  sum(h==4)/10000
  
# Part (iii) ========
  h = rbinom(n=10000,size=2,prob = 0.49)
  sum(h==1)/sum((h==1) | (h==0))

# Part (iv) ========
# For our population of interest, we would need to know the average number of children a family has.