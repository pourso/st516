# Module 2 Lab
# load ggplot library for qplot
library(ggplot2)

# create function to return sim rolling 2 dice
roll <- function(bones=1:6){
  dice <- sample(bones,size=2,replace=TRUE)
  sum(dice)
}


rolls <- replicate(10000, roll()) # Roll the dice 10 times!
rolls # Show the results, stored in the "rolls" vector
qplot(rolls, binwidth = 1) # Histogram of our roll outcomes
rolls == 7 # Gives a logical vector of "TRUE" (lucky 7) and "FALSE" (not lucky 7)
sum(rolls == 7) # R knows to count "TRUE" as 1, "FALSE" as 0
sum(rolls == 7)/10000 # Divide by 10 to get a proportion

# logical subsetting
x <- c(1,2,3,4,5,6,7,8)
x[x>4 & x<=6]
x[x>6 | x< 3]
