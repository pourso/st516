# ST 516 - Homework 1 ========

# This file will help guide you through the first problem on Homework 1.
# Text after a hash (#) without a quote is interpreted as a code comment.
# Anything else is interpreted as R code.


# Problem 1 ========

# Part (a)  ========

# Example
# Addition: three plus four
3 + 4

# Your turn...
# Exponentiation: Three to the second power 
# pourso - R uses caret for exponentiation, below is 3 squared
3^2

# Part (b)  ========

# Example 
example1 <- 10
example1 
example2 <- "twenty"
example2
class(example1)
class(example2)

# Your turn...
# pourso - examples of numeric, boolean and character variables
one <- 31.28
class(one)
# Don't forget two and three!

# Now you do the next two
# pourso - logical and character vars
two <- TRUE
class(two)
three <- "hooray"
class(three)


# Part (c)  ========

# Example
example <- c(1, 3, 2, 4)
names(example) <- c("odd1", "odd2", "even1", "even2")
example

# Your turn...
# pourso - example vector with named columns
numbers <- c(31282,5,1980,27)
names(numbers) <- c("Date", "George", "Year", "Trout")
numbers

# Part (d)  ========
# pourso - subset vector
sum(numbers[c("George","Trout")])

# Example 
prod(example) # prod() gives the product of the elements of a vector
example[c(2,4)] # This gives the 2nd and 4th elements of the "example" vector
example < 3

# Your turn...
# pourso - subset w/ boolean mask
numbers[numbers > 100]


# Now restart your R session. Highlight all of your code, and click Run. 
# Did it run without any errors?
