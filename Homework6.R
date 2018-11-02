library(ggplot2)
library(dplyr)
# 1. 2-sample vs paired t-test setup

# a. 
set.seed(1810)
A <- rnorm(10)
B <- rnorm(10)
C <- 0.5 + (0.8*A) + (sqrt(1-0.8^2)*B)
B <- 0.5 + B
qplot(A,binwidth=0.1)
qplot(B,binwidth=0.1)
qplot(C,binwidth=0.1)
# b. two-sample t-test
prob1 <- t.test(A,B)
prob1
#?pt
#standard error
se <- sqrt( (((sd(A))^2)/length(A)) + (((sd(B))^2)/length(B))  )
# welch's 2-sample t-ratio
t <- (mean(A)-mean(B)) / se
p <- 2*pt(t,prob1$parameter)

# c. paired t-test
#prob1b <- t.test(A,B,paired = TRUE)
diffs <- A-B
prob1b <- t.test(diffs)
prob1b
seb <- (sd(diffs) / (sqrt(length(diffs))))
#seb <- sd(diffs) / (sqrt(8))
tb <- mean(diffs) / seb
pb <- 2*pt(tb,prob1b$parameter)

#fun1 <- (sd(diffs) / (sqrt(length(diffs))))
#fun2 <- (sd(diffs) / sqrt(prob1b$parameter))

# d. comparison (see Homework6_answers.pdf)

# e. 
prob1e <- t.test(A,C)
#prob1e
prob1eb <- t.test(A,C,paired = TRUE)
#prob1eb

q1_data <- data.frame(
  obs=rep(1:10,3),
  value=c(A,B,C),
  group=rep(c("A","B","C"),each=10)
)
q1_data
# Histograms for each sample
qplot(value, data = q1_data) + facet_wrap(~ group, ncol = 1)
# Relationship between pairs of observations, A & B
qplot(group, value, data = filter(q1_data, group != "C"),
      group = obs, geom = c("point", "line"))
# Relationship between pairs of observations, A & C
qplot(group, value, data = filter(q1_data, group != "B"),
      group = obs, geom = c("point", "line"))


# 2. Language transcriptionist
q2_data <- data.frame(
  "sn" = c(1:7),
  "e" = c(15,19,45,35,67,13,33),
  "f" = c(16,18,60,54,70,11,34)
)
# b. 
prob2b <- t.test(q2_data[,"e"],q2_data[,"f"],paired = TRUE)
#prob2b

# 3. Chemist
q3_data <- data.frame(
#  "r" = c(1:8),
  "e" = c(456,222,567,344,222,334,543,447),
  "f" = c(343,242,990,222,344,455,600,323)
)
# b. 
prob3b <- t.test(q3_data[,"e"],q2_data[,"f"])
#prob3b


