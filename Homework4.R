#install.packages("openintro")
library("openintro")

# investigate data
gifted
head(gifted)
?gifted
histPlot(gifted$motheriq)

# b. compute z-stat
x <-gifted["motheriq"][,1]
x
qplot(x, binwidth=1)
mean(x)

Z <- (mean(x) - 100)/(sd(x)/sqrt(length(x)))
Z

# c. compute p-val for test
# Calculate p-value (one-sided test)
P <- pnorm(abs(Z), mean = 0, sd = 1, lower.tail = FALSE)
#P <- pnorm(abs(Z), mean = 0, sd = 1, lower.tail = TRUE)
P

# d. point estimate and 95% conf int
point_est <- mean(x)
point_est
lower_bound <- point_est - qnorm(0.975) * sd(x)/sqrt(36)
upper_bound <- point_est + qnorm(0.975) * sd(x)/sqrt(36)
lower_bound
upper_bound
