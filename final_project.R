library(ggplot2)

# 1. simulation study

# set random number generator seed
set.seed(1981)

# set number of samples, sample sizes of interest
n_sim <- 1000
ns <- c(10, 100, 1000)

# read in data
yrbss_2003 <- readRDS("mod9/yrbss_2003.rds")
yrbss_2013 <- readRDS("mod9/yrbss_2013.rds")

# 1.a. describe sampling distribution of sample mean of 2013 BMI data
# function to generate sample means
create_sample_mean <- function(n, n_sim){
  replicate(n_sim, mean(sample(x=yrbss_2013[["bmi"]],size=n)))
}

# function to create histogram overlay and return summary statistics and standard deviations
report_summary <- function(fn,file_string,x_label){
  # create sample distributions of desired sampling statistic
  stats <- lapply(ns, fn, n_sim = n_sim)
  df_stats <- data.frame(
    n_lo = stats[1],
    n_md = stats[2],
    n_hi = stats[3]
  )
  colnames(df_stats) <- c("n_lo","n_md","n_hi")
  
  # overlay histograms of sample distributions and include vertical lines depicting means and std dev's
  plot1a <- ggplot(df_stats) + 
    geom_histogram(aes(x = n_lo),fill="blue",alpha=0.3, binwidth = 0.1) +
    geom_vline(xintercept = mean(df_stats[["n_lo"]]), color = "blue", linetype = "dashed") +
    geom_vline(xintercept = mean(df_stats[["n_lo"]])+sd(df_stats[["n_lo"]]), color = "blue", linetype = "dotted") +
    geom_vline(xintercept = mean(df_stats[["n_lo"]])-sd(df_stats[["n_lo"]]), color = "blue", linetype = "dotted") +
    geom_histogram(aes(x = n_md),fill="red",alpha=0.3, binwidth = 0.1) +
    geom_vline(xintercept = mean(df_stats[["n_md"]]), color = "red", linetype = "dashed") +
    geom_vline(xintercept = mean(df_stats[["n_md"]])+sd(df_stats[["n_md"]]), color = "red", linetype = "dotted") +
    geom_vline(xintercept = mean(df_stats[["n_md"]])-sd(df_stats[["n_md"]]), color = "red", linetype = "dotted") +
    geom_histogram(aes(x = n_hi),fill="green",alpha=0.3, binwidth = 0.1) +
    geom_vline(xintercept = mean(df_stats[["n_hi"]]), color = "green", linetype = "dashed") +
    geom_vline(xintercept = mean(df_stats[["n_hi"]])+sd(df_stats[["n_hi"]]), color = "green", linetype = "dotted") +
    geom_vline(xintercept = mean(df_stats[["n_hi"]])-sd(df_stats[["n_hi"]]), color = "green", linetype = "dotted") +
    labs(x = x_label)
  saveRDS(plot1a,file_string)
  
  # summarize mean, standard deviation of each sample distribution  
  spreads_pe <- sapply(df_stats, sd)
  means_pe <- sapply(df_stats, mean)
  rbind(round(means_pe,3),round(spreads_pe,3))
}

# call function for sample means
report_summary(fn=create_sample_mean,file_string="mod9/plot1a.rds",x_label = "means")

#        n_lo   n_md   n_hi
# [1,] 23.642 23.642 23.643
# [2,]  1.563  0.498  0.151

# As can be seen from the output of rbind(), the mean of the sampling distribution is fairly stable
# as sample size increases from 10 to 100 to 1000, but the standard deviation dramatically decreases 
# with increasing sample size. This is illustrated in the histogram overlay, wherein the mean +- standard
# deviation is indicated by dashed and dotted lines of the corresponding color.

# 1.b. sample quantiles: 25th percentile
create_sample_quant <- function(n, n_sim){
  replicate(n_sim, quantile(sample(x=yrbss_2013[["bmi"]],size=n), probs=c(0.25)))
}
report_summary(fn=create_sample_quant,file_string="mod9/plot1b.rds",x_label = "quants")
#        n_lo   n_md   n_hi
# [1,] 20.629 20.301 20.274
# [2,]  1.308  0.407  0.128

# As can be seen from the returned values, the 25th percentile of the sampling distribution is fairly stable
# as sample size increases from 10 to 100 to 1000, but the standard deviation dramatically decreases 
# with increasing sample size. This is illustrated in the histogram overlay, where the spread of the dist 
# is clearly decreasing as the sample size increases, but are still roughly centered around the same
# value.

# 1.c. sample minimum
create_sample_min <- function(n, n_sim){
  replicate(n_sim, min(sample(x=yrbss_2013[["bmi"]],size=n)))
}
report_summary(fn=create_sample_min,file_string="mod9/plot1c.rds",x_label = "mins")
#        n_lo  n_md   n_hi
# [1,] 18.091 15.63 14.040
# [2,]  1.480  0.96  0.581

# In the case of the sample minimum, the standard deviation continues the expected trend: the values decrease
# with increasing sample size. However, in the case of the summary statistic, the mean value decreases
# significantly. This is visualized in the histogram overlay as the distributions are no longer centered
# around the same approximate value. Furthermore, the distribution corresponding to the largest sample size 
# (green, n = 1000) is truncated. This is intuitive, as we'd expect the summary statistic to approach the 
# population minimum as sample size increases.

# 1.d. difference in sample median BMI
# sample stat is difference of medians
# update sample size
ns <- c(5, 10, 100)
create_sample_median_diff <- function(n, n_sim){
  replicate(n_sim, median(sample(x=yrbss_2013[["bmi"]],size=n))-median(sample(x=yrbss_2003[["bmi"]],size=n)) )
}
report_summary(fn=create_sample_median_diff,file_string="mod9/plot1d.rds",x_label = "median's diff")
#       n_lo  n_md  n_hi
# [1,] 0.237 0.131 0.181
# [2,] 3.115 2.122 0.686

# The difference in median BMI continues to exhibit the expected trend in standard deviation, decreasing
# as sample size increases. The summary statistic is approximately normal and roughly centered around
# the same value. Given that the median is simply the 50th percetile, this would be expected for a single 
# median. The histogram overlay suggests that the difference in normal distributions is also approximately 
# normal.

# 1.e.
#> report_summary(fn=create_sample_mean,file_string="mod9/plot1a.rds",x_label = "means")
#       n_lo   n_md   n_hi
#[1,] 23.642 23.642 23.643
#[2,]  1.563  0.498  0.151
readRDS("mod9/plot1a.rds")

#> report_summary(fn=create_sample_quant,file_string="mod9/plot1b.rds",x_label = "quants")
#       n_lo   n_md   n_hi
#[1,] 20.629 20.301 20.274
#[2,]  1.308  0.407  0.128
readRDS("mod9/plot1b.rds")

#> report_summary(fn=create_sample_min,file_string="mod9/plot1c.rds",x_label = "mins")
#       n_lo  n_md   n_hi
#[1,] 18.091 15.63 14.040
#[2,]  1.480  0.96  0.581
readRDS("mod9/plot1c.rds")

#> report_summary(fn=create_sample_median_diff,file_string="mod9/plot1d.rds",x_label = "median's diff")
#     n_lo  n_md  n_hi
#[1,] 0.237 0.131 0.181
#[2,] 3.115 2.122 0.686
readRDS("mod9/plot1d.rds")

# In all cases of the simulation study, the standard deviations of the summary statistic decreases
# with increasing sample size. For the sample mean, this is understood from the Central Limit Theorem
# as the variance of the sample distribution is equal to the population variance divided by the sample size.
# Hence, as sample size increases, the variance decreases.
# The sampling distribution itself is apparently normal for the sample mean, again consistent with 
# the CLT. This appears to be true of the 0.25 quantile as well, although for seemingly more advanced
# reasons that laid out thus far in the coursework. The difference in medians also exhibits an approximately
# normal distribution. 
# The sample minimum, however, doesn't exhibit the same general behavior as the other summary statistics: as 
# sample size is increased, a greater range of values is included in the sample, and hence a the probability
# of a lower minimum increases. This is evident by the rightward shift of the mean sample minimum.