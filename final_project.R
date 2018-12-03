library(ggplot2)
library(rstudioapi) 

##### 1. simulation study

# set random number generator seed
set.seed(1981)

# set number of samples, sample sizes of interest
n_sim <- 1000
ns <- c(10, 100, 1000)

# read in data
current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path ))
yrbss_2003 <- readRDS("yrbss_2003.rds")
yrbss_2013 <- readRDS("yrbss_2013.rds")

##### 1.a. describe sampling distribution of sample mean of 2013 BMI data
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
report_summary(fn=create_sample_mean,file_string="plot1a.rds",x_label = "means")

##### 1.b. sample quantiles: 25th percentile
create_sample_quant <- function(n, n_sim){
  replicate(n_sim, quantile(sample(x=yrbss_2013[["bmi"]],size=n), probs=c(0.25)))
}
report_summary(fn=create_sample_quant,file_string="plot1b.rds",x_label = "quants")

##### 1.c. sample minimum
create_sample_min <- function(n, n_sim){
  replicate(n_sim, min(sample(x=yrbss_2013[["bmi"]],size=n)))
}
report_summary(fn=create_sample_min,file_string="plot1c.rds",x_label = "mins")

##### 1.d. difference in sample median BMI
# sample stat is difference of medians
# update sample size
ns <- c(5, 10, 100)
create_sample_median_diff <- function(n, n_sim){
  replicate(n_sim, median(sample(x=yrbss_2013[["bmi"]],size=n))-median(sample(x=yrbss_2003[["bmi"]],size=n)) )
}
report_summary(fn=create_sample_median_diff,file_string="plot1d.rds",x_label = "median's diff")

##### 1.e.
#> report_summary(fn=create_sample_mean,file_string="plot1a.rds",x_label = "means")
#       n_lo   n_md   n_hi
#[1,] 23.642 23.642 23.643
#[2,]  1.563  0.498  0.151
readRDS("plot1a.rds")

#> report_summary(fn=create_sample_quant,file_string="plot1b.rds",x_label = "quants")
#       n_lo   n_md   n_hi
#[1,] 20.629 20.301 20.274
#[2,]  1.308  0.407  0.128
readRDS("plot1b.rds")

#> report_summary(fn=create_sample_min,file_string="plot1c.rds",x_label = "mins")
#       n_lo  n_md   n_hi
#[1,] 18.091 15.63 14.040
#[2,]  1.480  0.96  0.581
readRDS("plot1c.rds")

#> report_summary(fn=create_sample_median_diff,file_string="plot1d.rds",x_label = "median's diff")
#     n_lo  n_md  n_hi
#[1,] 0.237 0.131 0.181
#[2,] 3.115 2.122 0.686
readRDS("plot1d.rds")

##### 2. data analysis

##### 2.a. how has BMI changed?

# inspect data
qplot(yrbss_2003[["bmi"]])
qplot(yrbss_2013[["bmi"]])

# clean data?
anyNA(yrbss_2003[["bmi"]])
anyNA(yrbss_2013[["bmi"]])

# consider sample means of BMI data
mean(yrbss_2003[["bmi"]])
mean(yrbss_2013[["bmi"]])

prob_2a <- t.test(x = yrbss_2013[["bmi"]], 
                  y = yrbss_2003[["bmi"]],
                  alternative = "g")
prob_2a

# 2.b. potential for smoking?

# first approach: classify observations as smokers and non-smokers

#create new column to indicate frequency of smoking ("smoker")
q33_levels <- c("0 days", "1 or 2 days", "3 to 5 days", "6 to 9 days", "10 to 19 days", "20 to 29 days", "All 30 days")
yrbss_2013$smoke = as.integer(factor(yrbss_2013[["q33"]],levels = q33_levels))

# create new column indicating class membership - non-smoker:0, smoker:1
yrbss_2013$is_smoker = 0
yrbss_2013[is.na(yrbss_2013$smoke),"is_smoker"] <- NA
yrbss_2013$is_smoker[yrbss_2013$smoke < 2] <- 0
yrbss_2013$is_smoker[yrbss_2013$smoke > 1] <- 1

# inspect data
barplot(table(yrbss_2013[yrbss_2013$sex=="Male","is_smoker"]),names.arg = c("non-smoker", "smoker"),
        main = "Male smoker status", xlab = "Class", ylab = "Count")
barplot(table(yrbss_2013[yrbss_2013$sex=="Female","is_smoker"]),names.arg = c("non-smoker", "smoker"),
        main = "Female smoker status",xlab = "Class", ylab = "Count")

# first approach: test for proportions
m<-table(yrbss_2013[yrbss_2013$sex=="Male","is_smoker"])
f<-table(yrbss_2013[yrbss_2013$sex=="Female","is_smoker"])

rbind(prop.table(m),prop.table(f))

# 2-sample test for equal proportions
sm <- c(m[2],f[2])
total <- c(m[1]+m[2],f[1]+f[2])
prop.test(sm,total, correct = FALSE)

# second approach: convert categorical responses to ordinal data

q33_t <- table(yrbss_2013[["smoke"]])
barplot(q33_t)

q33_nick <- c("0","1 or 2","3 to 5","6 to 9","10 to 19","20 to 29","30")
mf<-table(yrbss_2013[yrbss_2013$sex=="Male","smoke"])
ff<-table(yrbss_2013[yrbss_2013$sex=="Female","smoke"])
rbind(round(prop.table(mf),3),round(prop.table(ff),3))
barplot(table(yrbss_2013[yrbss_2013$sex=="Male","q33"]),names.arg = q33_nick,
        main = "Days male high-schoolers smoked in last 30",
        xlab = "Days", ylab = "Count")
barplot(table(yrbss_2013[yrbss_2013$sex=="Female","q33"]),names.arg = q33_nick,
        main = "Days female high-schoolers smoked in last 30",
        xlab = "Days smoked in last 30", ylab = "Count")

# chi-squared test requires count of new categorical data (male) and expected proportions (female)
chisq.test(table(x = yrbss_2013[yrbss_2013$sex=="Male","smoke"]),
           p = prop.table(table(yrbss_2013[yrbss_2013$sex=="Female","smoke"])),
           correct = FALSE)

# 2.c. how much TV do high-schoolers watch?

# inspect data as simple table
table(yrbss_2013[,"q81"])

# plot as bar chart
q81_nick <- c("0","<1","1","2","3","4","5+")
barplot(table(yrbss_2013[,"q81"]),names.arg = q81_nick,
        main = "Average hours of TV on school day", xlab = "Hours", ylab = "Count")

# create new column with ordinal data, review summary of proportions
yrbss_2013$tv = as.integer(factor(yrbss_2013[["q81"]]))
tv_table <- t(as.matrix(prop.table(table(yrbss_2013[,"tv"])))) 
colnames(tv_table) <- q81_nick
round(tv_table,3)
