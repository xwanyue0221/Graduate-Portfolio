######################################################
# IST776, Chapter 1
#
# Student Name: Wanyue Xiao
# Homework Number: Week 1
# Date: 28/08/2020
#
# Sttribution statement:
# 1. I did this homework by myself, with help from the book and the professor.

##### Exercise 1 ######
# Data Loading History:
# load("~/Documents/SYRACUSE/IST772 Quantitative Reasoning/WEEK 1/tinydata.Rdata")
# tinydata <- read_csv("Documents/SYRACUSE/IST772 Quantitative Reasoning/WEEK 1/tinydata.csv")

# Install Package:
# install.packages("BayesFactor")
library(BayesFactor)
help("Control")
View(Orange)
Summary(Orange)

hist(Orange$circumference)
# This is a histogram. The shape of this graph is asymmetric. It is neither flat or
# neat. It has some fluctuations. The highest bar is the fifth one. The lowest bar is the
# ninth one. The frequency of the first and the eighth one are the same, which is 5. The
# frequency of the rest bars are around 3.
plot(Orange$age, Orange$circumference)
# This is a scatter plot which consist of aound 35 points. The x-axis represents age 
# while y-axis represents the circumference. Most of the points are around the diagonal
# line. The age ranges from 118 to 1582 while circumference ranges from 30 to 214.

##### Exercise 2 ###### 
smallData <- c(71,71,72,74)
# mean: 
mean <- sum(smallData)/length(smallData)
mean
# the sum of squared deviations from the mean:
sum_suared_dev <- sum((smallData - mean)^2)
sum_suared_dev
# the variance:
variance <- sum_suared_dev/length(smallData)
variance
# the standard deviation:
sd <- sqrt(variance)
sd
# Standard Deviation, which is the square root of variance, gives us the units used in
# the original scale.

# 2. Merely having the median and mean, we could not decide the distribution of the data
# points. Similarly, knowing median and mean could not help use deciding the value of mode.

# 3. I would like to live in the Alpha City since the standard deviation is smaller than
# that of Omega City, indicating that the temperature change about 6 degree instead of 18 degree.
# Personally, I like the temperature to stay relatively stable.

# 4. 
myDist <- rbinom(1000,144,0.5)
mean(myDist)
median(myDist)
# There are two ways to calculate the standard deviation:
sqrt(sum((myDist - mean(myDist))^2)/length(myDist))
sd(myDist)
# There are 474 data points whose value is higher than the median (which is 72).
diff <- myDist - median(myDist)
length(diff[diff > 0])
# Create a histogram and calculate the 99th percentile value.
hist(myDist)
abline(v=quantile(myDist, .99),col="red")
# The value of 99th percentile is 84.01.
quantile(myDist, .99)
above <- myDist - quantile(myDist, .99)
length(above[above > 0])
# The number of data points whose value is higher than 84.01 is 10.

# 5. 
# Assuming there were around 10,000 cars passed the station. The speed ranges from 15 to 30.
# The shape of this distribution is pretty flat.
speed <- runif(10000, min=15, max=30)
mean(speed)
median(speed)
sqrt(sum((speed - mean(speed))^2)/length(speed))
hist(speed)
# Assuming there were around 10,000 cars passed the station. Since each day we have 24*60 minutes,
# we should set the runif number as 24*60. The number of cars passed per minute ranges 
# from 15 to 30. The shape of this distribution is pretty flat.
number_per_minute <- runif(24*60, min=0, max=14)
mean(number_per_minute)
median(number_per_minute)
sqrt(sum((number_per_minute - mean(number_per_minute))^2)/length(number_per_minute))
hist(speed)
# Assuming there were 10,000 cars passed the station. The weight of cars ranges from 
# 2,900 pounds to 3,800. Again, the distribution of the weight data points is flat.
weight <- runif(10000, min=2900, max=3800)
mean(weight)
median(weight)
sqrt(sum((weight - mean(weight))^2)/length(weight))
hist(weight)


