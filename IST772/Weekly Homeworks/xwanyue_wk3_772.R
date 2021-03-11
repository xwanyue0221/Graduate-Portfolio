######################################################
# IST772, Chapter 3
#
# Student Name: Wanyue Xiao
# Homework Number: Week 3
# Date: 09/05/2020
#
# Sttribution statement:
# 1. I did this homework by myself, with help from the book and the professor.

#Exercise 1 
set.seed(1234) 
testPop <- rnorm(100000, mean=100, sd=10)
hist(testPop)
# The distribution of this histogram has a bell shape, indicating it is normally
# distributed. The center of this histogram is about 100.

# EXERCISE 2
hist(testPop)
abline(v=quantile(testPop, probs=0.025),col="green") # Lower tail
abline(v=quantile(testPop, probs=0.975),col="green") # Upper tail 
quantile(testPop, probs = c(0.025, 0.975))

#EXERCISE 3
# the % of the cases that occur in the central region is 95%
# What percentage falls in the lower tail, below the 0.025 quantile in the histogram?
# the % that falls in the lower tail is 2.5
# What percentage falls in the upper tail, above the 0.975 quantile?
# the % that falls above in the upper tail is 2.5

#EXERCISE 4
sampleTestScores <- function(n){sample(testPop,size=n,replace=TRUE)}
# by creating a function whcih takes one parameter n, it returns samples of size n from
# the testPop dataset with replacement 
samplingDistribution <- replicate(1000, mean(sampleTestScores(100)))
# to create a sample distribution, replicate the calculation of the mean of # the sample of the 
# data set with size = 100 1000 times.

#EXERCISE 5
par(mfrow=c(2,1)) # Divide the plot area horizontally 
hist(testPop, xlim=c(50,140))
hist(samplingDistribution, xlim=c(50,140)) 
par(mfrow=c(1,1)) # Restore the plot area 

# Case A:  
#QUESTION 1 What is the mean of the sampling distribution? 
students <- rnorm(100000, mean=100, sd=10)
sample_student <- function(n){sample(students,size=n,replace=TRUE)} 
get_mean <- mean(sample_student(100))
get_mean #98.98 is the mean 
mean_list <- replicate(1000, mean(sample_student(100)))
hist(mean_list)

#QUESTION 2
hist(students)
abline(v=quantile(students, probs=0.025),col="green")
quantile(students, probs=0.025)

#QUESTION 3
abline(v=quantile(students, probs=0.975),col="red")
quantile(students, probs=0.975) 

#QUESTION 4
quantInv <- function(distr, value) ecdf(distr)(value)
quantInv(students, 101) # 0.5396 quantile

#QUESTION 5
# could not tell

#Case B:
# QUESTION 1
Utica <- rnorm(10000, mean=96, sd=10)
Utica.sample <- function(n){sample(Utica,size=n,replace=TRUE)} 
Utica.mean <- mean(Utica.sample(100))
Utica.mean 
replicate.Utica <- replicate(1000, mean(Utica.sample(100)))
hist(replicate.Utica)
abline(v=quantile(replicate.Utica, probs = c(0.025,0.975)))

# QUESTION 2
# yes, the new mean was drawn from the population that created the sampling distribution 

# Case C
# QUESTION 1
FDA <- rnorm(10000, mean=80, sd=10)
FDA.sample <- function(n){sample(FDA,size=n,replace=TRUE)} 
FDA.mean <- mean(FDA.sample(500))
FDA.mean 
replicate.FDA <- replicate(1000, mean(FDA.sample(500)))
hist(replicate.FDA)
abline(v=quantile(replicate.FDA, probs = c(0.025,0.975)))
# QUESTION 2
hist(FDA)
abline(v=quantile(FDA, probs=0.025),col="green")
quantile(FDA, probs=0.025)
# QUESTION 3
hist(FDA)
abline(v=quantile(FDA, probs=0.925),col="red")
quantile(FDA, probs=0.925)
# QUESTION 4
quantInv <- function(distr, value) ecdf(distr)(value)
quantInv(FDA, 79) # 0.4654 quantile
# QUESTION 5
# Yes, it is.
# QUESTION 6
# The sample size and population mean are changing. 




