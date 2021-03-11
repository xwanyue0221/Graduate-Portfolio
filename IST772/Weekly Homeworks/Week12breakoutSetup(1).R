# IST772 - Time Series Exercise Code Helper

title: "Week12breakout"
author: "Jeff Stanton"
date: "11/6/2020"
output: html_document
co_author: "Wanyue Xiao"

# Sttribution statement:
# 1. I modified this file based on an original file provided by the professor, including
# the resuage of code embedded in the rmd document

#install.packages("tseries") # You will need this if you don't have it

#----------------------------------------
# 1. Read in the data
#install.packages("readr")
#setwd("~/Dropbox/Teaching/Statistics/WeekByWeekLive/Week10")
# Set the working directory to where you have stored the file
library(readr) # Use the modern read_csv 
co2data <- read_csv("noaadata.csv") 
str(co2data)
summary(co2data)
# Note that this is not (yet) a time series
# Sites 1 and 3 are in Alaska: Site 1 is Cold Bay, 
# AK (in the Aleutian islands) and site 3 is Barrow, 
# AK on the Chukchi Sea. Site 2 is located at Mauna Loa, HI.
# # # Describe what you have observed in a comment: 
hist(co2data$site3)
# The distibution of those three site seems similar with each other.

#----------------------------------------
# 2. Create a correlation matrix of the data
round(cor(co2data),2)
# # # Describe what you have observed in a comment: 
# The sites are highly correlated with each others while the rest variables are not.

#----------------------------------------
# 3. Create a time series plot of Cold Bay, AK and other sites.
plot.ts(co2data$site1)
plot.ts(co2data$site2) 
plot.ts(co2data$site3) 

# Describe what you have observed in a comment:
# All of those variables has a upward trend but with different seasonality.

#----------------------------------------
# 4.	Convert the raw data into a multivariate time series object 
# with the following command:
co2series <- ts(co2data[,4:6], start=c(1978,10), frequency=12)
str(co2series)
plot(co2series)
# Describe what you have observed in a comment:
# Similarly, those two lines has a upward trend but with different seasonality or
# cyclinity.

#----------------------------------------
# 5.	Use the decompose() function to break each 
# site???s time series into its components. 
dec1 <- stats::decompose(co2series[,1])
dec2 <- stats::decompose(co2series[,2])
dec3 <- stats::decompose(co2series[,3])
#----------------------------------------
# 6.	Plot each decomposition object using the plot() command.
plot(dec1)
plot(dec2)
plot(dec3)

# Describe what you have observed in a comment:
# Despite of the trend that we already mentioned above, the pattern of random
# of dec1 is similar to that of dec2, which has a rather larger variaty in the 
# later period. What's more, the seasonality existed in each of the three.

#----------------------------------------
# 7.	put the random components into a data frame, remove missing 
# data, and convert to a times series. 
sites <- data.frame(dec1$random, dec2$random, dec3$random)
sites <- sites[complete.cases(sites),]
sites <- ts(sites,start=c(1978,10),frequency=12) 
summary(sites)
str(sites)
plot(sites) 
# Describe what you have observed in a comment:
# The second one has a rather patternless graph. Besides, the variance of dec3 is the
# smallest.

#----------------------------------------
# 8. examine the auto-correlation function (ACF) 
# graph for each of the random components
acf(sites[,1])
acf(sites[,2])
acf(sites[,3])

# Describe what you have observed in a comment:
# Does each series appear to be stationary?
# those plot are not stationary

#----------------------------------------
# 9. Run the Augmented Dickey-Fuller test on each time series
#install.packages("tseries")
library(tseries)
adf.test(sites[,1])
adf.test(sites[,2])
adf.test(sites[,3])

# Describe what you have observed in a comment:
# each result has p-value that is lower than 0.05, therefore, one can reject the null
# hypothesis.

#----------------------------------------
# 10.	Run cor() on the random component of 
# the decompositions
round(cor(sites),2)

# Describe what you have observed in a comment:
# dec1 and dec3 has a rather bigger r value, which is negative 0.2. For the rest
# two variables, the correlation value is quit near zero.


#----------------------------------------
# 11.	Conduct a change point analysis of the variability 
# of each time series random component. 12. Plot the results.
#install.packages("changepoint")
library(changepoint)
cptVarOut1 <- cpt.var(sites[,1])
cptVarOut1
plot(cptVarOut1)

cptVarOut2 <- cpt.var(sites[,2])
cptVarOut2
plot(cptVarOut2)

cptVarOut3 <- cpt.var(sites[,3])
cptVarOut3
plot(cptVarOut3)

# # # Run variance changepoints and plot the results for the other sites
# The first one and thrid one has a change point while the second one does not.


#----------------------------------------
# 13.	Write a brief paragraph integrating the results of the analyses 
# Remember that Sites 1 and 3 are in Alaska: Site 1 is Cold Bay, 
# AK (in the Aleutian islands) and site 3 is Barrow, 
# AK on the Chukchi Sea. Site 2 is located at Mauna Loa, HI.


