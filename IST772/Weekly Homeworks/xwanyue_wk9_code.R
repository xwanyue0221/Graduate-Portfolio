######################################################
# IST772, Chapter 8
#
# Coauthor Name: Wanyue Xiao, Jeff Stanton
# Homework Number: Week 10
# Date: 10/20/2020
#
# Sttribution statement:
# 1. I modified this file based on an original file provided by the professor, including
# the resuage of code embedded in the rmd document.

#1.
setwd("/Users/wanyuexiao/Downloads/Week 9")
load("c_london.RData")
summary(london)
hist(london$GoldMedals) # has outliers
hist(london$Silver) # has outliers
hist(london$Bronze) # has outliers
hist(london$PopnSize, breaks = 100) # has several outliers
# london has 6 columns, including the country, the number of gold/silver/bronze medal(s) gained
# in 2012 for each country, the corresponding income, and population. All columns, except country,
# are numeric type.

# 2.
simpleSum <- london$GoldMedals + london$Silver + london$Bronze
bordaSum <- (london$GoldMedals*3) + (london$Silver*2) + london$Bronze
dowdallSum <- london$GoldMedals + (london$Silver/2) + (london$Bronze/3)
medalSum <- data.frame(Country=london$Country,
                       simpleSum,
                       bordaSum,
                       dowdallSum,
                       Income=london$Income,
                       PopnSize=london$PopnSize)

# 3.
pairs(london[,-1])
# most of the points in each graph gather in bottom left corner.However, some outliers existed
# in each graph, locating in the upper right corner.

# 4.
medalSum$sqrtIncome <- sqrt(medalSum$Income)
medalSum$logIncome <- log(medalSum$Income)
medalSum$sinIncome <- asin(medalSum$Income)
medalSum$atanIncome <- atan(medalSum$Income)
medalSum$repIncome <- 1/medalSum$Income

par(mfrow=c(2,3))
hist(medalSum$sqrtIncome)
hist(medalSum$logIncome)
hist(medalSum$sinIncome)
hist(medalSum$atanIncome)
hist(medalSum$repIncome)
par(mfrow=c(1,1))

# 5.
lmOut1 <- lm(simpleSum ~ Income + PopnSize, data=medalSum)
lmOut2 <- lm(simpleSum ~ logIncome + PopnSize, data=medalSum)
lmOut3 <- lm(simpleSum ~ sqrtIncome + PopnSize, data=medalSum)
lmOut4 <- lm(simpleSum ~ sinIncome + PopnSize, data=medalSum)
lmOut5 <- lm(simpleSum ~ repIncome + PopnSize, data=medalSum)
lmOut6 <- lm(simpleSum ~ atanIncome + PopnSize, data=medalSum)

lmOut7 <- lm(bordaSum ~ sinIncome + PopnSize, data=medalSum)
lmOut8 <- lm(dowdallSum ~ sinIncome + PopnSize, data=medalSum)
summary(lmOut2)
# the R-squared value is significant as well as those b-weights.

# 6.
lmOutBayesFactor <- BayesFactor::lmBF(simpleSum ~ logIncome + PopnSize, data=medalSum, posterior=FALSE)
lmOutBayesEstimation <- BayesFactor::lmBF(simpleSum ~ logIncome + PopnSize, data=medalSum, posterior=TRUE, iterations=10000)
lmOutBayesFactor
summary(lmOutBayesEstimation)
# we cannot choose the sinIncome which contains NA value. Therefore, we will use logIncome.
# The coefficient of logIncome and PopSize is quit different with the coefficient obtained aboved.
# The 95% HDI for the coefficient of ogIncome and PopSize does not overlaps with 0, 
# providing evidence that the population value of that coefficient of each predictor 
# does credibly differ from 0.

# 7.
rsqList <- 1 - (lmOutBayesEstimation[,"sig2"] / var(medalSum$bordaSum))
summary(rsqList)

# 1.
data("anscombe")
str(anscombe)
lmOut1 <- lm(y1 ~ x1, data=anscombe)
summary(lmOut1) # 0.6665
lmOut2 <- lm(y2 ~ x2, data=anscombe)
summary(lmOut2) # 0.6662
lmOut3 <- lm(y3 ~ x3, data=anscombe)
summary(lmOut3) # 0.6663
lmOut4 <- lm(y4 ~ x4, data=anscombe)
summary(lmOut4) # 0.6667

# 2.
par(mfrow=c(2,2))
plot(anscombe$x1, anscombe$y1)
plot(anscombe$x2, anscombe$y2)
plot(anscombe$x3, anscombe$y3)
plot(anscombe$x4, anscombe$y4)
# those four plots have different point distribution. The first one has points located around
# the diagnal line. the second one has a para-curve. the thrid one has points located in a line.
# the last one has points gathered in the left side of the graph.

# 3.
hist(lmOut1$residuals)
hist(lmOut2$residuals)
hist(lmOut3$residuals)
hist(lmOut4$residuals)
# the thrid one does not normally distributed around zero.

# 4.
plot(lmOut1$fitted.values, lmOut1$residuals)
abline(h=0)
plot(lmOut2$fitted.values, lmOut2$residuals)
abline(h=0)
plot(lmOut3$fitted.values, lmOut3$residuals)
abline(h=0)
plot(lmOut4$fitted.values, lmOut4$residuals)
abline(h=0)
par(mfrow=c(1,1))
# the third one seems to have a different amont of deviation.

# 5.
library(car)
crPlots(lmOut1)
crPlots(lmOut2)
crPlots(lmOut3)
crPlots(lmOut4)
#  A significant difference between the residual line and the component line indicates that the 
# predictor does not have a linear relationship with the dependent variable. a significant 
# difference could be detected in the second and fourth graph.

# 6.
outlierTest(lmOut1)
outlierTest(lmOut2)
outlierTest(lmOut3)
outlierTest(lmOut4)
# three out of the four tests have significant result.

# 7.
regOutMCMC1 <- lmBF(y1 ~ x1, data=anscombe, posterior=TRUE, iterations=10000)
summary(regOutMCMC1)

regOutMCMC2 <- lmBF(y2 ~ x2, data=anscombe, posterior=TRUE, iterations=10000)
summary(regOutMCMC2)

regOutMCMC3 <- lmBF(y3 ~ x3, data=anscombe, posterior=TRUE, iterations=10000)
summary(regOutMCMC3)

regOutMCMC4 <- lmBF(y4 ~ x4, data=anscombe, posterior=TRUE, iterations=10000)
summary(regOutMCMC4)

#8.
install.packages("glvma")
library(glvma)

# 9.
summary(gvlma::gvlma(lmOut1))
summary(gvlma::gvlma(lmOut2))
summary(gvlma::gvlma(lmOut3))
summary(gvlma::gvlma(lmOut4))
# for each result, it contains the report of lm() function and gvlma() runction.
# for the first part one can get the statistics of the linear regression model while
# for the second part one can see if the assumptions is acceptable or not.

# 10.
myY <- rnorm(32)
myX1 <- rnorm(32) 
myX2 <- rnorm(32) 
myX3 <- myX1 + myX2 + rnorm(32)
myData <- data.frame(myY, myX1, myX2, myX3)
lmOutV <- lm(myY ~ myX1 + myX2 + myX3, data=myData)
car::vif(lmOutV)

