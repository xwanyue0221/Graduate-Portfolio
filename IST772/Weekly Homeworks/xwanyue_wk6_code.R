######################################################
# IST772, Chapter 6
#
# Student Name: Wanyue Xiao
# Homework Number: Week 6
# Date: 09/29/2020
#
# Sttribution statement:
# 1. I did this homework by myself, with help from the book and the professor.

# 1. 
insurance <- read.csv("/Users/wanyuexiao/Documents/SYRACUSE/IST772 Quantitative Reasoning/week 6/dWeek6insurance.csv",
                      header = TRUE,
                      stringsAsFactors = TRUE)
str(insurence)
summary(insurence)

# 2.
boxplot(charges ~ age, data = insurance)
boxplot(charges ~ sex, data = insurance)
boxplot(charges ~ children, data = insurance)
boxplot(charges ~ smoker, data = insurance)
boxplot(charges ~ region, data = insurance)

# 3.
tapply(insurance$charges, insurance$region, mean)
tapply(insurance$charges, insurance$region, sd)
# the mean of those four regions are 13406.38  12417.58  14735.41  12346.94.
# among those values, that of southeast is the highest while that of southwest is the lowest.
# the sd of those four regions are 11255.80  11072.28  13971.10  11557.18.

# 4.
aovOut <- aov(charges ~ region, data = insurence)
aovOut

# 5.
library(BayesFactor)
insurence$region <- as.factor(insurence$region)
result <- anovaBF(charges ~ region, data = insurence)
mcmcOut2 <- posterior(result,iterations=10000) 
boxplot(as.matrix(mcmcOut2[,2:5]))
summary(mcmcOut2)
result # to get the Bayes Factor

# 6.
# if we choose the alpha value as 0.5, we might need to reject the null hypothesis.
# the Bayes Factor is 0.139821:1 in favor of the null:alternative, which is pretty 
# week in this case. besides, looking at the boxplot, only the northeat region overlap 
# 0, the rest of the three groups do not overlap 0, indicating that those three groups
# significantly variant from the grand mean of the population.

# 7.
TukeyHSD(aovOut)
# the significant pairwaise is southwest-southeast.

# 8.
install.packages("emmeans")
library(emmeans)

# 9.
emOut <- emmeans(aovOut, "region")
plot(emOut) 
summary(emOut)

# 10.
pwwp(emOut)
# could not run this function

# 11. 
# accordingly, southwest and northwest seems to overlap 
# the same range of values. southeast has the highest ci range,
# which implies that this region variant from the others significantly.

