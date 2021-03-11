load("datasets4.RData")

# 1.
str(usVaccines)

plot.ts(usVaccines)
plot(diff(usVaccines))
acf(usVaccines[,1])
acf(usVaccines[,2])
acf(diff(usVaccines[,3]))
acf(usVaccines[,4])
acf(usVaccines[,5])

# 2.
decDTP1 <- decompose(ts(usVaccines[,1], frequency = 2))
plot(decDTP1)

decHepB_BD <- stats::decompose(ts(usVaccines[,2], frequency = 2))
plot(decHepB_BD)
decPol3 <- stats::decompose(ts(usVaccines[,3], frequency = 2))
plot(decPol3)
decHib3 <- stats::decompose(ts(usVaccines[,4], frequency = 2))
plot(decHib3)
decMCV1 <- stats::decompose(ts(usVaccines[,5], frequency = 2))
plot(decMCV1)

# 2(3)
recent_year <- ts(usVaccines[c(-1:-20),], frequency = 1, start = 2000)
adf.test(recent_year[,1])
acf(recent_year[,2])

A <- cpt.mean(usVaccines[,1])
plot(A,cpt.col='grey',cpt.width=5)

plot(diff(usVaccines))
acf(diff(usVaccines[,5]))
adf.test(diff(usVaccines[,2]))
cor(diff(usVaccines))
# Transformations such as logarithms can help to stabilise the variance of a time series. 
# Differencing can help stabilise the mean of a time series by removing changes in the level of a time series, 
# and therefore eliminating (or reducing) trend and seasonality.
mean(diff(usVaccines))

# 3.(1)
# WithoutDTP, WithoutPolio, WithoutMMR, and WithoutHepB
summary(100-districts[,c(2:5)]) # percentage of people who get the vaccine
boxplot(100-districts[,c(2:5)]) # percentage of people who get the vaccine
apply(100-districts[,c(2:5)], 2, mean)

# 3(2)
cor(districts[,c(2:5)])
cor(100-districts[,c(2:5)])

# 3(3)
usVaccines[c(38),] # for 2017
mean(100-districts[,2])
mean(100-districts[,3])
mean(100-districts[,5])

districts$logPctUpToDate <- districts$PctUpToDate
districts$logPctBeliefExempt<- districts$PctBeliefExempt
round(cor(test[,c(7, 13:18)]),2)

test <- districts[-which(is.na(districts$logPctFamilyPoverty)),]
sd_df <- data.frame(scale(districts[,c(6,8,15:18)],center=TRUE,scale=FALSE))
sd_df$DistrictComplete <- districts$DistrictComplete
sd_df <- sd_df[-which(is.na(sd_df$logPctFamilyPoverty)),]

summary(lm(PctBeliefExempt ~ logPctFamilyPoverty + logPctChildPoverty + logEnrolled + logTotSchools, data=sd_df))
summary(lmBF(PctBeliefExempt ~ logPctFamilyPoverty + logPctChildPoverty + logEnrolled + logTotSchools, data=sd_df, posterior=FALSE))
summary(lmBF(PctBeliefExempt ~ logPctFamilyPoverty + logPctChildPoverty + logEnrolled + logTotSchools, data=sd_df, posterior=TRUE, iterations=10000))

library(car)
vif(lm(PctBeliefExempt ~ logPctFamilyPoverty + logPctChildPoverty + logEnrolled + logTotSchools, data=sd_df))

summary(lm(PctUpToDate ~ logPctFamilyPoverty + logPctChildPoverty + logEnrolled + logTotSchools, data=sd_df))
summary(lmBF(PctUpToDate ~ logPctFamilyPoverty + logPctChildPoverty + logEnrolled + logTotSchools, data=sd_df, posterior=FALSE))
summary(lmBF(PctUpToDate ~ logPctFamilyPoverty + logPctChildPoverty + logEnrolled + logTotSchools, data=sd_df, posterior=TRUE, iterations=10000))


which(is.na(districts$logPctFamilyPoverty))

# 5.
plot(districts$logEnrolled, districts$PctBeliefExempt)
summary(lm(PctBeliefExempt ~ logPctChildPoverty, data=districts))$r.squared
summary(lm(PctBeliefExempt ~ logPctFamilyPoverty, data=districts))$r.squared
summary(lm(PctBeliefExempt ~ logEnrolled, data=districts))$r.squared
summary(lm(PctBeliefExempt ~ logTotSchools, data=districts))$r.squared

# 6.
summary(lm(PctUpToDate ~ logPctChildPoverty, data=districts))$r.squared
summary(lm(PctUpToDate ~ logPctFamilyPoverty, data=districts))$r.squared
summary(lm(PctUpToDate ~ logEnrolled, data=districts))$r.squared
summary(lm(PctUpToDate ~ logTotSchools, data=districts))$r.squared

0.1249 - 0.122

4.0118/100

result$r.squared

sd_df <- data.frame(scale(districts[,c(6,8,13:16)], center=TRUE, scale=FALSE))
sd_df <- data.frame(scale(sd_df,center=TRUE,scale=FALSE))
result <- summary(lm(PctBeliefExempt ~ logTotSchools + logEnrolled + logPctChildPoverty + logPctFamilyPoverty, data=districts))
result
max(abs(result$coefficients[2:5]))

result <- summary(lm(PctUpToDate ~ logTotSchools + logEnrolled + logPctChildPoverty + logPctFamilyPoverty, data=sd_df))
result
max(abs(result$coefficients[2:5]))

summary(lm(PctBeliefExempt ~ logTotSchools + logEnrolled + logPctChildPoverty + logPctFamilyPoverty, data=districts))

0.1249 - 0.08062
0.1249 - 0.04388
0.1249 - 0.03663
0.1249 - 0.04389

0.1234 - 0.07672
0.1234 - 0.04154
0.1234 - 0.03607
0.1234 - 0.04369


# 7.
summary(lm(PctUpToDate ~ TotalSchools + PctFamilyPoverty, data=districts))$adj.r.squared
summary(lm(PctUpToDate ~ TotalSchools + PctFamilyPoverty + Enrolled, data=districts))$adj.r.squared
summary(lm(PctUpToDate ~ TotalSchools + PctFamilyPoverty + Enrolled + TotalSchools, data=districts))$adj.r.squared

summary(lm(PctUpToDate ~ TotalSchools + PctFamilyPoverty, data=districts))$adj.r.squared
summary(lm(PctUpToDate ~ TotalSchools + PctFamilyPoverty + Enrolled, data=districts))$adj.r.squared
summary(lm(PctUpToDate ~ TotalSchools + PctFamilyPoverty + Enrolled + TotalSchools, data=districts))$adj.r.squared


r1 <- summary(lm(PctUpToDate ~ logEnrolled + logPctFamilyPoverty + logTotSchools + logPctChildPoverty, data=districts))$r.squared

r1 - summary(lm(PctUpToDate ~ logEnrolled + logPctFamilyPoverty + logTotSchools, data=districts))$r.squared
r1 - summary(lm(PctUpToDate ~ logEnrolled + logPctFamilyPoverty + logPctChildPoverty, data=districts))$r.squaredf
r1 - summary(lm(PctUpToDate ~ logEnrolled + logTotSchools + logPctChildPoverty, data=districts))$r.squared
r1 - summary(lm(PctUpToDate ~ logPctFamilyPoverty + logTotSchools + logPctChildPoverty, data=districts))$r.squared


# 8.
# aovOut1 = aov(PctUpToDate ~ PctChildPoverty * Enrolled, data=districts)
# summary(aovOut1)

# sample <- districts
# sample$PctChildPoverty <- factor(sample$PctChildPoverty)
# sample$Enrolled <- factor(sample$Enrolled)
# levels(sample$PctChildPoverty) = c("Poor", "Medium", "Rich")
# levels(sample$Enrolled) = c("Small", "Medium", "High")
# aovOut2 = anovaBF(PctUpToDate ~ PctChildPoverty * Enrolled, data=sample)
# aovOut2 
# aovOut2[4]/aovOut2[3]

stdDis <- data.frame(scale(districts[,c(6,8,9,10,11,12)],center=TRUE,scale=FALSE))
lmOut1 <- lm(PctUpToDate ~ PctChildPoverty * Enrolled, data=stdDis)
summary(lmOut1)
plot(districts$PctChildPoverty, residuals(lmOut1))
abline(h = 0)

mcmcOut <- lmBF(PctUpToDate ~ PctChildPoverty * Enrolled, data=stdDis, posterior=TRUE, iterations=10000)
summary(mcmcOut)
rsqList <- 1 - (mcmcOut[,'sig2'] / var(districts$PctUpToDate))
mean(rsqList) # Overall mean R-squared 
quantile(rsqList,c(0.025))
quantile(rsqList,c(0.975))

# delta-R-squared
without <- lm(PctUpToDate ~ PctChildPoverty + Enrolled, data=districts)
with <- lm(PctUpToDate ~ PctChildPoverty * Enrolled, data=districts)
library(lmSupport)
modelCompare(without, with)
lmOutBayes1 <- lmBF(PctUpToDate ~ PctChildPoverty + Enrolled, data=districts)
lmOutBayes2 <- lmBF(PctUpToDate ~ PctChildPoverty * Enrolled, data=districts)
lmOutBayes2/lmOutBayes1

# 9.
colnames(districts)
glmOut <- glm(DistrictComplete ~ PctChildPoverty, data=districts, family = binomial(link="logit")) 
summary(glmOut) 
hist(residuals(glmOut)) 
exp(coef(glmOut))
