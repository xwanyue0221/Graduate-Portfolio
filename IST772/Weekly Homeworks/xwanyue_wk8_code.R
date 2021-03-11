######################################################
# IST772, Chapter 8
#
# Student Name: Wanyue Xiao
# Homework Number: Week 8
# Date: 10/13/2020
#
# Sttribution statement:
# 1. I did this homework by myself, with help from the book and the professor.

# 1.
data("HairEyeColor")
HEcombined <- HairEyeColor[ , ,1] + HairEyeColor[ , ,2]
sum(HEcombined)
# the total number of students is 592.

# 2.
HEcombined/sum(HEcombined)
# the difference between orginial Hecombined table and this table is the scale. Instead of 
# showing the exact number of student, the table shows the proportion of student in the population.
# the totality of columns or rows should be equal to 1.
# no, i cannot.

# 3.
chiOut <- chisq.test(HEcombined)
chiOut
# output:
#  data:  HEcombined
#  X-squared = 138.29, df = 9, p-value < 2.2e-16
# The p-value is less than 0.05, indicating that null hypothesis of independence between
# Hair and Eye will be rejected. The two variablesa are not independent.
# To calculate the number of degrees of freedom of a contigency table, 
# the equation is (4-1)*(4-1)=9.

# 4.
chiOut$residuals

# 5.
HEsmall <- HEcombined[c(1,4), c(1,2)]
HEsmall

# 6.
library(BayesFactor)
ctOut <- contingencyTableBF(HEsmall, sampleType="poisson", posterior=TRUE, iterations=10000)
summary(ctOut)
# it is important that we can get the 95% HDI range of each four combinations. take the 
# brown:black as en example. the mean of lambda[1,1] is similar to that of brown:black. 
# checking the hdi range, we could also get the 95% hdi of brown:black, which is from 52.526 
# to 84.72.

# 7.
firstRowRatio <- ctOut[,"lambda[1,1]"] / ctOut[,"lambda[1,2]"]
hist(firstRowRatio)
mean(firstRowRatio)
secondRowRatio <- ctOut[,"lambda[2,1]"] / ctOut[,"lambda[2,2]"]
hist(secondRowRatio)
HEsmall
# we create a list of 10,000 posterior results, each one calculated as a ratio of the Black:Brown cell 
# count to the Black:Blue cell count for one element in the posterior distribution. then to visualize the
# list, we can get a right-skewed histogram. The mean of firstRowRatio is 3.46, indicating that
# the count for the brown cell was as 3.5 times as higher as the count for the blue cell.
# Similarly, we can get histogram of secondRowRatio. 

# 8.
diff <- firstRowRatio - secondRowRatio
hist(diff)
abline(v=quantile(diff,c(0.025)), col='black') # Low end of 95% HDI 
abline(v=quantile(diff,c(0.975)), col='black') # High end of 95% HDI 
mean(diff) # Here’s the center of the distribution
# the HDI does not overlap with zero. there is credible evidence that in the population there is an association between 
# those two variables.

# 9.
altCorrSample <- read.csv("/Users/wanyuexiao/Documents/SYRACUSE/IST772 Quantitative Reasoning/week 8/altCorrSample.csv",
                      header = TRUE,
                      stringsAsFactors = TRUE)
altCorrSample <- altCorrSample[,-1]
str(altCorrSample)

# 10.
par(mfrow=c(2,2))
hist(altCorrSample$science)
hist(altCorrSample$sciRank)
hist(altCorrSample$tech)
hist(altCorrSample$techRank)
# science variable ranges from 8 to 20 while tech variable ranges from 2 to 8.
# the majority of science points locate between 12 and 16 while the majority of tech points
# locate batween 5 and 8. The axis and y axis of scirank and techrank are at the same scale.

# 11.
cor(altCorrSample$sciRank, altCorrSample$techRank, method = "spearman")
cor.test(altCorrSample$sciRank, altCorrSample$techRank, method = "spearman")

# 12.
cor(altCorrSample$sciRank, altCorrSample$techRank, method = "kendall")
cor.test(altCorrSample$sciRank, altCorrSample$techRank, method = "kendall")










