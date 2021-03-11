library(changepoint)
library(readr)
library(BayesFactor)
library(MCMCpack)

data(usVa)

# probability
flip = rbinom(n=100000, size = 9, prob = 0.5)
table(flip)
barplot(table(flip))
barplot(table(flip)/100000)
barplot(cumsum(table(flip)/100000))

# inference and CI
boxplot(mpg ~ am, data = mtcars)
t.test(mtcars$mpg[mtcars$am==0] ,mtcars$mpg[mtcars$am==1])
library(animation)
conf.int(level=0.95)

# bayesian and traditional testing
library(BEST)
carsBest <- BESTmcmc(mtcars$mpg[mtcars$am==0], mtcars$mpg[mtcars$am==1]) 
plot(carsBest)
carsBest
plotAll(carsBest)

library(effsize)
cohen.d(mtcars$mpg[mtcars$am==0] ,mtcars$mpg[mtcars$am==1])

# ANOVA
rf(n=100,df1=2,df2=57)
hist(rf(n=100,df1=2,df2=57))

# Run ANOVA on groups sampled from the same population 
set.seed(10)
precipAmount <- sample(precip,60,replace=TRUE)
# Group designators, 3 groups
precipGrp <- as.factor(rep(seq(from=1,to=3,by=1),20))
precipDF <- data.frame(precipAmount, precipGrp)
boxplot(precipAmount ~ precipGrp, data=precipDF)
# Run the ANOVA
precipOut <- aov(precipAmount ~ precipGrp, data=precipDF) 
summary(precipOut)

fVals <- seq(from=0.01,to=5,by=0.01)
plot(fVals, df(fVals,df1=2,df2=57))
# Add points to the same plot, different df1
points(fVals, df(fVals,df1=3,df2=57))
# Add points to the same plot, different df1 
points(fVals, df(fVals,df1=4,df2=57))

library(BayesFactor) 
chicksBayesOut <- anovaBF(weight ~ feed, data=chickwts)
# Run mcmc iterations
mcmcOut2 <- posterior(chicksBayesOut,iterations=10000) 
boxplot(as.matrix(mcmcOut2[,2:7])) 
plot(mcmcOut[,'mu']) 
summary(mcmcOut2)
plot(BESTmcmc(chickwts[chickwts$feed=='sunflower',1], chickwts[chickwts$feed=='meatmeal',1]))

# chi-square test - bayesian 
make2x2table <- function(ul){
  ll <- 50 - ul # Calculate the lower-left cell 
  ur <- 30 - ul # Calculate the upper-right cell 
  lr <- 50 - ur # Calculate the lower-right cell 
  matrix(c(ul,ur,ll,lr), nrow=2, ncol=2, byrow=TRUE) 
} 
ctBFout <- contingencyTableBF(make2x2table(20), sampleType='poisson', posterior=FALSE)
ctBFout

ctMCMCout <- contingencyTableBF(make2x2table(20), sampleType='poisson', posterior=TRUE, iterations=10000)
summary(ctMCMCout)
downProp <- ctMCMCout[,'lambda[1,1]'] / ctMCMCout[,'lambda[2,1]']
hist(downProp) 
upProp <- ctMCMCout[,'lambda[1,2]'] / ctMCMCout[,'lambda[2,2]'] 
hist(upProp) 

diffProp <- downProp - upProp
hist(diffProp)
abline(v=quantile(diffProp,c(0.025)), col='black') # Lower bound of 95% HDI 
abline(v=quantile(diffProp,c(0.975)), col='black') # Upper bound of 95% HDI 


# linear regression
data("States")
regOutMCMC <- lmBF(Life.Exp ~ HS.Grad + Income + Illiteracy, data=stateData, posterior=TRUE, iterations=10000) 
summary(regOutMCMC) 
abline(v=quantile(regOutMCMC[,'Illiteracy'],c(0.025)), col='black') 
abline(v=quantile(regOutMCMC[,'Illiteracy'],c(0.975)), col='black') 
	
rsqList <- 1 - (stateOutMCMC[,'sig2'] / var(stateData$Life.Exp)) 
mean(rsqList) # Overall mean R-squared 
quantile(rsqList,c(0.025))
quantile(rsqList,c(0.975)) 


# interaction
interaction.plot()

library(HSAUR)
data('weightgain', package = 'HSAUR')
wg <- weightgain # Copy the dataset into a new object 
interaction.plot(x.factor=wg$source,trace.factor=wg$type,response=wg$weightgain)

aovOut2 = aov(weightgain ~ source * type, data=weightgain)
summary(aovOut2)
aovOut3 = anovaBF(weightgain ~ source*type, data=weightgain)
aovOut3[4]/aovOut3[3] 

mcmcOut <- posterior(aovOut3[4],iterations=10000) 


library(gplots)
plotmeans(weightgain ~ interaction(source,type,sep =''), data = weightgain, connect = list(c(1,2),c(3,4))) 

# time serires
data("ChickWeight")
ch16index <- ChickWeight$Time == 16
ch18index <- ChickWeight$Time == 18
bothChicks <- ChickWeight[ch16index | ch18index,] # Both sets together
# Grab weights for t=16
time16weight <- bothChicks[bothChicks$Time == 16,'weight']
# Grab weights for t=18
time18weight <- bothChicks[bothChicks$Time == 18,'weight'] 
cor(time16weight,time18weight)

mean(time16weight)
mean(time18weight)
# Independent groups t-test
t.test(time18weight,time16weight, paired = FALSE)
summary(BESTmcmc(time18weight,time16weight)) # Run the Bayesian equivalent

t.test(time18weight,time16weight,paired = TRUE)

chwBal <- ChickWeight # Copy the dataset chwBal$TimeFact <- as.factor(chwBal$Time) # Convert Time to a factor
# Make a list of rows
list <- rowSums(table(chwBal$Chick,chwBal$TimeFact))==12
list <- list[list==TRUE] # Keep only those with 12 observations 
list <- as.numeric(names(list)) # Extract the row indices
chwBal <- chwBal[chwBal$Chick %in% list,] # Match against the data

str(EuStockMarkets)
data("EuStockMarkets")
View(EuStockMarkets)

cor(EuStockMarkets)
cor(diff(EuStockMarkets))
mean(diff(EuStockMarkets))

acf(diff(EuStockMarkets))
decOut <- decompose(diff(EuStockMarkets))
plot(decOut$trend)
plot(decOut$seasonal)
plot(decOut$random)

acf(decOut$trend, na.action = na.pass)
acf(decOut$seasonal)
acf(decOut$random, na.action = na.pass)

# 
plot(EuStockMarkets)
plot(diff(EuStockMarkets))
adf.test(diff(EuStockMarkets[,'DAX']))
acf(diff(EuStockMarkets[,'DAX']))
mean(diff(EuStockMarkets))

dec1 <- decompose(diff(EuStockMarkets[,'DAX']))
acf(dec1$random, na.action = na.pass)
mean(dec1$random, na.rm = TRUE)

# change point
DAX <- EuStockMarkets[,'DAX']
DAXcp <- cpt.mean(DAX) 
DAXcp
plot(DAXcp)
plot(DAXcp, cpt.col='grey', cpt.width=5)
plot(cpt.mean(diff(DAX)))

DAXcp <- cpt.mean(DAX,class=FALSE)
DAXcp['conf.value']

library(bcp)
bcpDAX <- bcp(as.vector(DAX)) 
plot(bcpDAX)
plot(bcpDAX$posterior.prob >.95)




#
data("mtcars")
View(mtcars)
summary(mtcars[,c(1,3,4,5)])
sub <- mtcars[,c(1,3,4,5)]
cor.test(sub$mpg, sub$disp)
cor(sub)
boxplot(sub)


table(StudentData$COUNTY)
data <- StudentData[StudentData$year == 2010,]
data <- data[,c(2,5,6,7,8,9)]
data$nMMR <- 1 - data$nMMR / data$n
data$nDTP <- 1 - data$nDTP / data$n
data$nPolio <- 1- data$nPolio / data$n
data$nPBE <- 1- data$nPBE / data$n
data$nPME <- 1- data$nPME / data$n
data <- data[,-2]
data <- as.data.frame(data)

sample <- data[sample(nrow(data), 700), ]
summary(sample[,c(2:5)])

cor(sample[,c(2:5)])
boxplot(sample[,c(2:5)])

mtcars
summary(lm(mpg ~ cyl, data = mtcars))
summary(lm(mpg ~ hp, data = mtcars))
summary(lm(mpg ~ wt, data = mtcars))
summary(lm(mpg ~ cyl + hp + wt, data = mtcars))



