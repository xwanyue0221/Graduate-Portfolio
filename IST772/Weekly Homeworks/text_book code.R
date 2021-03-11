data("ChickWeight")

ch16index <- ChickWeight$Time == 16
ch18index <- ChickWeight$Time == 18
bothChicks <- ChickWeight[ch16index | ch18index,] # Both sets together
# Grab weights for t=16
time16weight <- bothChicks[bothChicks$Time == 16,'weight']
# Grab weights for t=18
time18weight <- bothChicks[bothChicks$Time == 18,'weight'] 
cor(time16weight,time18weight)

# if the dataset is independent
mean(time16weight)
mean(time18weight)
# Independent groups t-test
t.test(time18weight,time16weight,paired = FALSE) 
library(BEST)
BESTmcmc(time18weight,time16weight) # Run the Bayesian equivalent

# if the dataset is dependent
t.test(time18weight,time16weight,paired = TRUE)
weightDiffs <- time18weight - time16weight # Make difference scores
t.test(weightDiffs) # Run a one sample t-test on difference scores
# Run the Bayesian one-sample test on difference scores 
BESTmcmc(weightDiffs)

# balanced data
table(ChickWeight$Chick,ChickWeight$Time)
# managing missing data
chwBal <- ChickWeight # Copy the dataset
chwBal$TimeFact <- as.factor(chwBal$Time) # Convert Time to a factor
# Make a list of rows
list <- rowSums(table(chwBal$Chick,chwBal$TimeFact))==12
list <- list[list==TRUE] # Keep only those with 12 observations 
list <- as.numeric(names(list)) # Extract the row indices
chwBal <- chwBal[chwBal$Chick %in% list,] # Match against the data
summary(aov(weight ~ TimeFact + Error(Chick), data=chwBal))

# exANOVA
library(ez)
install.packages('ez')
ezANOVA(data=chwBal,dv=.(weight),within=.(TimeFact),wid=.(Chick),detailed=TRUE)
data(ANT)
head(ANT)
ezPrecis(ANT)
#Run an ANOVA on the mean correct RT data.
rt_anova = ezANOVA(
  data = ANT[ANT$error==0,]
  , dv = rt
  , wid = subnum
  , within = .(cue,flank))

# time series analysis
set.seed(1234)
tslen <- 180
ex1 <- rnorm(n=tslen,mean=0,sd=10)
tex1 <- ex1 + seq(from=1, to=tslen, by=1) 
plot.ts(tex1) # Plot the time series
# Control random numbers
# About half a year of daily points # Make a random variable
# Add the fake upward trend with a connected line

ex2 <- rnorm(n=tslen,mean=0,sd=10) # Make another random variable 
tex2 <- ex2 + seq(from=1, to=tslen, by=1) # Add the fake upward trend 
cor(ex1, ex2) # Correlation between the two random variables
cor(tex1, tex2) # Correlation between the two time series

ex3 <- rnorm(n=tslen,mean=0,sd=10)
tex3 <- ex3 + seq(from=1, to=tslen, by=1) # Add the fake upward trend 
tex3 <- tex3 + sin(seq(from=0,to=36,length.out=tslen))*20
plot.ts(tex3)
# decompose the plot
decOut <- decompose(ts(tex3,frequency=30)) 
plot(decOut)

set.seed(1234)
tslen <- 180
ex1 <- rnorm(n=tslen,mean=0,sd=10) # Make a random variable 
acf(ex1)


tex1 <- ex1 + seq(from=1, to=tslen, by=1) # Add the fake upward trend
acf(tex1)

ex3 <- rnorm(n=tslen,mean=0,sd=10)
tex3 <- ex3 + seq(from=1, to=tslen, by=1) # Add the fake upward trend 
tex3 <- tex3 + sin(seq(from=0,to=36,length.out=tslen))*20
acf(tex3)
acf(decOut$trend,na.action=na.pass)
acf(decOut$seasonal)

install.packages('tseries')
library(tseries)
decComplete <- decOut$random[complete.cases(decOut$random)] 
adf.test(decComplete) # Shows significant, so it is stationary

plot(diff(EuStockMarkets))
adf.test(diff(EuStockMarkets[,'DAX']))
min(cor(diff(EuStockMarkets)))^2

install.packages('changepoint') 
library(changepoint)
DAX <- EuStockMarkets[,'DAX'] 
DAXcp <- cpt.mean(DAX)
DAXcp
plot(DAXcp,cpt.col='grey',cpt.width=5) 

DAXcp <- cpt.mean(DAX,class=FALSE)
DAXcp['conf.value']

install.packages('bcp') 
library(bcp)
bcpDAX <- bcp(as.vector(DAX)) 
plot(bcpDAX)

# 2.
install.packages('nlme')
library(nlme)
data(Blackmore)

data(AirPassengers)
plot(diff(AirPassengers))
plot(cpt.var(diff(AirPassengers)))

plot(cpt.mean(AirPassengers))

Jan <- AirPassengers[,'Jan']
plot(AirPassengers)
library(bcp)
plot(bcp(AirPassengers))
