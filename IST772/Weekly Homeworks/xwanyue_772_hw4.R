######################################################
# IST772, Chapter 4
#
# Student Name: Wanyue Xiao
# Homework Number: Week 2
# Date: 09/15/2020
#
# Sttribution statement:
# 1. I did this homework by myself, with help from the book and the professor.

# 1. 
my.dir <- getwd()
bBatterydata <- read.csv(paste0(my.dir, "/bBatterydata.csv"), header = TRUE)
str(bBatterydata)
summary(bBatterydata)
# All are numeric data. Obs ranges from 1 to 172. Battery has two numbers. Time seems to have lots of numbers.

# 2. 
boxplot(bBatterydata$Time ~ bBatterydata$Battery)
# The range of type 1 is quite large while that of type 2 is rether small.
# Type 2 also has outliers.

# 3.
tapply(X=bBatterydata$Time, INDEX=bBatterydata$Battery,FUN=mean)
tapply(X=bBatterydata$Time, INDEX=bBatterydata$Battery,FUN=sd)
# the mean of type 2 is higher. For sd, the value of type 1 is definitely higher
# than that of type 2.

# 4. 
t.test(bBatterydata$Time[bBatterydata$Battery == 1], 
       bBatterydata$Time[bBatterydata$Battery == 2])
# the upper boundary is -83.49022 while the lower bounadry is -58.41675.

# 5. Confidence interval: is a range of values that's likely to include a population value with 
# a certain degree of confidence. 
# There are 95 of those replication samples that the population difference in weight between 
# type 1 and type 2 could be a negative number somewhere in the region of -70.95349 plus 
# or minus about 12.53673.

# 6.
# The quality of type 1 batteries are generally lower than that of type 2, indicating that there
# must be something wrong happened since the confidence interval is negative.

# 7.
cBattPop <- read.csv(paste0(my.dir, "/cBattPop.csv"), header = TRUE)
str(cBattPop)
summary(cBattPop)

# 8.
popMeanDiff <- tapply(X=cBattPop$Time,INDEX=cBattPop$Battery,FUN=mean)
popMeanDiff <- 1372.947 - 1396.585
popMeanDiff

# 9.
replBattCI <- function() {
  mySamp <- cBattPop[sample(1:100000,size=172, replace=TRUE), ] 
  nicad <- mySamp$Time[mySamp$Battery==1]
  liion <- mySamp$Time[mySamp$Battery==2] 
  return(t.test(nicad,liion)$conf.int)
}

# 10.
confIntList <- t(replicate(100,replBattCI()))
confIntList
# The left is lower boundary, the right column is upper boundary

# 11.
plot.ts(confIntList[,1],col="red", ylim=c(-40,-5))

# 12.
lines(confIntList[,2], col="green")

# 13.
abline(h=popMeanDiff)

# 14.
# For red line, there are 6 times.
# For green line, there are four times.
# The crossings aren't bad since it only has limited number compared 
# to the whole 100 times experiments.

