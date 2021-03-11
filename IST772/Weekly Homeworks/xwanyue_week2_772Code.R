######################################################
# IST772, Chapter 2
#
# Student Name: Wanyue Xiao
# Homework Number: Week 2
# Date: 09/01/2020
#
# Sttribution statement:
# 1. I did this homework by myself, with help from the book and the professor.

##### Exercise 1 ######
lapply(0:7, function(i) choose(i, 0:i))
# If we wish to run a trial which could generate the result appears in the final
# row (the eighth row), we have to throw 7 coin at one experiment. To calculate that
# we can sum up the number in eighth row. The sum is 128, which is the two to the 
# seveth power. 

##### Exercise 2 ######
choose(7, 0:7)/128
# The number of trials would we have observed three heads is 35. 

##### Exercise 3 ######
table(rbinom(n=20,size=7,prob=0.5))
# This list does not contain 0 and 7. Since the result will change every time
# we run the rbinom function, the list will also change. Hence, it is possible 
# that none of the event has all the coin with head above. 

##### Exercise 4 ######
dbinom(x=0:7, size=7, prob=0.5)
# dbinom function returns the value of the probability density function (pdf) of the 
# binomial distribution. In question 3, I only have 6 events with the corresponding
# frequency. However, in this question, I have 8 events, including the event that
# all coins are head above and event that all coins are tail above. The reason is 
# that we specify the random variable x in dbinom function. Hence it will generate 
# all the result. rbinom() function, on the countrary, will generate result 
# randomly.

##### Exercise 5 ######
factories <- matrix(c(0,6,4,6,0,6,6,4,5,4,9,0), ncol=3, byrow=TRUE)
colnames(factories) <- c("1","2","3")
rownames(factories) <- c("vehicle","spill", "equipment", "injury")
factories <- as.table(factories)
addmargins(factories)

##### Exercise 6 ######
accMatrix <- matrix(data=c(0,6,4,6,0,6,6,4,5,4,9,0), nrow=4,byrow=T,
                    dimnames=list(c("Vehicle","Spill","Equipment","Injury"),
                                  c("Factory 1", "Factory 2", "Factory 3")))

##### Exercise 7 ######
proMatrix <- as.table(accMatrix)
proMatrix <- proMatrix/margin.table(proMatrix)
proMatrix

##### Exercise 8 ######
rowSums(accMatrix)
colSums(accMatrix)

##### Exercise 9 ######
proMatrix[4,2]
# If we only look at one single cell in the matrix, it is not difficult to find that
# factory 2 has the highest proportion record, which is 0.18. 
proMatrix[,2]
# this is the raw proportion of each accident of factory 2.

##### Exercise 10 ######
proMatrix[,2]/addmargins(proMatrix)[5,2]
# the probability of vehicle accident in factory 2 is 0.3157895. since we have 
# divide each value by the proportion of factory 2 in the population, we can find
# that the sum of those four accidents' proportion is 1.0 .

##### Exercise 11 ######
addmargins(proMatrix)
# the most prevalent type of accident is equipment accident, whose proportion is 
# 0.30.
addmargins(proMatrix)[3,]/0.3
# factory 1 has the higehst of possibility (which is 40%) that the equipment 
# accident will happend. nearly 27% that this type of accident will happend 
# in factory 2 while that proportion of factory 3 is 0.33.








