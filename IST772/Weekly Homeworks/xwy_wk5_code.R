######################################################
# IST772, Chapter 5
#
# Student Name: Wanyue Xiao
# Homework Number: Week 5
# Date: 09/21/2020
#
# Sttribution statement:
# 1. I did this homework by myself, with help from the book and the professor.

library(BEST)
bestOut <- BESTmcmc(mtcars$mpg[mtcars$am==0], mtcars$mpg[mtcars$am==1])
bestOut
plot(bestOut)

# 2.
mean(mtcars$mpg[mtcars$am==0])-mean(mtcars$mpg[mtcars$am==1])

# 3.
# The HDI boundary values are -11.6 and -2.86.

# 4.
# HDI is presented in the context of a probability distribution of estimated population mean differences. 
# The interpretation is that 95% of the likely values of the population mean difference lie in the 
# bell-shaped area between -11.6 and -2.86.

# 5. 
# No. Only 0.1% is above the zero. The implication is that manual transmissions are better than
# automatic transmissions.

# 6.
plotAll(bestOut)
# Group 2 is likely to have greater variability in the population since it has a larger range 
# of the sd values.

# 7.
library(JAGS)

# 7.
# I would like to choose manual transmission since it constantly offer a better functionality 
# compared with automatic transmissions.

# 11.
# the plot(samples[,"mu_diff"]) command could plot a graph which shows the HDI.
# Yes, the HDI merely overlap zero, just a small portion of it. Nearly 0.1% of the
# HDI are above the zero point.

# 12. 
# Even though the results are similar, JAGS model considers the aspect of effect size and also plots the iteration trace plot.






