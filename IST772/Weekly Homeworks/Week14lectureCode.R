# Week 14 R Code to go with lecture slides


plot(iris$Sepal.Length, iris$Petal.Width) # Basic bivariate plot
abline(lm(iris$Petal.Width ~ iris$Sepal.Length)) # Show a regression line
abline(coef=c(11,-1.6)) # Plot the perpendicular

# First, run a four component model to show the concept of alternative axes
irisOut <- psych::principal(iris[-5], nfactors = 4)
irisOut

# Now a two component model to match what appears in the book
irisOut <- psych::principal(iris[-5], nfactors = 2)
irisOut

plot(irisOut) # Show loadings
plot(irisOut$scores) # Show component scores
 
psych::alpha(iris[-5], check.keys = TRUE) # Review which items fit together and which do not

# Now build and examine a composite
irisNS <- scale(iris[-5]) 					# standardize each variable
flowerSize <- (irisNS[,1]+ irisNS[,3]+ irisNS[,4])/3 	# All except Sepal.Width
length(flowerSize)						# Check the vector
mean(flowerSize)						# Examine the mean
sd(flowerSize)						# And the standard deviation
hist(flowerSize)
barplot(flowerSize ~ iris$Species)


# Phase 2: Dimension reduction using caret

#install.packages("caret")
#install.packages("paran")


library(caret) # The caret package
data(BloodBrain) # BloodBrain data contains two objects: bbbDescr and logBBB

dim(bbbDescr) # 134 columns!
length(logBBB) # Both have 208 cases

# What happens if we try to build a predictor model with all of these predictors?
summary(stats::lm(logBBB ~ ., data=bbbDescr)) # There are five vars causing singularity
summary(stats::lm(logBBB ~ ., data=bbbDescr))$adj.r.squared

# Let's find predictors that are combinations of other predictors
lcList <- caret::findLinearCombos(bbbDescr)
str(lcList) # Show the structure
cor(bbbDescr[,25],bbbDescr[,22]) # Demonstrate a linear combination
lcList # SHow the whole object
lcList$remove # Here's the list of columns to remove
bbbSlim1 <- bbbDescr[,-lcList$remove] # Take them away
dim(bbbSlim1) # 130 columns!
summary(stats::lm(logBBB ~ ., data=bbbSlim1))$adj.r.squared # No change: 
# All we have done is eliminate linear combinations

# Let's look for highly intercorrelated predictors
correlationMatrix <- cor(bbbSlim1)
highlyCorrelated <- caret::findCorrelation(correlationMatrix, cutoff=0.90, names=TRUE)
highlyCorrelated
highlyCorrelated <- caret::findCorrelation(correlationMatrix, cutoff=0.90, names=FALSE)

bbbSlim2 <- bbbSlim1[,-highlyCorrelated] # Removes the highly correlated predictors
dim(bbbSlim2) # 97 columns!
summary(stats::lm(logBBB ~ ., data=bbbSlim2))$adj.r.squared # Some decline?


# Three general approaches to feature selection: Embedded methods: Work during
# the analysis process to penalize model complexity; Filter methods: Score/weight
# each predictor individually in order to choose the most promising; Wrapper
# methods: Search process that sorts through combinations of predictors
# (conceptually similar to stepwise regression)

# Try a filter based method
varImpDF <- caret::filterVarImp(x=bbbSlim2, y=logBBB, nonpara = FALSE)
# For nonpara = FALSE, these are the absolute values of the 
# t-value for each predictor's coefficient
barplot(sort(varImpDF$Overall))
# Any value <2 is not significant, so drop these
which(varImpDF$Overall < 2)
removeCols <- which(varImpDF$Overall < 2)

bbbSlim3 <- bbbSlim2[,-removeCols]
dim(bbbSlim3) # 60 columns left!
summary(stats::lm(logBBB ~ ., data=bbbSlim3))$adj.r.squared # Some decline?

# Now use a wrapper-based method to test combinations
# Control structure sets up the run for 10 cross validations
control <- caret::rfeControl(functions=lmFuncs, method="cv", number=10)
# lmFuncs uses linear method to screen; also try: treeBagFuncs

# run the RFE algorithm, retain the best subset of maxPred or less
# Note: Depending on power of computer, this can take many minutes
maxPred <- 25
set.seed(123)
rfeResults <- caret::rfe(x=bbbSlim3, y=logBBB, sizes=c(1:maxPred), rfeControl=control)
print(rfeResults) # Shows the table of cross validation results
graphics::plot(rfeResults, type=c("g", "o")) # Make a line plot of model sizes and RMSE

caret::predictors(rfeResults) # Lists predictors in order of goodness

str(bbbSlim3[,caret::predictors(rfeResults)]) # Review top variables
cor(bbbSlim3[,caret::predictors(rfeResults)]) # Review top variables

# Now we will just take the best ones
bbbSlim4 <- bbbSlim3[,caret::predictors(rfeResults)]
dim(bbbSlim4) # 19 predictors!
summary(stats::lm(logBBB ~ ., data=bbbSlim4))$adj.r.squared

# Run a parallel analysis to see if we can use PCA to further reduce
library(paran)
paran::paran(bbbSlim4, iterations=330, graph=TRUE) # Looks like 4 components

# Save the result this time, so we can grab the number of components
paranOut <- paran::paran(bbbSlim4, iterations=330, graph=FALSE)

# Generate factor scores for a four component model: paranOut$Retained has the number
# of components we should use
library(psych)
prinOut <- psych::principal(bbbSlim4, 
                            nfactors = paranOut$Retained, 
                            residuals = FALSE,
                            rotate="varimax", 
                            scores=TRUE)
prinOut
bbbSlim5 <- data.frame(prinOut$scores) # Make a data frame of factor scores
summary(lm(logBBB ~ ., data=bbbSlim5))$adj.r.squared # Big decline?

# Make a list of adjusted R squared values for each model
adjRsquaredList <- c(
  summary(stats::lm(logBBB ~ ., data=bbbDescr))$adj.r.squared,
  summary(stats::lm(logBBB ~ ., data=bbbSlim1))$adj.r.squared,
  summary(stats::lm(logBBB ~ ., data=bbbSlim2))$adj.r.squared,
  summary(stats::lm(logBBB ~ ., data=bbbSlim3))$adj.r.squared,
  summary(stats::lm(logBBB ~ ., data=bbbSlim4))$adj.r.squared,
  summary(stats::lm(logBBB ~ ., data=bbbSlim5))$adj.r.squared)
labelList <- c("134","130","97","60","19","PCA") # Labels for the plot

# Show the gradual decline in R-squared values
graphics::barplot(adjRsquaredList, ylab="Adjusted R-Squared",names.arg=labelList)

# That's the end of the slides for this section
#-------------------------------------------------------

#-------------------------------------------------------
#### Breakout Exercise: Now do the same thing yourself with the cox2 data #
library(caret)
data(cox2)
str(cox2Descr) # Here is your data set of predictors
str(cox2IC50) # Here is your vector of DV values
str(cox2Class) # Don't use this for now: This is a binary class variable

# Begin by screening for linear combinations
# Then look for high correlation predictors
# Then use a filter method to screen individual predictors against the DV
# Then use a wrapper method to screen sets of predictors
# If there's anything left, see if a principal components analysis will shrink it!
# Don't forget to compare the adjusted R-squared values

