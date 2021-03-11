######################################################
# IST772, Chapter 9
#
# Coauthor Name: Wanyue Xiao, Jeff Stanton
# Homework Number: Week 10
# Date: 10/20/2020
#
# Sttribution statement:
# 1. I modified this file based on an original file provided by the professor, including
# the resuage of code embedded in the rmd document


# 1.
setwd("/Users/wanyuexiao/Downloads/week 10")
interaction <- read.csv("./Week10interaction.csv")

# 2.
summary(interaction)
str(interaction)
hist(interaction$DV)
hist(interaction$IV1)
hist(interaction$IV2)
hist(interaction$GRP)
corrplot::corrplot(interaction)
pairs(interaction)

# 3.
new_interaction <- interaction
new_interaction[c(2,3)] <- lapply(new_interaction[c(2,3)], function(x) c(scale(x, center = TRUE, scale = FALSE)))
summary(new_interaction)
# The interaction may be highly correlated with the other independent variables. 
# To solve such an issue, one can eliminate a majority of the spurious correlation by centering 
# those independent variables around a mean of 0. 

# 4.
summary(lm(DV ~ IV1 + IV2 , data = new_interaction))
# the p-value of IV2 is smaller than 0.05, indicating that the rersult is significant while that of 
# IV1 is above 0.05.

# 5.
summary(lm(DV ~ IV1 * IV2 , data = new_interaction))
# compared with question 4, the p-values of IV1 and IV2 are all above 0.05 while that of the interaction
# is below 0.05. Therefore, the ineteraction is statistically significant.

# 6.
main = aov(DV ~ IV1 + IV2 , data = new_interaction)
summary(main)
inter = aov(DV ~ IV1 * IV2 , data = new_interaction)
summary(inter)
# all the predictors in those two models are statistically significant.

# 7.
# install.packages("lmSupport")
library(lmSupport)
modelCompare(main, inter)
# the larger the delta-R-squared is, the stronger the interaction effect. since the p-value
# is below 0.05, we can reject the null hypothesis of no difference between the R-squared values 
# of the two models. 

# 8.
new_df <- new_interaction[c(1,2,3)]
new_df <- rbind(new_df, c(min(new_df$IV1), min(new_df$IV2)))
new_df <- rbind(new_df, c(min(new_df$IV1), max(new_df$IV2)))
new_df <- rbind(new_df, c(max(new_df$IV1), min(new_df$IV2)))
new_df <- rbind(new_df, c(max(new_df$IV1), min(new_df$IV2)))

# 9.
lm(new_interaction$DV ~ IV1 * IV2, data= new_df)
predict(lm.sol,new_df, interval="prediction")
predict(lm.sol,new_df, interval="prediction")
predict(lm.sol,new_df, interval="prediction")
predict(lm.sol,new_df, interval="prediction")

# 10.
dataset1 <- new_df[new_df$IV2 > median(new_df$IV2),]
dataset2 <- new_df[new_df$IV2 < median(new_df$IV2),]

# 11.
summary(lm(DV ~ IV1, data = dataset1))
summary(lm(DV ~ IV1, data = dataset2))

# 12.
plot(interaction$DV, interaction$IV1)
abline(lm(DV ~ IV1, data = dataset1),col='black',lty=3)
abline(lm(DV ~ IV1, data = dataset2),col='green',lty=3)

# 14.
lapply(new_interaction[c(1,2,3)], function(x) t.test(x ~ new_interaction$GRP, paired = TRUE, na.action = na.pass))

# 15.
summary(lm(DV ~ IV1 + IV2 + GRP, data=new_interaction))

# 16.
summary(lm(DV ~ IV1 * IV2 + GRP, data=new_interaction))
#  all of those varaibles's p-value are above 0.05.